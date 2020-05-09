#-------------------------------------------------------------------------------
#
# Final project for BIOS 7719 Information Visualization 
#
# v 1.0 
# Tim Vigers 
# May 9, 2020
#
#-------------------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(dplyr)
library(lubridate)
library(plotly)
library(DT)

# Define UI for data upload app ----
ui <- fluidPage(
    theme=shinytheme("superhero"),
    
    # App title ----
    titlePanel("Uploading Files"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            # Input: Select a file ----
            fileInput("csvs", "Choose Files",
                      multiple = TRUE),
            # Horizontal line ----
            tags$hr(),
            # Horizontal line 
            tags$hr(),
            # Download buttons
            downloadButton("downloadData", "Download Summary Measures")
        ),
        # Main panel for displaying outputs
        mainPanel(
            # Output: Tabset w/ plots and summary
            tabsetPanel(type = "tabs",
                        tabPanel("AGP", plotOutput("agp")),
                        tabPanel("Radial AGP",  plotOutput("radagp")),
                        tabPanel("Summary Measures", DT::dataTableOutput("summary"))
            )
        )
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    table <- reactive({
        l <- lapply(input$csvs$datapath, function(x){
            id = paste(sample(letters,6),collapse = "")
            df = read.csv(x,stringsAsFactors = F,na.strings = "")
            df = df[,c(2,8)]
            colnames(df) = c("timestamp","sensorglucose")
            df$timestamp = sub("T"," ",df$timestamp)
            df$id = id
            df = df[,c("id","timestamp","sensorglucose")]
            df = df[complete.cases(df),]
            df
        })
        table <- do.call(rbind,l)
        table$sensorglucose <- suppressWarnings(as.numeric(table$sensorglucose))
        table$timestamp <- lubridate::ymd_hms(table$timestamp)
        table <- table[complete.cases(table),]
        table$id <- as.factor(table$id)
        table$agp <- lubridate::round_date(table$timestamp,unit = "5 minutes")
        table$agp <- as.POSIXct(format(table$agp,format = "%H:%M"),
                                format = "%H:%M",tz = "UTC")
        return(as.data.frame(table))
    })

    p <- reactive({
        df <- as.data.frame(table())
        summ <- df %>% dplyr::arrange(id,agp) %>% dplyr::group_by(id,agp) %>%
            summarise(sg = mean(sensorglucose,na.rm = T)) %>%
            mutate(label = format(agp,format = "%H:%M")) %>% ungroup()
        summ <- summ %>% dplyr::group_by(id) %>%
            mutate(id_spline = as.numeric(predict(smooth.spline(sg))$y)) %>%
            ungroup()

        summ$all_spline = loess(summ$sg~as.numeric(summ$agp))$fitted

        ggplot(summ,aes(x = agp,y = id_spline,group = id)) + geom_line()
        # summ %>% plotly::group_by(id) %>%
        #     plot_ly(x = ~agp, y = ~id_spline,
        #             text=~paste0("ID: ",id,"\n","Time: ",label,"\n","Mean SG: ",round(sg))) %>%
        #     add_lines(hoverinfo = 'text',
        #               line = list(color = 'rgb(31, 119, 180)'),opacity = 0.5) %>% plotly::ungroup() %>%
        #     add_lines(y=smooth$fitted,text=~paste0("Time: ",label,"\n","Mean SG: ",round(sg)),
        #               hoverinfo = 'text',line=list(color = 'rgb(255, 127, 14)')) %>%
        #     layout(
        #         xaxis = list(
        #             type = 'date',
        #             tickformat = "%H:%M",
        #             title = "Time of Day"),
        #         yaxis = list(
        #             title = "Mean Sensor Glusose (mg/dL)",
        #             range = c(0,400)))
    })
    
    # p <- reactive({
    #     df <- as.data.frame(table())
    #     summ <- df %>% dplyr::arrange(id,agp) %>% dplyr::group_by(id,agp) %>%
    #         summarise(sg = mean(sensorglucose,na.rm = T)) %>%
    #         mutate(label = format(agp,format = "%H:%M")) %>% ungroup()
    #     ggplot(summ,aes(x = agp, y = sg, group = id)) + geom_line()
    # })
    
    output$agp = renderPlot({p()})
    output$summary = DT::renderDataTable({as.data.frame(table())})
}

# Create Shiny app ----
shinyApp(ui, server)