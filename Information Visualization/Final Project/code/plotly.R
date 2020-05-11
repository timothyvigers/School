library(dplyr)
library(plotly)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
set.seed(1017)

app <- Dash$new()
# Read in files
files = list.files("/home/tim/Desktop/CGMs",full.names = T)
# Combine into one df
l = lapply(files, function(x){
  id = paste(sample(letters,6),collapse = "")
  df = read.csv(x,stringsAsFactors = F,na.strings = "")
  df = df[,c(2,8)]
  colnames(df) = c("timestamp","sensorglucose")
  df$timestamp = sub("T"," ",df$timestamp)
  df$id = id
  df = df[,c("id","timestamp","sensorglucose")]
  df$gender = sample(c("M","F","Non-binary"),1)
  df$ethnicity = sample(c("NHW","Hispanic","Other"),1)
  df = df[complete.cases(df),]
  df
})
df = do.call(rbind,l)
# Format columns
df$sensorglucose = suppressWarnings(as.numeric(df$sensorglucose))
df$timestamp = lubridate::ymd_hms(df$timestamp,tz = "UTC")
df = df[complete.cases(df),]
df$id = as.factor(df$id)
df$agp = lubridate::round_date(df$timestamp,unit = "5 minutes")
df$agp = as.POSIXct(strftime(df$agp,format = "%H:%M"),format = "%H:%M",tz = 'UTC')
# Sumarize for plots
summ = df %>% arrange(id,agp) %>% dplyr::group_by(id,agp) %>%
  summarise(sg = mean(sensorglucose,na.rm = T),
            gender = gender[1],ethnicity = ethnicity[1]) %>%
  mutate(label = format(agp,format = "%H:%M")) %>% ungroup()
# Smooth with splines
summ = summ %>% group_by(id) %>%
  mutate(id_spline = as.numeric(predict(smooth.spline(sg))$y)) %>% ungroup()
# Table names
t1 <- c("id","Mean","SD","CV","Percent Time Low","Percent Time in Range","Percent Time High")
# Participant means
means = df %>% group_by(id) %>% summarise(mean = mean(sensorglucose))
# Colors
lowcol = '#1f77b4'
ircol = '#2ca02c'
highcol = '#d62728'
# Radio button options
gender_options = lapply(c(unique(df$gender)[order(unique(df$gender))]), 
                        function(x){
                          list(label = x,
                               value = x)
                        })

ethnicity_options = lapply(c(unique(df$ethnicity)[order(unique(df$ethnicity))]), function(x){
  list(label = x,
       value = x)
})

page_size <- 10

# Dash layout
app$layout(
  htmlDiv(
    list(
      # Title
      htmlH1('cgmViz: A web application for visualizing continuous glucose monitor data.',
             style = list(
               textAlign = 'center'
             )),
      htmlH5("Demographic filters"),
      htmlH6("Ethnicity"),
      dccChecklist(
        id = 'ethnicity',
        options = ethnicity_options,value = sapply(ethnicity_options, "[[", 2)
      ),
      htmlH6("Gender"),
      dccChecklist(
        id = 'gender',
        options = gender_options,value = sapply(gender_options, "[[", 2)
      ),
      htmlH5("Data filters"),
      dccChecklist(
        id = 'mean-filter',
        options = list(list(label = "Show only participants with mean in range",
                            value = "In range"))
      ),
      htmlH5('Time in range selector'),
      dccRangeSlider(
        id = 'tir-slider',
        min = 40,
        max = 400,
        step = 5,
        tooltip = list(placement = "bottom"),
        value = c(70,180)
      ),
      dccChecklist(id = 'show-agp',
                   options = list(list(label = "Show range on AGP",value = "yes")),
                   value = "yes"),
      dccChecklist(id = 'rad-agp',
                   options = list(list(label = "Radial AGP",value = "yes"))),
      # Tabs
      # Regular AGP
      dccGraph(id = 'agp'),
      # TIR
      dccGraph(id = 'tir'),
      # Summary boxplots
      dccGraph(id = 'toprow'),
      dccGraph(id = 'bottomrow'),
      htmlH5("Summary table"),
      htmlH6("Cells with > 30% time high are colored red, > 10% low are colored blue, and > 80% in range are colored green."),
      # Table
      dashDataTable(
        id = 'table',
        columns = lapply(t1, 
                         function(colName){
                           list(
                             id = colName,
                             name = colName,
                             deletable = TRUE
                           )
                         }),
        style_data_conditional = list(
          list(
            'if' = list(column_id = 'Percent Time High', 
                        filter_query = '{Percent Time High} > 30'),
            backgroundColor = highcol,
            color = 'white'
          ),
          list(
            'if' = list(column_id = 'Percent Time Low', 
                        filter_query = '{Percent Time Low} > 10'),
            backgroundColor = lowcol,
            color = 'white'
          ),
          list(
            'if' = list(column_id = 'Percent Time in Range', 
                        filter_query = '{Percent Time in Range} > 80'),
            backgroundColor = ircol,
            color = 'white'
          )
        ),
        page_current = 0,
        page_size = page_size,
        page_action = 'custom',
        sort_action = 'custom',
        sort_mode = 'single',
        sort_by = list()
      )
    ))
)

app$callback(
  output = list(output(id='agp', property='figure'),
                output(id='tir', property='figure')),
  params = list(input(id='ethnicity', property='value'),
                input(id='gender', property='value'),
                input(id='tir-slider', property='value'),
                input(id = 'show-agp',property = 'value'),
                input(id = 'rad-agp',property = 'value'),
                input(id = 'mean-filter',property = 'value')),
  function(ethnicity,gender,values,show,radagp,meanfilter) {
    # Filter
    # Data
    if ("In range" %in% meanfilter){
      keep = as.character(means$id[which(means$mean >= values[1] & means$mean <= values[2])])
      summ_plot = summ[summ$ethnicity %in% ethnicity & summ$gender %in% gender &
                         summ$id %in% keep,]
      df_plot = df[df$ethnicity %in% ethnicity & df$gender %in% gender &
                     df$id %in% keep,]
    } else {
      summ_plot = summ[summ$ethnicity %in% ethnicity & summ$gender %in% gender,]
      df_plot = df[df$ethnicity %in% ethnicity & df$gender %in% gender,]
    }
    # Demographics
    
    # Mean for cohort
    overall_plot = summ_plot %>% group_by(agp) %>%
      summarise(sg = mean(sg),ethnicity = ethnicity[1],gender = gender[1],
                label = label[1]) %>% 
      mutate(all_spline = as.numeric(predict(smooth.spline(sg))$y)) %>% ungroup()
    # Radial plot
    summ_plot$t = as.numeric((summ_plot$agp - min(summ_plot$agp)))
    summ_plot$t = summ_plot$t/(max(summ_plot$t)/360)
    overall_plot$t = as.numeric((overall_plot$agp - min(overall_plot$agp)))
    overall_plot$t = overall_plot$t/(max(overall_plot$t)/360)
    # IDs for traces
    ids = unique(summ_plot$id)
    # Regular AGP
    agp <- plot_ly(summ_plot,type = 'scatter',mode = 'lines')
    # Individual traces
    for (id in ids) {
      temp = summ_plot[summ_plot$id == id,]
      agp <- add_trace(agp,data = temp,x = ~agp,y = ~id_spline,opacity = 0.5,
                       line = list(color = '#1f77b4'),name = id,
                       text = ~paste0("ID: ",id,"<br>","Time: ",label,"<br>","Mean SG: ",round(sg)),
                       hoverinfo = 'text')
    }
    # Cohort average
    agp <- add_trace(agp,data = overall_plot,x = ~agp,y = ~all_spline,
                     line = list(color = '#ff7f0e'),name = "Mean",
                     text = ~paste0("Time: ",overall_plot$label,"<br>","Mean SG: ",round(overall_plot$sg)),
                     hoverinfo = 'text')
    # Layout
    agp <- layout(agp,xaxis = list(
      type = 'date',
      tickformat = "%H:%M",
      title = "Time of Day",
      fixedrange = T),
      yaxis = list(
        title = "Mean Sensor Glusose (mg/dL)",
        range = c(0,400))
    )
    # Radial
    rad <- plot_ly(summ_plot,type = 'scatterpolar',mode = 'lines')
    # Individual traces
    for (id in ids) {
      temp = summ_plot[summ_plot$id == id,]
      rad <- add_trace(rad,data = temp,theta = ~t,r = ~id_spline,opacity = 0.5,
                       line = list(color = '#1f77b4'),name = id,
                       text = ~paste0("ID: ",id,"<br>","Time: ",label,"<br>","Mean SG: ",round(sg)),
                       hoverinfo = 'text')
    }
    # Cohort average
    rad <- add_trace(rad,data = overall_plot,theta = ~t,r = ~all_spline,
                     line = list(color = '#ff7f0e'),name = "Mean",
                     text = ~paste0("Time: ",overall_plot$label,"<br>","Overall Mean SG: ",round(overall_plot$sg)),
                     hoverinfo = 'text')
    # Layout
    rad <- layout(rad,
                  showlegend = T,
                  polar = list(
                    angularaxis = list(
                      rotation = 90,
                      direction = 'clockwise',
                      tickvals = c(90,180,270),
                      ticktext = c("06:00","12:00","18:00")
                    ),
                    radialaxis = list(
                      visible = TRUE,
                      ticks = "outside",
                      angle = 90,
                      tickangle = 90,
                      range = c(0,400))
                  )
    )
    if('yes' %in% show) {
      agp <- layout(agp,shapes = list(
        list(type = "rect",
             fillcolor = "green", line = list(color = "green"), opacity = 0.2,
             x0 = (as.numeric(min(df$agp))+21600) * 1000, 
             x1 = (as.numeric(max(df$agp))+21600) * 1000, xref = "x",
             y0 = as.numeric(values[1]), y1 = as.numeric(values[2]), yref = "y"))
      )
    }
    # Calculate TIR
    tir <- df_plot %>% group_by(id) %>% 
      summarise(perc_time_low = length(which(sensorglucose < values[1]))/n() * 100,
                perc_time_ir = length(which(sensorglucose >= values[1] & 
                                              sensorglucose < values[2]))/n() * 100,
                perc_time_high = length(which(sensorglucose >= values[2]))/n() * 100) %>%
      arrange(desc(perc_time_ir))
    
    tir <- plot_ly(tir, x = ~id, y = ~perc_time_low, type = 'bar',
                   name = paste0("< ",values[1]),hoverinfo = 'text',
                   text = ~paste(round(perc_time_low,2),"%"),
                   marker = list(color = lowcol,opacity = 0.5))
    tir <- tir %>% add_trace(y = ~perc_time_ir,hoverinfo = 'text',
                             name = paste0(values[1]," - ",values[2]),
                             text = ~paste(round(perc_time_ir,2),"%"),
                             marker = list(color = ircol,opacity = 0.5))
    tir <- tir %>% add_trace(y = ~perc_time_high,hoverinfo = 'text',
                             name = paste0("> ",values[2]),
                             text = ~paste(round(perc_time_high,2),"%"),
                             marker = list(color = highcol,opacity = 0.5))
    tir <- tir %>% layout(barmode = 'stack',yaxis = list(title = "Percent Time"),
                          xaxis = list(title = "ID",categoryorder = "array",
                                       categoryarray = ~id))
    
    if('yes' %in% radagp) {
      return(list(rad,tir))
    } else {
      return(list(agp,tir))
    }
  })

app$callback(
  output = list(output(id='toprow', property='figure'),
                output(id='bottomrow', property='figure'),
                output(id='table', property='data')),
  params = list(input(id='ethnicity', property='value'),
                input(id='gender', property='value'),
                input(id='tir-slider', property='value'),
                input(id = 'table', property = 'page_current'),
                input(id = 'table', property = 'page_size'),
                input(id = 'table', property = 'sort_by')),
  function(ethnicity,gender,values,page_current, page_size, sort_by) {
    # Filter
    df_plot = df[df$ethnicity %in% ethnicity & df$gender %in% gender,]
    # Summary table
    t <- df_plot %>% group_by(id) %>% 
      summarise(Mean = mean(sensorglucose),
                SD = sd(sensorglucose),
                CV = SD/Mean,
                `Percent Time Low` = length(which(sensorglucose < values[1]))/n() * 100,
                `Percent Time in Range` = length(which(sensorglucose >= values[1] & 
                                                         sensorglucose < values[2]))/n() * 100,
                `Percent Time High` = length(which(sensorglucose >= values[2]))/n() * 100)
    # Boxplots
    meanbox <- plot_ly(t,type = 'box',showlegend = F) %>% 
      add_trace(y=~Mean,line = list(color = 'black'),opacity = 0.5,name = "Mean SG")
    sdbox <- plot_ly(t,type = 'box',showlegend = F) %>% 
      add_trace(y=~SD,line = list(color = 'black'),opacity = 0.5,name = "SD")
    cvbox <- plot_ly(t,type = 'box',showlegend = F) %>% 
      add_trace(y=~CV,line = list(color = 'black'),opacity = 0.5,name = "CV")
    lowbox <- plot_ly(t,type = 'box',showlegend = F) %>% 
      add_trace(y=~`Percent Time Low`,line = list(color = 'black'),
                name = "% Time Low",fillcolor = lowcol,opacity = 0.5) %>%
      layout(yaxis = list(range = c(0,100)))
    irbox <- plot_ly(t,type = 'box',showlegend = F) %>% 
      add_trace(y=~`Percent Time in Range`,line = list(color = 'black'),
                name = "% Time in Range",fillcolor = ircol,opacity = 0.5) %>%
      layout(yaxis = list(range = c(0,100)))
    highbox <- plot_ly(t,type = 'box',showlegend = F) %>% 
      add_trace(y=~`Percent Time High`,line = list(color = 'black'),
                name = "% Time High",fillcolor = highcol,opacity = 0.5) %>%
      layout(yaxis = list(range = c(0,100)))
    toprow <- subplot(meanbox,sdbox,cvbox)
    bottomrow <- subplot(lowbox,irbox,highbox,shareY = T,titleY = F)
    
    # Table
    t[,2:ncol(t)] <- lapply(t[,2:ncol(t)], function(x){round(x,2)})
    t$id <- as.character(t$id)
    
    subdf <- if(length(sort_by) != 0) {
      column_id <- sort_by[[1]][['column_id']]
      decreasing <- sort_by[[1]][['direction']] == "desc"
      # sort by column
      t[order(t[, column_id], decreasing = decreasing), ]
    } else {t}
    start_id <- (page_current * page_size + 1)
    end_id <- ((page_current + 1) * page_size)
    table <- subdf[start_id:end_id, ]
    return(list(toprow,bottomrow,table))
  })

app$run_server(showcase = T)
