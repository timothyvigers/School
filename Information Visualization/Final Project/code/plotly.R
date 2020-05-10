library(dplyr)
library(plotly)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
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
        options = ethnicity_options
      ),
      htmlH6("Gender"),
      dccChecklist(
        id = 'gender',
        options = gender_options
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
      # Tabs
      dccTabs(id="tabs",children=list(
        # Regular AGP
        dccTab(label='AGP', value='agp', children = list(
          dccGraph(id = 'agp')
        )),
        # Radial AGP
        dccTab(label='Radial AGP', value='radagp', children = list(
          dccGraph(id = 'radagp')
        )),
        # TIR
        dccTab(label='Time in Range', value='tir',children = list(
          dccGraph(id = 'tir')
        )),
        # Table
        dccTab(label='Summary Measures', value='table',children = list(
          dccGraph(id = 'table')
        ))
      ))
    )
  )
)

app$callback(
  output = list(id='agp', property='figure'),
  params = list(input(id='ethnicity', property='value'),
                input(id='gender', property='value'),
                input(id='tir-slider', property='value'),
                input(id = 'show-agp',property = 'value')),
  function(ethnicity,gender,values,show) {
    summ_plot = summ[summ$ethnicity %in% ethnicity & summ$gender %in% gender,]
    # IDs for traces
    ids = unique(summ_plot$id)
    # Regular AGP
    fig <- plot_ly(summ_plot,type = 'scatter',mode = 'lines')
    # Individual traces
    for (id in ids) {
      temp = summ_plot[summ_plot$id == id,]
      fig <- add_trace(fig,data = temp,x = ~agp,y = ~id_spline,opacity = 0.5,
                       line = list(color = '#1f77b4'),name = id,
                       text = ~paste0("ID: ",id,"<br>","Time: ",label,"<br>","Mean SG: ",round(sg)),
                       hoverinfo = 'text')
    }
    # Mean for cohort
    overall_plot = summ_plot %>% group_by(agp) %>%
      summarise(sg = mean(sg),ethnicity = ethnicity[1],gender = gender[1],
                label = label[1]) %>% 
      mutate(all_spline = as.numeric(predict(smooth.spline(sg))$y)) %>% ungroup()
    # Cohort average
    fig <- add_trace(fig,data = overall_plot,x = ~agp,y = ~all_spline,
                     line = list(color = '#ff7f0e'),name = "Mean",
                     text = ~paste0("Time: ",overall_plot$label,"<br>","Mean SG: ",round(overall_plot$sg)),
                     hoverinfo = 'text')
    # Layout
    fig <- layout(fig,xaxis = list(
      type = 'date',
      tickformat = "%H:%M",
      title = "Time of Day",
      fixedrange = T),
      yaxis = list(
        title = "Mean Sensor Glusose (mg/dL)",
        range = c(0,400))
    )
    if('yes' %in% show) {
      fig <- layout(fig,shapes = list(
        list(type = "rect",
             fillcolor = "green", line = list(color = "green"), opacity = 0.2,
             x0 = (as.numeric(min(df$agp))+21600) * 1000, 
             x1 = (as.numeric(max(df$agp))+21600) * 1000, xref = "x",
             y0 = as.numeric(values[1]), y1 = as.numeric(values[2]), yref = "y")))
      return(fig)
    } else {
      return(fig)
    }
  })

app$callback(
  output = list(id='radagp', property='figure'),
  params = list(input(id='ethnicity', property='value'),
                input(id='gender', property='value')),
  function(ethnicity,gender) {
    summ_plot = summ[summ$ethnicity %in% ethnicity & summ$gender %in% gender,]
    # IDs for traces
    ids = unique(summ_plot$id)
    # Radial AGP
    summ_plot$t = as.numeric((summ_plot$agp - min(summ_plot$agp)))
    summ_plot$t = summ_plot$t/(max(summ_plot$t)/360)
    fig <- plot_ly(summ_plot,type = 'scatterpolar',mode = 'lines')
    # Individual traces
    for (id in ids) {
      temp = summ_plot[summ_plot$id == id,]
      fig <- add_trace(fig,data = temp,theta = ~t,r = ~id_spline,opacity = 0.5,
                       line = list(color = '#1f77b4'),name = id,
                       text = ~paste0("ID: ",id,"<br>","Time: ",label,"<br>","Mean SG: ",round(sg)),
                       hoverinfo = 'text')
    }
    # Mean for cohort
    overall_plot = summ_plot %>% group_by(agp) %>%
      summarise(sg = mean(sg),ethnicity = ethnicity[1],gender = gender[1],
                label = label[1]) %>% 
      mutate(all_spline = as.numeric(predict(smooth.spline(sg))$y)) %>% ungroup()
    overall_plot$t = as.numeric((overall_plot$agp - min(overall_plot$agp)))
    overall_plot$t = overall_plot$t/(max(overall_plot$t)/360)
    # Cohort average
    fig <- add_trace(fig,data = overall_plot,theta = ~t,r = ~all_spline,
                     line = list(color = '#ff7f0e'),name = "Mean",
                     text = ~paste0("Time: ",overall_plot$label,"<br>","Overall Mean SG: ",round(overall_plot$sg)),
                     hoverinfo = 'text')
    # Layout
    fig <- layout(fig,
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
    return(fig)
  })

app$callback(
  output = list(id='tir', property='figure'),
  params = list(input(id='tir-slider', property='value'),
                input(id='ethnicity', property='value'),
                input(id='gender', property='value')),
  function(values,ethnicity,gender) {
    df_plot = df[df$ethnicity %in% ethnicity & df$gender %in% gender,]
    # Calculate TIR
    tir <- df_plot %>% group_by(id) %>% 
      summarise(perc_time_low = length(which(sensorglucose < values[1]))/n() * 100,
                perc_time_ir = length(which(sensorglucose >= values[1] & 
                                              sensorglucose < values[2]))/n() * 100,
                perc_time_high = length(which(sensorglucose >= values[2]))/n() * 100) %>%
      arrange(desc(perc_time_ir))
    
    fig <- plot_ly(tir, x = ~id, y = ~perc_time_low, type = 'bar',
                   name = paste0("< ",values[1]),hoverinfo = 'text',
                   text = ~paste(round(perc_time_low,2),"%"),
                   color = '#1f77b4')
    fig <- fig %>% add_trace(y = ~perc_time_ir,hoverinfo = 'text',
                             name = paste0(values[1]," - ",values[2]),
                             text = ~paste(round(perc_time_ir,2),"%"),
                             color = '#2ca02c')
    fig <- fig %>% add_trace(y = ~perc_time_high,hoverinfo = 'text',
                             name = paste0("> ",values[2]),
                             text = ~paste(round(perc_time_high,2),"%"),
                             color = '#d62728')
    fig <- fig %>% layout(barmode = 'stack',yaxis = list(title = "Percent Time"),
                          xaxis = list(title = "ID",categoryorder = "array",
                                       categoryarray = ~id))
    return(fig)
  })

app$callback(
  output = list(id='table', property='figure'),
  params = list(input(id='tir-slider', property='value'),
                input(id='ethnicity', property='value'),
                input(id='gender', property='value')),
  function(values,ethnicity,gender) {
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
    
    t[,2:ncol(t)] <- lapply(t[,2:ncol(t)], function(x){round(x,2)})
    
    table <- plot_ly(t,
                     type = 'table',
                     header = list(
                       values = names(t),
                       align = rep('center', ncol(t)),
                       line = list(width = 1, color = 'black'),
                       fill = list(color = '#1f77b4'),
                       font = list(color = c("black"))
                     ),
                     cells = list(
                       values = t(as.matrix(unname(t))),
                       align = c('left', rep('center', ncol(t))),
                       line = list(color = "black", width = 1),
                       font = list(color = c("black"))
                     ))
    return(table)
  })

app$run_server(showcase = T)
