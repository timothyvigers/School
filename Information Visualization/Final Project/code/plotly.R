library(dplyr)
library(plotly)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

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
  df = df[complete.cases(df),]
  df
})
df = do.call(rbind,l)
# Format columns
df$sensorglucose = suppressWarnings(as.numeric(df$sensorglucose))
df$timestamp = lubridate::ymd_hms(df$timestamp)
df = df[complete.cases(df),]
df$id = as.factor(df$id)
df$agp = lubridate::round_date(df$timestamp,unit = "5 minutes")
df$agp = as.POSIXct(strftime(df$agp,format = "%H:%M"),format = "%H:%M",tz = 'UTC')
# Sumarize
summ = df %>% arrange(id,agp) %>% dplyr::group_by(id,agp) %>%
  summarise(sg = mean(sensorglucose,na.rm = T)) %>%
  mutate(label = format(agp,format = "%H:%M")) %>% ungroup()
# Smooth with splines
summ = summ %>% group_by(id) %>%
  mutate(id_spline = as.numeric(predict(smooth.spline(sg))$y)) %>% ungroup()
# Mean for overall cohort
overall = summ %>% group_by(agp) %>%
  summarise(sg = mean(sg)) %>% 
  mutate(all_spline = as.numeric(predict(smooth.spline(sg))$y)) %>% ungroup()
# Convert for radial plot
summ$t = as.numeric((summ$agp - min(summ$agp)))
summ$t = summ$t/(max(summ$t)/360)
overall$t = as.numeric((overall$agp - min(overall$agp)))
overall$t = overall$t/(max(overall$t)/360)
# IDs for traces
ids = unique(df$id)
# Regular AGP
agptraces <- lapply(ids,function(id) {
  name = as.character(id)
  which_id <- which(summ$id == id)
  df_sub <- summ[which_id, ]
  with(
    df_sub,
    list(
      x = agp,
      y = id_spline,
      name = name,
      text = paste0("ID: ",id,"<br>","Time: ",label,"<br>","Mean SG: ",round(sg)),
      hoverinfo = 'text',
      mode = 'lines',
      opacity=0.3,
      line = list(color = '#1f77b4')
    )
  )
})
agptraces[[(length(agptraces) + 1)]] <- list(
  x = overall$agp,
  y = overall$all_spline,
  name = "Cohort Mean",
  text = paste0("Time: ",summ$label,"<br>","Mean SG: ",round(summ$sg)),
  hoverinfo = 'text',
  mode = 'lines',
  line = list(color = '#ff7f0e')
)
# Radial AGP
radagptraces <- lapply(ids,function(id) {
  name = as.character(id)
  which_id <- which(summ$id == id)
  df_sub <- summ[which_id, ]
  with(
    df_sub,
    list(
      theta = t,
      r = id_spline,
      name = name,
      type = 'scatterpolar',
      text = paste0("ID: ",id,"<br>","Time: ",label,"<br>","Mean SG: ",round(sg)),
      hoverinfo = 'text',
      mode = 'lines',
      opacity=0.3,
      line = list(color = '#1f77b4')
    )
  )
})
radagptraces[[(length(radagptraces) + 1)]] <- list(
  theta = overall$t,
  r = overall$all_spline,
  name = "Cohort Mean",
  type = 'scatterpolar',
  text = paste0("Time: ",summ$label,"<br>","Mean SG: ",round(summ$sg)),
  hoverinfo = 'text',
  mode = 'lines',
  line = list(color = '#ff7f0e')
)
# TIR
tir <- df %>% group_by(id) %>%
  summarise()

# Dash layout
app$layout(
  htmlDiv(
    list(
      # Title
      htmlH1('cgmViz: A web application for visualizing continuous glucose monitor data.'),
      # Tabs
      dccTabs(id="tabs",children=list(
        # Regular AGP
        dccTab(label='AGP', value='agp', children = list(
          dccGraph(id = 'agp',
                   figure = list(data = agptraces,
                                 layout = list(
                                   xaxis = list(
                                     type = 'date',
                                     tickformat = "%H:%M",
                                     title = "Time of Day",
                                     fixedrange = T),
                                   yaxis = list(
                                     title = "Mean Sensor Glusose (mg/dL)",
                                     range = c(0,400)),
                                   clickmode = 'event+select',
                                   shapes = list(type = "rect",
                                                 fillcolor = "green", line = list(color = "green"), opacity = 0.3,
                                                 x0 = as.numeric(min(summ$agp))* 24 * 60 * 60 * 1000,
                                                 x1 = as.numeric(max(summ$agp))* 24 * 60 * 60 * 1000, xref = "x",
                                                 y0 = 70, y1 = 180, yref = "y")
                                 )
                                 
                   )
          )
        )),
        dccTab(label='Radial AGP', value='radial-agp',children = list(
          dccGraph(id = 'radial-agp',
                   figure = list(data = radagptraces,
                                 layout = list(
                                   showlegend = TRUE,
                                   polar = list(
                                     radialaxis = list(
                                       visible = TRUE,
                                       ticks = "outside",
                                       angle = 90,
                                       tickangle = 90,
                                       range = c(0,400)),
                                     angularaxis = list(
                                       rotation = 90,
                                       direction = 'clockwise',
                                       tickvals = c(90,180,270),
                                       ticktext = c("06:00","12:00","18:00")
                                     )
                                   )
                                 )
                                 
                   )
          )
        )),
        dccTab(label='Time in Range', value='tir',children = list(
          dccGraph(id = 'tir'),
          dccRangeSlider(
            id = 'tir-slider',
            min = 40,
            max = 400,
            step = 5,
            tooltip = list(placement = "bottom"),
            value = c(70,180)
          )
        ))
      ))
    )
  )
)

app$callback(
  output = list(id='tir', property='figure'),
  params = list(input(id='tir-slider', property='value')),
  function(value) {
    tir <- df %>% group_by(id) %>% 
      summarise(perc_time_low = length(which(sensorglucose < value[1]))/n() * 100,
                perc_time_ir = length(which(sensorglucose >= value[1] & 
                                              sensorglucose < value[2]))/n() * 100,
                perc_time_high = length(which(sensorglucose >= value[2]))/n() * 100) %>%
      arrange(perc_time_ir)
    
    fig <- plot_ly(tir, x = ~id, y = ~perc_time_low, type = 'bar',
                   name = paste0("< ",value[1]),hoverinfo = 'text',
                   text = ~paste(round(perc_time_low,2),"%"),
                   color = '#1f77b4')
    fig <- fig %>% add_trace(y = ~perc_time_ir,hoverinfo = 'text',
                             name = paste0(value[1]," - ",value[2]),
                             text = ~paste(round(perc_time_ir,2),"%"),
                             color = '#2ca02c')
    fig <- fig %>% add_trace(y = ~perc_time_high,hoverinfo = 'text',
                             name = paste0("> ",value[2]),
                             text = ~paste(round(perc_time_high,2),"%"),
                             color = '#d62728')
    fig <- fig %>% layout(barmode = 'stack',yaxis = list(title = "Percent Time"),
                          xaxis = list(title = "ID",categoryorder = "array",
                                       categoryarray = ~id))
    return(fig)
  })
app$run_server(showcase = T)
