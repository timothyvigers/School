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
  id = sub(".*_","",tools::file_path_sans_ext(basename(x)))
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

# dccSlider starts from 5;
app$layout(
  htmlDiv(
    list(
      dccGraph(id = 'agp-with-slider'),
      dccSlider(
        id = 'time-slider',
        value = 5,
        min = 5,
        max = 60,
        step = 5
      )
    )
  )
)

app$callback(
  output = list(id='agp-with-slider', property='figure'),
  params = list(input(id='time-slider', property='value')),
  
  function(minutes) {
    t = paste(minutes,"minutes")
    df$agp = lubridate::round_date(df$timestamp,unit = t)
    df$agp = as.POSIXct(strftime(df$agp,format = "%H:%M"),format = "%H:%M")
    # Sumarize
    summ = df %>% arrange(id,agp) %>% dplyr::group_by(id,agp) %>%
      summarise(sg = mean(sensorglucose,na.rm = T)) %>%
      mutate(label = format(agp,format = "%H:%M")) %>% ungroup()
    # Smooth with splines
    summ = summ %>% group_by(id) %>%
      mutate(id_spline = as.numeric(predict(smooth.spline(sg))$y)) %>% ungroup()
    # LOESS for overall cohort
    smooth = loess(summ$sg~as.numeric(summ$agp))
    # Plotly
    # Regular AGP
    list(data = list(x = summ$agp, 
                     y = summ$id_spline,
                     type = 'scatter'),        
      layout = list(title = minutes)
    )
  }
)

# agp <- summ %>% plotly::group_by(id) %>%
#   plot_ly(x = ~agp, y = ~id_spline,
#           text=~paste0("ID: ",id,"\n","Time: ",label,"\n","Mean SG: ",round(sg))) %>%
#   add_lines(hoverinfo = 'text',
#             line = list(color = 'rgb(31, 119, 180)'),opacity = 0.5) %>% plotly::ungroup() %>%
#   add_lines(y=smooth$fitted,text=~paste0("Time: ",label,"\n","Mean SG: ",round(sg)),
#             hoverinfo = 'text',line=list(color = 'rgb(255, 127, 14)')) %>%
#   layout(
#     xaxis = list(
#       type = 'date',
#       tickformat = "%H:%M",
#       title = "Time of Day"),
#     yaxis = list(
#       title = "Mean Sensor Glusose (mg/dL)",
#       range = c(0,400)))
# 
# # Radial AGP
# summ$t = as.numeric((summ$agp - min(summ$agp)))
# summ$t = summ$t/(max(summ$t)/360)
# 
# radagp <- summ %>% plotly::group_by(id) %>%
#   plotly::plot_ly(r = ~id_spline,theta = ~t,type = 'scatterpolar',
#                   mode = 'lines',hoverinfo = "text",opacity = 0.5,
#                   text = ~paste0("ID: ",id,"\n","Time: ",label,"\n","Mean SG: ",round(sg)),
#                   line = list(color = 'rgb(31, 119, 180)')) %>%
#   add_trace(r = ~smooth$fitted,opacity = 1,
#             text = ~paste0("Time: ",label,"\n","Mean SG: ",round(sg)),
#             line = list(color = 'rgb(255, 127, 14)')) %>%
#   layout(showlegend = TRUE,
#          polar = list(
#            radialaxis = list(
#              visible = TRUE,
#              ticks = "outside",
#              angle = 90,
#              tickangle = 90,
#              range = c(0,400)),
#            angularaxis = list(
#              rotation = 90,
#              direction = 'clockwise',
#              tickvals = c(90,180,270),
#              ticktext = c("06:00","12:00","18:00")
#            )
#          )
#   )
app$run_server(showcase = T)
