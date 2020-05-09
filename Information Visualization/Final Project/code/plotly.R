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
      line = list(color = 'rgb(31, 119, 180)')
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
  line = list(color = 'rgb(255, 127, 14)')
)

# Dash layout
app$layout(
  htmlDiv(
    list(
      # Title
      htmlH1('cgmViz: A web application for visualizing continuous glucose monitor data.'),
      # Tabs
      dccTabs(id="tabs-example", value='tab-1-example', children=list(
        dccTab(label='Tab One', value='tab-1-example'),
        dccTab(label='Tab Two', value='tab-2-example')
      )),
      # Regular AGP
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
                                             x0 = min(summ$agp), x1 = max(summ$agp), xref = "x",
                                             y0 = 70, y1 = 180, yref = "y")
                             )
                             
               )
      )
    )
  )
)

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
