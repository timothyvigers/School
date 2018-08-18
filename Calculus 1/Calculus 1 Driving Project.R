# Driving data project for Calculus 1

# Read in data, load ggplot.
drivingdata <- read.csv("Driving Project Data.csv",stringsAsFactors = FALSE)
library(ggplot2)

# Convert mph to miles per minute.
drivingdata$Speed..mpm <- drivingdata$Speed..mph./60

# Plot speed by minute.
plot <- 
  ggplot(drivingdata,aes(x=drivingdata$Time..min.,y=drivingdata$Speed..mpm)) + 
  geom_point() + labs(x = "Time (minutes)",y = "Speed (miles per minute)") + 
  scale_x_continuous(breaks = drivingdata$Time..min.)

# Add left Riemann rectangles.
leftplot <- plot + 
  geom_rect(xmin = 0,xmax = 2,ymin = 0, ymax = 0,
            colour = "lightseagreen",fill = "lightseagreen") + 
  geom_rect(xmin = 2,xmax = 4,ymin = 0, ymax = drivingdata$Speed..mpm[2],
            colour = "lightseagreen",fill = "lightseagreen") +
  geom_rect(xmin = 4,xmax = 6,ymin = 0, ymax = drivingdata$Speed..mpm[4],
            colour = "lightseagreen",fill = "lightseagreen") +
  geom_rect(xmin = 6,xmax = 8,ymin = 0, ymax = drivingdata$Speed..mpm[6],
            colour = "lightseagreen",fill = "lightseagreen") +
  geom_rect(xmin = 8,xmax = 10,ymin = 0, ymax = drivingdata$Speed..mpm[8],
            colour = "lightseagreen",fill = "lightseagreen") +
  geom_rect(xmin = 10,xmax = 12,ymin = 0, ymax = drivingdata$Speed..mpm[10],
            colour = "lightseagreen",fill = "lightseagreen") +
  geom_rect(xmin = 12,xmax = 14,ymin = 0, ymax = drivingdata$Speed..mpm[12],
            colour = "lightseagreen",fill = "lightseagreen") +
  geom_rect(xmin = 14,xmax = 16,ymin = 0, ymax = drivingdata$Speed..mpm[14],
            colour = "lightseagreen",fill = "lightseagreen") +
  labs(x = "Time (minutes)",y = "Speed (miles per minute)") + geom_point()

# Add midpoint rectangles.
midplot <- plot + 
  geom_rect(xmin = 0,xmax = 2,ymin = 0, ymax = drivingdata$Speed..mpm[1],
            colour = "dodgerblue",fill = "dodgerblue") + 
  geom_rect(xmin = 2,xmax = 4,ymin = 0, ymax = drivingdata$Speed..mpm[3],
            colour = "dodgerblue",fill = "dodgerblue") +
  geom_rect(xmin = 4,xmax = 6,ymin = 0, ymax = drivingdata$Speed..mpm[5],
            colour = "dodgerblue",fill = "dodgerblue") +
  geom_rect(xmin = 6,xmax = 8,ymin = 0, ymax = drivingdata$Speed..mpm[7],
            colour = "dodgerblue",fill = "dodgerblue") +
  geom_rect(xmin = 8,xmax = 10,ymin = 0, ymax = drivingdata$Speed..mpm[9],
            colour = "dodgerblue",fill = "dodgerblue") +
  geom_rect(xmin = 10,xmax = 12,ymin = 0, ymax = drivingdata$Speed..mpm[11],
            colour = "dodgerblue",fill = "dodgerblue") +
  geom_rect(xmin = 12,xmax = 14,ymin = 0, ymax = drivingdata$Speed..mpm[13],
            colour = "dodgerblue",fill = "dodgerblue") +
  geom_rect(xmin = 14,xmax = 16,ymin = 0, ymax = drivingdata$Speed..mpm[15],
            colour = "dodgerblue",fill = "dodgerblue") +
  labs(x = "Time (minutes)",y = "Speed (miles per minute)") + geom_point()

# Add right Riemann rectangles.
rightplot <- plot + 
  geom_rect(xmin = 0,xmax = 2,ymin = 0, ymax = drivingdata$Speed..mpm[2],
            colour = "darkorchid",fill = "darkorchid") + 
  geom_rect(xmin = 2,xmax = 4,ymin = 0, ymax = drivingdata$Speed..mpm[4],
            colour = "darkorchid",fill = "darkorchid") +
  geom_rect(xmin = 4,xmax = 6,ymin = 0, ymax = drivingdata$Speed..mpm[6],
            colour = "darkorchid",fill = "darkorchid") +
  geom_rect(xmin = 6,xmax = 8,ymin = 0, ymax = drivingdata$Speed..mpm[8],
            colour = "darkorchid",fill = "darkorchid") +
  geom_rect(xmin = 8,xmax = 10,ymin = 0, ymax = drivingdata$Speed..mpm[10],
            colour = "darkorchid",fill = "darkorchid") +
  geom_rect(xmin = 10,xmax = 12,ymin = 0, ymax = drivingdata$Speed..mpm[12],
            colour = "darkorchid",fill = "darkorchid") +
  geom_rect(xmin = 12,xmax = 14,ymin = 0, ymax = drivingdata$Speed..mpm[14],
            colour = "darkorchid",fill = "darkorchid") +
  geom_rect(xmin = 14,xmax = 16,ymin = 0, ymax = drivingdata$Speed..mpm[16],
            colour = "darkorchid",fill = "darkorchid") +
  labs(x = "Time (minutes)",y = "Speed (miles per minute)") + geom_point()