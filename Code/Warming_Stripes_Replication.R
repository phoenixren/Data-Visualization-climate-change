#Import libraries
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

#Read extreme temperature data from 1850-2018
temp <- read.table("~/Documents/Temp.txt")
str(temp)

#Select only the year and median of the temperature to build warming stripes 
temp_yr <- select(temp, V1, V2)

#Transform the year into timestamp for easier manipulation
temp_yr <- mutate(temp_yr,date=str_c(V1,"01-01",sep="-")%>%ymd())

#There is no outlier 
summary(temp_yr) 

#Create the style for the graph
theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major=element_blank(),
        legend.title = element_blank(),
        axis.text.x=element_text(vjust=3),
        panel.grid.minor=element_blank(),
        plot.title=element_text(size=14,face="bold")
  )

#Assign colors
col_strip <- brewer.pal(11,"RdBu")

#Create the graph
ggplot(temp_yr,
       aes(x=date,y=1,fill=V2))+
  geom_tile()+
  scale_x_date(date_breaks = "6 years",
               date_labels = "%Y",
               expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_gradientn(colors=rev(col_strip))+
  guides(fill=guide_colorbar(barwidth = 2))+
  labs(title="Warm Stripes",
       caption="Global Surface Temperature Analysis")+
  theme_strip