library(dplyr)
library(readr)

### Read in Data
# weather data
weather = read.csv("Durham_monthlyClimate.csv")[1:24,]
# departmental emission data
emission = read.csv("dept_emission_selected.csv")

### Join!
e.w = left_join(emission,weather,by=c("AcctgMonth"="Month"))
write.csv(e.w,"dept_emission_withWeather.csv",row.names = F)
