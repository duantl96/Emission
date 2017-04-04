library(tidyr)
library(dplyr)
library(readr)

### Read in Data
# departmental emission data
emission = read.csv("dept_emission_selected.csv")
# weather data
weather = read.csv("Durham_monthlyClimate.csv")[1:24,]
# course data
c = read.csv("dept_building_selected.csv")
# term-month correspondence data
t_m = read.csv("Term_Month_correspondence.csv",header = F)
colnames(t_m) = c("Month","Term")

### Join in weather data
e.w = left_join(emission,weather,by=c("AcctgMonth"="Month"))
#write.csv(e.w,"dept_emission_withWeather.csv",row.names = F)

### Process course data
# transform to long df
c = c %>%
  gather(.,"Term","n_course",3:10) %>%
  group_by(Dept.Name,Term) %>%
  summarise(sum(n_course))
#write.csv(c,"dept_course_selected.csv",row.names = F)

### join in correspondence data
e.w.c = left_join(e.w,t_m,by = c("AcctgMonth"="Month"))
e.w.c = left_join(e.w.c,c,by = c("Dept.Name","Term"))
#write.csv(e.w.c,"FULL.csv",row.names = F)
