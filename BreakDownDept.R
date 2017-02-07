library(readr)
library(tidyr)
library(xlsx)
library(dplyr)

### Read in the data
building=read.csv("Energy Data with Carbon Emission.csv")
dept.area=read.xlsx("Space by building and dept 11-10-16.xlsx",sheetIndex = 1)
# keep / derive variables of interest
build.keys= building %>%
  select(c(UtilityType,Carbon.Emission.MTCO2e.,BldgName,AcctgMonth,AcctgFY,kBtu,Entity,PrimaryUse))
dept.area=dept.area %>%
  select(-DEPT)
build.area=dept.area%>%
  group_by(Bldg..) %>%
  summarise(build.sum=sum(Area))
dept.area.perc=left_join(dept.area,build.area,by="Bldg..")
dept.area.perc=dept.area.perc%>%
  mutate(area.perc=Area/build.sum) %>%
  select(Bldg..,Building,Dept.Name,Area,area.perc)

### Clean up checklist
# 1. split BldgName into Buiding Name and Building ID
build.keys=build.keys%>%
  separate(BldgName,into=c("Bldg Name", "Bldg ID"),sep="\\[")
for (i in 1:length( build.keys$`Bldg ID`)){
  build.keys$`Bldg ID`[i]=substr(build.keys$`Bldg ID`[i],1,4)
} 
build.keys=build.keys%>%
  mutate(`Bldg ID`=as.numeric(`Bldg ID`))
# 24 NA for [OLD1]-- outdoor lighting
  
# 2. join by building ID
b=left_join(build.keys,dept.area.perc,by=c(`Bldg ID`="Bldg.."))
b=b %>%
  mutate(Carbon.Emission.MTCO2e.=Carbon.Emission.MTCO2e.*area.perc,kBtu=kBtu*area.perc) %>%
  select(`Bldg ID`,Building,Dept.Name,AcctgFY,AcctgMonth,UtilityType,kBtu,Carbon.Emission.MTCO2e.,Area,PrimaryUse,Entity)
# b is now the table with dept info
write.csv(b,file="dept_all_0131.csv",row.names = F)

# 3.0 get sum area of each dept
dept.area.sum=dept.area%>%
  group_by(Dept.Name)%>%
  summarise(dept.total.area=sum(Area))
write.csv(dept.area.sum,file = "dept_total_area.csv",row.names = F)
# 3. aggregate dept emission
#dept.emission=b%>%
#  group_by(Dept.Name,AcctgMonth,UtilityType)%>%
#  summarise(Carbon.Emission.MTCO2e.dept=sum(Carbon.Emission.MTCO2e.),
#            kBtu.dept=sum(kBtu)
#            ) %>%
#  arrange(AcctgMonth)

# Change added 02/06/2017: A version with only the total emission (no utility types)
dept.emission=b%>%
  group_by(Dept.Name,AcctgMonth)%>%
  summarise(Carbon.Emission.MTCO2e.sum=sum(Carbon.Emission.MTCO2e.),
         kBtu.dept=sum(kBtu)
  ) %>%
  arrange(AcctgMonth)

write.csv(dept.emission,file="dept_emission_total.csv",row.names = F)
