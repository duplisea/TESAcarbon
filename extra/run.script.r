#flying.distances= read.csv("~/github/TESAcarbon/extra/flying.distances.csv", header=T, row.names=1)
#save(flying.distances,file="~/github/TESAcarbon/data/flying.distances.rda")
#TESA.course= read.csv("~/Downloads/TESA.course.csv")
#save(TESA.course,file="~/github/TESAcarbon/data/TESAcourse.rda")
library(TESAcarbon)
TESA.course$plane.distance= distance.match.f(TESA.course$Home,"Ottawa")
#fill in NA  flying distances
TESA.course$plane.distance[25:27]=c(0,0,6000)


#2019-2020 programme
TESA.1920= read.csv("~/github/TESAcarbon/extra/TESA19-20.csv")
activities= as.character(unique(TESA.1920$activity.name))
all.activity.distance=vector()
for (i in 1:length(activities)){
  activity= activities[i]
  specific.activity= TESA.1920[TESA.1920$activity.name==activity,]
  activity.plane.distance= distance.match.f(specific.activity$origin,specific.activity$destination)[,1]
  all.activity.distance= c(all.activity.distance, activity.plane.distance)
}
all.activity.distance[is.na(all.activity.distance)]=0
TESA.1920$plane.distance= all.activity.distance*2
TESA.1920$car.distance= TESA.1920$car.distance*2
TESA.1920$bustrain.distance= TESA.1920$bustrain.distance*2
TESA.1920$C= C.f(hotel.nights=TESA.1920$hotel.nights,
    plane.distance=TESA.1920$plane.distance*2,
    bustrain.distance= TESA.1920$bustrain.distance*2,
    car.distance= TESA.1920$car.distance*2,
    number.car.sharing=TESA.1920$car.sharing,
    meals=TESA.1920$meals)
TESA.1920$C[is.nan(TESA.1920$C)]=0
round(sum(TESA.1920$C),3)

tmp=data.frame(ncol=5,nrow=length(activities))
for (i in 1:length(activities)){
  activity= activities[i]
  tmp[i,1]= activity
  specific.activity= TESA.1920[TESA.1920$activity.name==activity,]
  tmp[i,2]= unique(as.character(specific.activity$destination))
  tmp[i,3]=round(sum(specific.activity$C),3)
  tmp[i,4]= nrow(specific.activity)
  tmp[i,5]= round(tmp[i,3]/tmp[i,4],3)
}
names(tmp)= c("Activity","Location","Total.CO2","Participants","CO2.per.Participant")
tmp
