library(data.table)
library(ggplot2)
library(maps)
library(geosphere)
library(cowplot)
library(ggrepel)


#city.position[home %like% "Andrew"]
setwd("/home/daniel/github/TESAcarbon/extra/")
#save(TESA19.20,file="~/github/TESAcarbon/data/TESA19.20.rda")
TESA19.20= fread("TESA19-20.csv")

input=TESA19.20
input.data=TESA19.20

distance.lookup.f= function(input.data){
  origin.vector= input.data$origin
  destination.vector= input.data$destination
  countries= unique(input.data$origin.country)
  datacitycountry= paste0(input.data$origin, input.data$origin.country)
  # lookup distances between locations
  cities= as.data.table(world.cities)[country.etc %in% countries]
    #error check countries and cities
  #  cbind(sort(cities[,unique(country.etc)]),sort(countries)) stop("origin and destination vectors are not the same length")

    if(any(!(origin.vector %in% cities$name)))
      stop(paste(origin.vector[!(origin.vector %in% cities$name)], ": this origin city country combination does not have an entry in the world cities database. Options:
        (1) Names are generally English but not always, e.g. use Goteborg and not Gothenburg
        (2) Do not use accents
        (3) Centres with populations < 1000 are often not in the database. Try a close larger centre
        (4) Try fuzzy matching your centre with the first three letters in quotation marks and in sentence case, e.g.
            world.cities[grep('Got', world.cities$name),]
            and use the city name in the 'name' and 'country.etc' column in your input data.sheet"))
  cities$citycountry= paste0(cities$name,cities$country.etc)
  cities= cities[citycountry %in% datacitycountry]
  city.matrix= CJ(name=cities$name, name1=cities$name,unique=TRUE)
  tmp= merge(x=city.matrix, y=cities[,.(name,lat,long)], by.x = "name", by.y = "name", allow.cartesian=TRUE)
  city.position= merge(x=tmp, y=cities[,.(name,lat,long)], by.x = "name1", by.y = "name", allow.cartesian=TRUE)
  city.position= data.table(origin=city.position$name1, destination=city.position$name, long.origin=city.position$long.y,
    lat.origin=city.position$lat.y, long.destination= city.position$long.x, lat.destination= city.position$lat.x)
  city.position$distance= distGeo(city.position[,.(long.origin,lat.origin)],
                                  city.position[,.(long.destination,lat.destination)])/1000
  city.position= city.position[origin %in% unique(origin.vector) | destination %in% unique(destination.vector)]
  city.position$city.combo= paste0(city.position$origin,city.position$destination)
  distances=vector(length=length(origin.vector))
  for (i in 1:length(origin.vector)){
    distances[i]= city.position[city.position$origin==origin.vector[i] &
              city.position$destination==destination.vector[i],]$distance
  }
  distances= round(distances,0)

  #lat and long of cities
  origin.locations= cities[ name %in% unique(origin.vector)]
  destination.locations= cities[ name %in% unique(destination.vector)]
  localisation= list(distance= distances, origin.locations= origin.locations,
    destination.locations= destination.locations)
  localisation
}






carbon.footprint.f= function(input, Title.name="Carbon footprint"){
  localisation= distance.lookup.f(input)
  localisation$origin.locations$capital=0
  localisation$destination.locations$capital=1
  cities= localisation$origin[!(name %in% localisation$destination.locations$name)]
  cities= rbind(localisation$destination,cities)
  input$plane.distance= localisation$distance
  input$C= C.f(hotel.nights=input$hotel.nights,
    plane.distance=input$plane.distance*2,
    bustrain.distance= input$bustrain.distance*2,
    car.distance= input$car.distance*2,
    number.car.sharing=input$car.sharing,
    meals=input$meals)
  total.per.activity= round(tapply(input$C,input$activity.name,sum),3)
  participants.per.activity= tapply(input$C,input$activity.name,length)
  per.capita.per.activity=round(total.per.activity/participants.per.activity,3)
  mean.per.capita= round(mean(per.capita.per.activity),3)
  total.C= round(sum(input$C),3)
  tab1= data.frame(Activity=names(total.per.activity),
    Total=total.per.activity,
    Participation=participants.per.activity,
    C.per.person=per.capita.per.activity)
  location= input[,.(unique(destination),unique(activity.type)),activity.name]
  tab1$location= location$V1[match(tab1$Activity,location$activity.name)]
  tab1$type= location$V2[match(tab1$Activity,location$activity.name)]
  tab= list(carbon= tab1, countries= unique(input$origin.country),locations= cities)
  tab

  mapbar= function(C.emissions){
    carbon= ggplot(tab$carbon,aes(y=Total,x=Activity))+
      geom_col(show.legend = F,size=.5) +
      ylim(0,max(tab$carbon$Total)*1.2)+
      annotate("text",x=1:nrow(tab$carbon),y=rep(max(tab$carbon$Total)*1.15,rep=nrow(tab$carbon)),label=tab$carbon$location,cex=2) +
      annotate("text",x=1:nrow(tab$carbon),y=rep(max(tab$carbon$Total)*1.05,rep=nrow(tab$carbon)),label=tab$carbon$type,cex=2) +
      annotate("text",x=1:nrow(tab$carbon),y=rep(max(tab$carbon$Total)/4,rep=nrow(tab$carbon)),
        label=paste0("per capita=",round(tab$carbon$C.per.person,2)),cex=3,col="orange")+
      theme_classic()+
      aes(stringr::str_wrap(Activity, 15))+
      ylab("Carbon emissions (t)")+
      xlab(NULL)+
      coord_flip()

    map=ggplot(as.data.table(map_data("world")), aes(x = long, y = lat, group = group)) +
      geom_polygon(fill="lightgray", colour = "white") +
      geom_point(data = tab$locations,aes(long, lat, group = name)) +
      geom_point(data = tab$locations[capital==1],aes(long, lat, group = name),col='red',pch=21,size=2,stroke=2) +
      geom_text_repel(data = tab$locations,aes(long, lat,label = name,group = name),color = 'black', size  = 3,
        box.padding = 0.7, point.padding = 0.5) +
      theme_void()

    plots= plot_grid(map,carbon,nrow=2)
    title <- ggdraw() +
      draw_label(paste0(Title.name, ', Total C = ',round(sum(tab$carbon$Total),1),' t'), fontface = 'bold', x = 0, hjust = 0) +
      theme(plot.margin = margin(0, 0, 0, 7))
      plot_grid(title, plots, ncol = 1,rel_heights = c(0.1, 1))
  }
  print(mapbar(tab))
  tab
}

library(TESAcarbon)

#save(ICES, file="~/github/TESAcarbon/data/ICES.rda")
ICES= fread("ICES.1course.csv")
pdf("ICES.mock.example.pdf")
carbon.footprint.f(ICES, "ICES training carbon footprint, mock example")
dev.off()

TESA19.20= fread("TESA19-20.csv")
pdf("TESA.19-20.pdf")
carbon.footprint.f(TESA19.20,"TESA carbon footprint 2019-20")
dev.off()
