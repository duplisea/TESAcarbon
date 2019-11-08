#carbon budget for TESA travel

# carbon.params=list(
#   #from https://calculator.carbonfootprint.com
#   #consider using http://impact.brighterplanet.com/documentation
#   # meal assumes an average cost per meal of $20 USD
#   # hotel assumes a cost of $160 USD/night
#   # the meal carbon cost is incremental, i.e. you would eat at home so the carbon cost is reduced by 50% based on
#   # cbc article that says restaurants are twice as carbon intensive per meal than at home.
#   # Units are in tonnes CO2
#   C.plane= c(6.507e-5,0.00),
#   C.car= c(0.000283,0),
#   C.bustrain= c(5.8e-5,0),
#   C.hotel= c(0.064,0),
#   C.meal= c(0.0162,0),
#   C.meal.discount= 0.5)
# save(carbon.params,file="~/github/TESAcarbon/data/carbon.params.rda")

#' Calculate the incremental carbon footprint of a person attending a TESA activity
#'
#' @param hotel.nights number of nights staying at a hotel solo in a room
#' @param plane.distance the total flight distance (back and forth) from home to the activity. Set as 0 if you did not take the plane (km)
#' @param bustrain.distance the total distance travelled on a bus or train for the activity. Set as 0 if you did not take a bus or train (km)
#' @param car.distance the total distance travelled in a car for the activity. Set as 0 if you did not take a car (km)
#' @param number.car.sharing the number of people who shared the car to travel to and from the activity
#' @param meals the number of meals eaten out during the trip (do not include meals you made yourself)
#' @description  This is a rough CO2 calculator. The output is in tonnes CO2 for the trip. It is based on
#'         information from the carboon footprint calculator. It makes several assumptions: an average meal out
#'         costs 20 USD. The average hotel cost is 160 USD/night, bustrain carbon cost is an average of several
#'         modes of wheeled public transport, car travel is based on a standard gasoline car with engine > 2l.
#'         Meal carbon costs are discounted by 50 percent because restaurants are about half as efficient as home
#'         for carbon use mostly owing to food waste (cbc article). Results compare pretty well with
#'         https://calculator.carbonfootprint.com but they tend to be slightly lower primarily because restaurant
#'         meal carbon costs are discounted by 50 percent.
#'
#'         The carbon footprint is incremental in that it assumes most of the carbon consuming activities that a
#'         participant does at home (except meals) continue at home while they are away. e.g. they still heat their
#'         house, feed the dog. The incremental cost might be slightly high but given the result is less than
#'         the total carbon cost from carbonfootprint, we may have struck a balance. We also consider this
#'         most useful in comparison between TESA activities and years rather than as an absolute CO2 emission
#'         measure.
#' @author Daniel Duplisea
#' @export
#' @examples C.f(hotel.nights=5, plane.distance=1200, bustrain.distance= 0, car.distance=40, number.car.sharing=1, meals=15)
C.f= function(hotel.nights, plane.distance, bustrain.distance, car.distance, number.car.sharing, meals){
  number.car.sharing[number.car.sharing==0]=1
  c.hotel= carbon.params$C.hotel[1] * hotel.nights + carbon.params$C.hotel[2]
  c.plane= carbon.params$C.plane[1] * plane.distance + carbon.params$C.plane[2]
  c.bustrain= carbon.params$C.bustrain[1] * bustrain.distance + carbon.params$C.bustrain[2]
  c.car= (carbon.params$C.car[1] * car.distance + carbon.params$C.car[2]) / number.car.sharing
  c.meal= (carbon.params$C.meal[1] * meals + carbon.params$C.meal[2]) * carbon.params$C.meal.discount
  C.total= c.plane + c.car + c.hotel + c.meal
  C.total
}

#' Lookup flying distances between locations
#'
#' @param input.data an object in the workspace with at least columns exactly named:
#'        "activity.type"
#'        "activity.name"
#'        "origin"
#'        "destination"
#'        "origin.country"
#'        "hotel.nights"
#'        "meals"
#'        "bustrain.distance"
#'        "car.distance"
#'        "car.sharing"
#' @description This looks up flying distances between origin and destination airports. Flying distances are
#'        taken as direct geographic distances between cities and does not accounting for routing. It has some
#'        simple error checking. If you get an error it will likely be here and it is because the cities database
#'        does not contain your location. Some options are given when an error arises.
#'
#'        You are unlikely to want to call this function on its own. It is used inside the main function.
#' @authors Daniel Duplisea, Martin Pastoors
#' @export
distance.lookup.f= function(input.data){
  origin.vector= input.data$origin
  destination.vector= input.data$destination
  countries= unique(input.data$origin.country)
  datacitycountry= paste0(input.data$origin, input.data$origin.country)
  # lookup distances between locations
  #cities= as.data.table(world.cities)[country.etc %in% countries]
  cities= as.data.table(world.cities)[country.etc %in% countries]
    #error check cities
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

#' Calculate the carbon footprint for meetings based on transportation, hotel and restaurant meals
#'
#' @param input.data an object in the workspace with at least columns exactly named:
#'        "activity.type"
#'        "activity.name"
#'        "origin"
#'        "destination"
#'        "origin.country"
#'        "hotel.nights"
#'        "meals"
#'        "bustrain.distance"
#'        "car.distance"
#'        "car.sharing"
#' @param Title.name A text string for how you want the plot named
#' @param list.out logical, output the list to the console
#' @description Calculates the carbon emissions footprint for all the activites. It plots the calculation
#'        as a bar graph with total and per capita values. It also produces a map with all the home cities of
#'        participants as well as the destination cities which are circled in red. The total carbon emissions
#'        of all activities combined is shown in the title. A list with calculations is also produced.
#' @authors Daniel Duplisea
#' @export
carbon.footprint.f= function(input, Title.name="Carbon footprint", list.out=T){
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
  if (list.out) tab
}
