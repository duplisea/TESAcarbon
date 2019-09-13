#carbon budget for TESA travel

# carbon.params=list(
#   #from https://calculator.carbonfootprint.com
#   #consider using http://impact.brighterplanet.com/documentation
#   # meal assumes an average cost per meal of $20 USD
#   # hotel assumes a cost of $160 USD/night
#   # the meal carbon cost is incremental, i.e. you would eat a home so the carbon cost is reduced by 50% based on
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

#' Lookup flying distances between locations (main DFO science locations)
#'
#' @param origin the origin city. It must be an exact match of the distance lookup table. No spaces and no
#'        punctuation.
#' @param destination the destination for the activity. For a single activity in say Ottawa use
#'        destination="Ottawa". If you put it in as a vector with "Ottawa" repeated as many times as the
#'        origin vector, then just use the first column of the output.
#' @description This looks up flying distances between origin and destination airports. Flying distances are
#'        generally taken as direct air distances between cities (https://canada-map.com/distance/). For
#'        smaller airports, where doubling back usually occurs then this is considered. For example Mont-Joli
#'        to St. John's requires that you fly southwest to Montreal before flying over Mont-Joli again
#'        northeast to get to St. John's. There we no effort to account for example that essentially straight
#'        flights would still have layovers at intermediate airports that could add km to the actual flying
#'        distance. The multiple possible routings can easily violate assumptions made in that kind of
#'        consideration.
#' @author Daniel Duplisea
#' @export
#' @examples
#'        # for the exact names in the distance table
#'        names(flying.distances)
#'
#'        # An example use the TESA.course data and Ottawa as destination
#'        distance.match.f(TESA.course$Home, "Ottawa")
#'
#'        # You will see that the last three values are NA and that is because neither Burlington nor
#'        # Copenhagen have entries in the distance table
distance.match.f= function(origin, destination){
  origin.pos= match(origin,names(flying.distances))
  destination.pos= match(destination,row.names(flying.distances))
  fdistance= flying.distances[origin.pos,destination.pos]
  fdistance
}

