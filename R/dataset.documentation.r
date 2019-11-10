# dataset documentation

#' Carbon parameters
#'
#' A list with different carbon emission rates for one unit of travel (km), hotel (nights) and restaurant food (meals)
#'
#' \itemize{
#'   \item C.plane carbon emissions per km of plane travel per person
#'   \item C.car carbon emissions per km of car travel for a car with a motor > 2l
#'   \item C.bustrain carbon emissions per km of bus or train travel per person
#'   \item C.hotel carbon emissions per night in an average hotel (160 USD/night)
#'   \item C.meal carbon emissions per restaurant meal (20 USD/meal), average over breakfast, lunch, supper
#'   \item C.meal.discount the proportional reduction in restaurant meal carbon to compensate for the fact
#'         that participants are not eating a meal at home if they are eating one in a restaurant. A CBC article
#'         cited a value showing that eating a meal in a restaurant emits about 2X the amount of carbon per meal
#'         than one does at home owing to food waste and other forms of additional carbon consumption associated
#'         with restaurants
#' }
#'
#' @docType data
#' @keywords datasets
#' @name carbon.params
#' @usage data(params)
#' @format A list
NULL


#' ICES
#'
#' A data.frame with a mock example of how a course program for ICES would need to be setup in order to calculate
#' the carbon footprint for training courses.
#'
#' \itemize{
#'   \item activity.type the type of activity. You choose. e.g. online course, course, workshop, working group ...
#'   \item activity.name whatever you want to name the activity for the output figure
#'   \item origin the home city of the participant. Please try to choose the closest centre with >1000 people. For small places, e.g. Faroe Islands, you can use small centres
#'   \item destination the distination city for the activity
#'   \item origin.country the home country of the participant
#'   \item hotel.nights the number of nights the participant will sleep in a hotel. 0 if none
#'   \item meals the total number of meals the participant will eat in a restaurant. 0 if none
#'   \item bustrain.distance the total distance that a participant will take the bus or train. 0 if none
#'   \item car.distance the total distance that a participant will drive a car. 0 if none
#'   \item flying logical, if flying or not, 1 for yes 0 for no.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name ICES
#' @usage data(ICES)
#' @format A data.frame or data.table or tibble
NULL
