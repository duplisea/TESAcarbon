Carbon footprint for TESA activities
------------------------------------

The group Technical Expertise in Stock Assessment (TESA) organises
courses and workshops each year for DFO employees to continuarlly build
the expertise in the Department of Fisheries and Oceans to get to,
remain at and hopefully push the boundaries of stock assessment
expertise. The activities organised by TESA entail a carbon footprint in
the form of travel to training courses and workshops. TESA also,
however, offers online training possibilities each year which have
little or neglible incremental carbon emissions associated with them.

In an effort to quantify TESA carbon emissions, to track them by year,
activity and location, this carbon footprint calculator has been
developed. It accounts only for travel related carbon emissions
(transport, hotel, meals) associated with TESA activities.

Carbon emissions (tonnes CO2) per km flying, driving, train or bus and
emissions for a night in a hotel or average meal in a restaurant are
taken from
<a href="https://calculator.carbonfootprint.com/" class="uri">https://calculator.carbonfootprint.com/</a>.
Flying distances are generally taken as direct air distances between
cities
(<a href="https://canada-map.com/distance/" class="uri">https://canada-map.com/distance/</a>).
For smaller airports, where doubling back usually occurs then this is
considered. For example Mont-Joli to St. John’s requires that you fly
southwest to Montreal before flying over Mont-Joli again northeast to
get to St. John’s. There we no effort to account for example that
essentially straight flights would still have layovers at intermediate
airports that could add km to the actual flying distance. The multiple
possible routings can easily violate assumptions made in that kind of
consideration and we cannot really account for that in this kind of
calculator since each traveller will determine their own routing.

Given assumptions, the most useful aspect of this is to keep
calculations consistent and then look at changes from year to year in
the carbon emissions.

Install and load the library
----------------------------

    devtools::install_github("duplisea/TESAcarbon")
    library(TESAcarbon)

Calculate the incremental carbon footprint for a traveller
----------------------------------------------------------

Five nights in a hotel, a 600 km plane trip (1200 km total return), 40
km of driving to and from airport solo and 15 meals

    C.f(hotel.nights=5,
        plane.distance=600*2,
        bustrain.distance= 0,
        car.distance=40,
        number.car.sharing=1,
        meals=15)

    ## [1] 0.530904

Carbon emissions for a hypothetical course in Ottawa
----------------------------------------------------

Here is an example for a course being held in Ottawa with 27
participants from all regions. Most participants fly but two
participants from Burlington decide to drive and share a car. Most
participants stay five days but a couple stay only four. It is assumed
that everyone except people from Ottawa have 14 meals. The Ottawa people
have 1 meal which is the group supper. The course instructor is from
Copenhagen and they have one more night’s stay and 3 more meals.

    # This is how you would import the csv file but it is already available by default in the library
    #TESA.course= read.csv("TESA.course.csv")
    TESA.course$C= C.f(hotel.nights=TESA.course$hotel.nights,
        plane.distance=TESA.course$plane.distance*2,
        bustrain.distance= TESA.course$bustrain.distance*2,
        car.distance= TESA.course$car.distance*2,
        number.car.sharing=TESA.course$car.sharing,
        meals=TESA.course$meals)

    ## Error in `$<-.data.frame`(`*tmp*`, C, value = numeric(0)): replacement has 0 rows, data has 27

    TESA.course[,c(2,10)]

    ## Error in `[.data.frame`(TESA.course, , c(2, 10)): undefined columns selected

    sum(TESA.course$C)

    ## [1] 0

You will see that the Copenhagen trip is the most carbon heavy while the
locals in Ottawa have the lowest footprint. Driving a large car from
Burlington with two people is not very different than flying from
Halifax or Moncton. The total incremental carbon footprint of TESA
activity is 17.2 tonnes of C02.

Projected carbon emissions for TESA 2019/20 activities
------------------------------------------------------

In order to distribute money to regions each year, TESA requires a
fairly solid participant list from each region where travel costs per
participant can be determined and funds allocated accordingly. This list
was used to develop a projected carbon emissions scenario for 2019.

First you need to determine the flying distances from the distances
lookup square matrix. This is sparse at the moment with only locations
that have DFO research labs associated with them included as that is
where TESA activity participants come from (fill this in further if you
want and create a pull request).

    activities= as.character(unique(TESA19to20$activity.name))

    ## Error in unique(TESA19to20$activity.name): object 'TESA19to20' not found

    all.activity.distance=vector()
    for (i in 1:length(activities)){
      activity= activities[i]
      specific.activity= TESA19to20[TESA19to20$activity.name==activity,]
      activity.plane.distance= distance.match.f(specific.activity$origin,specific.activity$destination)[,1]
      all.activity.distance= c(all.activity.distance, activity.plane.distance)
    }

    ## Error in eval(expr, envir, enclos): object 'activities' not found

    all.activity.distance[is.na(all.activity.distance)]=0
    TESA19to20$plane.distance= all.activity.distance

    ## Error in TESA19to20$plane.distance = all.activity.distance: object 'TESA19to20' not found

The above does some formatting as well like it changes NA to 0.

    TESA19to20$C= C.f(hotel.nights=TESA19to20$hotel.nights,
        plane.distance=TESA19to20$plane.distance*2,
        bustrain.distance= TESA19to20$bustrain.distance*2,
        car.distance= TESA19to20$car.distance*2,
        number.car.sharing=TESA19to20$car.sharing,
        meals=TESA19to20$meals)

    ## Error in C.f(hotel.nights = TESA19to20$hotel.nights, plane.distance = TESA19to20$plane.distance * : object 'TESA19to20' not found

    TESA19to20$C[is.nan(TESA19to20$C)]=0

    ## Error in TESA19to20$C[is.nan(TESA19to20$C)] = 0: object 'TESA19to20' not found

    round(sum(TESA19to20$C),3)

    ## Error in eval(expr, envir, enclos): object 'TESA19to20' not found

Now we can summarise the calculation and make a table

    total.per.activity= round(tapply(TESA19to20$C,TESA19to20$activity.name,sum),3)

    ## Error in tapply(TESA19to20$C, TESA19to20$activity.name, sum): object 'TESA19to20' not found

    participants.per.activity= tapply(TESA19to20$C,TESA19to20$activity.name,length)

    ## Error in tapply(TESA19to20$C, TESA19to20$activity.name, length): object 'TESA19to20' not found

    per.capita.per.activity=round(total.per.activity/participants.per.activity,3)

    ## Error in eval(expr, envir, enclos): object 'total.per.activity' not found

    mean.per.capita= round(mean(per.capita.per.acitivty),3)

    ## Error in mean(per.capita.per.acitivty): object 'per.capita.per.acitivty' not found

    total.C= round(sum(TESA19to20$C),3)

    ## Error in eval(expr, envir, enclos): object 'TESA19to20' not found

    sort.of.nice.table= t(rbind(total.per.activity,participants.per.activity,per.capita.per.activity))

    ## Error in rbind(total.per.activity, participants.per.activity, per.capita.per.activity): object 'total.per.activity' not found

    sort.of.nice.table=rbind(sort.of.nice.table, c(sum(sort.of.nice.table[,1]),sum(sort.of.nice.table[,2]),mean(sort.of.nice.table[,3])))

    ## Error in rbind(sort.of.nice.table, c(sum(sort.of.nice.table[, 1]), sum(sort.of.nice.table[, : object 'sort.of.nice.table' not found

    row.names(sort.of.nice.table)[6]=  "Summary"

    ## Error in row.names(sort.of.nice.table)[6] = "Summary": object 'sort.of.nice.table' not found

    sort.of.nice.table

    ## Error in eval(expr, envir, enclos): object 'sort.of.nice.table' not found

References
----------

<a href="https://calculator.carbonfootprint.com/" class="uri">https://calculator.carbonfootprint.com/</a>
