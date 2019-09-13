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
    all.activity.distance=vector()
    for (i in 1:length(activities)){
      activity= activities[i]
      specific.activity= TESA19to20[TESA19to20$activity.name==activity,]
      activity.plane.distance= distance.match.f(specific.activity$origin,specific.activity$destination)[,1]
      all.activity.distance= c(all.activity.distance, activity.plane.distance)
    }
    all.activity.distance[is.na(all.activity.distance)]=0
    TESA19to20$plane.distance= all.activity.distance

The above does some formatting as well like it changes NA to 0.

    TESA19to20$C= C.f(hotel.nights=TESA19to20$hotel.nights,
        plane.distance=TESA19to20$plane.distance*2,
        bustrain.distance= TESA19to20$bustrain.distance*2,
        car.distance= TESA19to20$car.distance*2,
        number.car.sharing=TESA19to20$car.sharing,
        meals=TESA19to20$meals)
    TESA19to20$C[is.nan(TESA19to20$C)]=0
    round(sum(TESA19to20$C),3)

    ## [1] 73.194

Now we can summarise the calculation and make a table

    total.per.activity= round(tapply(TESA19to20$C,TESA19to20$activity.name,sum),3)
    participants.per.activity= tapply(TESA19to20$C,TESA19to20$activity.name,length)
    per.capita.per.activity=round(total.per.activity/participants.per.activity,3)
    mean.per.capita= round(mean(per.capita.per.activity),3)
    total.C= round(sum(TESA19to20$C),3)
    tab= data.frame(Activity=names(total.per.activity),
      Total=total.per.activity,
      Participation=participants.per.activity,
      C.per.person=per.capita.per.activity)
    tab

    ##                    Activity  Total Participation C.per.person
    ## DataLimited     DataLimited 18.194            26        0.700
    ## Geostatistics Geostatistics 12.697            25        0.508
    ## Introduction   Introduction 23.496            35        0.671
    ## Risk                   Risk 18.807            31        0.607
    ## Rtools               Rtools  0.000            87        0.000

    sum(tab$Total)

    ## [1] 73.194

    sum(tab$Participation)

    ## [1] 204

TESA’s total incremental Carbon footprint for 2019/20 is projected to be
just over 73 tonnes CO2. 204 people are supposed to participate in TESA
activities. \#\# References

<a href="https://calculator.carbonfootprint.com/" class="uri">https://calculator.carbonfootprint.com/</a>
