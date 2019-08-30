Carbon footprint for TESA activities
------------------------------------

This calculates and incremental carbon footprint for participants in
TESA activities. From this you can calculate the total incremental
carbon emissions from an activity (tonnes CO2).

Install and load the library
----------------------------

    devtools::install_github("duplisea/TESAcarbon")
    library(TESAcarbon)

Calculate the incremental carbon footprint
------------------------------------------

Five nights in a hotel, a 1200 km return plane trip, 40 km of driving to
and from airport solo and 15 meals

    C.f(hotel.nights=5,
        plane.distance=1200,
        bustrain.distance= 0,
        car.distance=40,
        number.car.sharing=1,
        meals=15)

    ## [1] 0.542304

Here is an example for a course being held in Ottawa with 27
participants from all regions. Most participants fly but two
participants from Burlington decide to drive and share a car. Most
participants stay five days but a couple stay only four. It is assumed
that everyone except people from Ottawa have 14 meals. The Ottawa people
have 1 meal which is the group supper. The course instructor is from
Copenhagen and they have one more night’s stay and 3 more meals.

    #TESA.course= read.csv("TESA.course.csv")
    TESA.course$C= C.f(hotel.nights=TESA.course$hotel.nights,
        plane.distance=TESA.course$air.distance*2,
        bustrain.distance= TESA.course$bustrain.distance*2,
        car.distance= TESA.course$car.distance*2,
        number.car.sharing=TESA.course$car.sharing,
        meals=TESA.course$meals)
    TESA.course[,c(2,10)]

    ##          Home         C
    ## 1   Vancouver 0.9054956
    ## 2   Vancouver 0.9054956
    ## 3     Nanaimo 0.8506054
    ## 4     Nanaimo 0.9146054
    ## 5     Nanaimo 0.9146054
    ## 6    MontJoli 0.4678637
    ## 7    MontJoli 0.5318637
    ## 8    MontJoli 0.5318637
    ## 9     Halifax 0.5692138
    ## 10    Halifax 0.5692138
    ## 11    Halifax 0.5692138
    ## 12    StJohns 0.6750177
    ## 13    StJohns 0.6750177
    ## 14    StJohns 0.6750177
    ## 15    StJohns 0.6750177
    ## 16     Ottawa 0.0195000
    ## 17     Ottawa 0.0195000
    ## 18   Winnipeg 0.5994352
    ## 19   Winnipeg 0.6634352
    ## 20   Winnipeg 0.6634352
    ## 21   Winnipeg 0.6634352
    ## 22    Moncton 0.5567204
    ## 23    Moncton 0.5567204
    ## 24    Moncton 0.5567204
    ## 25 Burlington 0.5778100
    ## 26 Burlington 0.5778100
    ## 27 Copenhagen 1.2988438

    sum(TESA.course$C)

    ## [1] 17.18348

You will see that the Copenhagen trip is the most carbon heavy while the
locals in Ottawa have the lowest footprint. Driving a large car from
Burlington with two people is not very different than flying from
Halifax or Moncton. The total incremental carbon footprint of TESA
activity is 17.2 tonnes of C02.

References
----------

<a href="https://calculator.carbonfootprint.com/" class="uri">https://calculator.carbonfootprint.com/</a>
