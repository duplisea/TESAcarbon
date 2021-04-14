# Load libraries ----------------------------------------------------------
library(shiny)
library(TESAcarbon)
library(maps)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    titlePanel("TESA Carbon - Carbon footprint estimator for meetings and courses"),
    navlistPanel(
        widths=c(3,9),
        tabPanel(
            "Single traveller",
            h3("Calculate the carbon footprint for a single traveller"),
            fluidRow(
                column(width=6,
                    h4("Travel"),
                    helpText("For all of the fields below, please include return trip distance."),
                    flowLayout(
                        textInput("plane.distance", 
                            "Total distance travelled by airplane (in kilometers)", 
                            # value="Distance in kilometers",
                            placeholder="Enter a number, exclude 'km'"),
                        textInput("bustrain.distance", 
                            "Total distance travelled by bus or train (in kilometers)", 
                            # value="Distance in kilometers",
                            placeholder="Enter a number, exclude 'km'"),
                        textInput("car.distance", 
                            "Total distance travelled by car (in kilometers)", 
                            # value="Distance in kilometers",
                            placeholder="Enter a number, exclude 'km'"),
                        numericInput("number.car.sharing", 
                            "How many passengers were in your car?", 
                            # value="Distance in kilometers",
                            value=1,
                            min=1,
                            max=10,
                            step=1
                        )
                    )
                ),
                column(width=6,
                    h4("Hotel nights and meals"),
                    fluidRow(
                        textInput("hotel.nights",
                            "How many nights in a hotel will you stay?",
                            placeholder="Enter a number"
                        ),
                        textInput("meals", 
                            "How many restaurant meals?",
                            placeholder="Enter a number"
                        ),
                        hr(),
                        actionButton("calc", "Estimate carbon footprint", width="50%"),
                        tags$head(
                            tags$style(HTML('#calc{background-color:Gray;
                                color: white}'))
                        )
                    )
                )
            ),
            fluidRow(
                hr(),
                textOutput("cf.plane"),
                textOutput("cf.bustrain"),
                textOutput("cf.car"),
                textOutput("cf.hotel"),
                textOutput("cf.meal"),
                br(),
                textOutput("cf"),
                tags$head(tags$style("#cf{color: DodgerBlue;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
                    )
                )
            )
        ),
        tabPanel(
            "Multiple travellers",
            column(width=6,
                helpText("Here is some example ICES data. You can download a template, fill this in with your own travellers' data, and re-upload."),
                downloadButton("downloadTemplate", "Download")
            ),
            column(width=6,
                fileInput("uploadedCSV", "Choose a CSV file for upload (maximum 5MB)", multiple = FALSE, accept = c("test/csv", "text/comma-separated-values", "text/plain", ".csv"))
            ),
            fluidRow(column(12,
                p("The first five rows of your uploaded data are shown below.")
            ),
            column(width=10,
                tableOutput("uploaded")
            )),
            hr(),
            fluidRow(
                column(width=6, 
                    uiOutput("bad_dest"),
                    uiOutput("mult_dest")
                ),
                column(width=6,
                    uiOutput("bad_orig"),
                    uiOutput("mult_orig")
                )
            ),
            # uiOutput("origin_check"),
#            hr(),
#            textOutput("multi.cf"),
            hr(),
            fluidRow(
                column(8, align="center", 
                actionButton("runMultipleTravellers", "Calculate carbon footprint for multiple travellers", width="50%"),
                tags$head(
                            tags$style(HTML('#calc{background-color:Gray;
                                color: white}'))
                        ),
                plotOutput(outputId = "mappedOutput", width="100%", height="400px")
                )
            )
        ),
        tabPanel(
            "Modify carbon emission parameters",
            helpText("Carbon emissions (tonnes CO2) per km flying, driving, train or bus and emissions for a night in a hotel or average meal in a restaurant are taken from https://calculator.carbonfootprint.com/. You can modify these estimates for custom calculations."),
            hr(),
            h4("Transit (plane, car, and public transit)"),
            fluidRow(column(4, 
                h5("Plane travel"),
                numericInput("param_plane1", "CO2 emitted PER KM of plane travel", value=TESAcarbon::carbon.params$C.plane[1], min=0),
                numericInput("param_plane2", "Fixed CO2 emitted by plane travel (i.e. in addition to per-km emissions)", value=TESAcarbon::carbon.params$C.plane[2], min=0)
            ),
            column(4, 
                h5("Car travel"),
                numericInput("param_car1", "CO2 emitted per km of car travel", value=TESAcarbon::carbon.params$C.car[1], min=0),
                numericInput("param_car2", "Fixed CO2 emitted by car travel (i.e. in addition to per-km emissions)", value=TESAcarbon::carbon.params$C.car[2], min=0)
            ), 
            column(4,
                h5("Public transit"),
                numericInput("param_bustrain1", "CO2 emitted per km of bus/train travel", value=TESAcarbon::carbon.params$C.bustrain[1], min=0),
                numericInput("param_bustrain2", "Fixed CO2 emitted by bus/train travel", value=TESAcarbon::carbon.params$C.bustrain[2], min=0)
            )),
            hr(),
            h4("Room and board"),
            fluidRow(column(4, 
                h5("Hotel stay CO2"),
                numericInput("param_hotel1", "CO2 emitted PER NIGHT stayed in a hotel", value=TESAcarbon::carbon.params$C.hotel[1], min=0),
                numericInput("param_hotel2", "Fixed CO2 emissions for staying in a hotel (i.e. in addition to per-night CO2 emissions)", value=TESAcarbon::carbon.params$C.hotel[2], min=0)
            ), 
            column(4, 
                h5("Eating out CO2"),
                numericInput("param_meal1", "CO2 emitted PER MEAL", value=TESAcarbon::carbon.params$C.meal[1], min=0),
                numericInput("param_meal2", "Fixed CO2 emissions for eating out (i.e. in addition to per-meal emissions)", value=TESAcarbon::carbon.params$C.meal[2], min=0),
                numericInput("param_mealdiscount", "CO2 meal discounting (i.e. what proportion of CO2 from eating out would you have emitted anyway by eating at home?)", value=TESAcarbon::carbon.params$C.meal.discount, min=0, max=1)
            ))
            # actionButton("saveParams", "Save and use custom parameters")
        )
    )
)

# Server ------------------------------------------------------------------
server <- function(input, output){
    # To account for non-default parameters, copy the functions here:
    C.f= function(hotel.nights, plane.distance, bustrain.distance, car.distance, number.car.sharing, meals){
      number.car.sharing[number.car.sharing==0]=1
      c.hotel= carbon.params.mod()$C.hotel[1] * hotel.nights + carbon.params.mod()$C.hotel[2]
      c.plane= carbon.params.mod()$C.plane[1] * plane.distance + carbon.params.mod()$C.plane[2]
      c.bustrain= carbon.params.mod()$C.bustrain[1] * bustrain.distance + carbon.params.mod()$C.bustrain[2]
      c.car= (carbon.params.mod()$C.car[1] * car.distance + carbon.params.mod()$C.car[2]) / number.car.sharing
      c.meal= (carbon.params.mod()$C.meal[1] * meals + carbon.params.mod()$C.meal[2]) * carbon.params.mod()$C.meal.discount
      C.total= c.plane + c.car + c.hotel + c.meal
      C.total
    }


    # Local copy of the carbon footprint calculator, new version includes "localization" as an input
    carbon.footprint.fmod= function(input, localisation, Title.name="Carbon footprint", list.out=T){
      ## localisation= distance.lookup.f(input)
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

    # observeEvent(input$saveParams, {
    carbon.params.mod <<- reactive({
        list(
            "C.plane" = c(input$param_plane1, input$param_plane2),
            "C.car" = c(input$param_car1, input$param_car2),
            "C.bustrain" = c(input$param_bustrain1, input$param_bustrain2),
            "C.hotel" = c(input$param_hotel1, input$param_hotel2),
            "C.meal" = c(input$param_meal1, input$param_meal2),
            "C.meal.discount" = input$param_mealdiscount
        )
    })
    # })

    # First panel: individual calculation
    observeEvent(input$calc, {
        # Do some simple replacements to remove "km" and "kilometers" instances
        if(is.na(input$plane.distance)){
            updateTextInput(session, "plane.distance", value=0)
        }
        if(is.na(input$bustrain.distance)){
            updateTextInput(session, "bustrain.distance", value=0)
        }
        if(is.na(input$car.distance)){
            updateTextInput(session, "car.distance", value=0)
        }
        if(is.na(input$hotel.nights)){
            updateTextInput(session, "hotel.nights", value=0)
        }
        if(is.na(input$meals)){
            updateTextInput(session, "meals", value=0)
        }

        plane.distance <- as.numeric(gsub(".*?([0-9]+).*", "\\1", input$plane.distance))
        bustrain.distance <- as.numeric(gsub(".*?([0-9]+).*", "\\1", input$bustrain.distance)) # as.numeric(gsub("kilometers", "", x=gsub(as.character("km", "", x=input$bustrain.distance))))
        car.distance <- as.numeric(gsub(".*?([0-9]+).*", "\\1", input$car.distance)) # as.numeric(gsub("kilometers", "", x=gsub(as.character("km", "", x=input$car.distance))))
        hotel.nights <- as.numeric(gsub(".*?([0-9]+).*", "\\1", input$hotel.nights)) # as.numeric(as.character(gsub("nights", "", x=input$hotel.hights)))
        meals <- as.numeric(gsub(".*?([0-9]+).*", "\\1", input$meals))# as.numeric(as.character(gsub("meals", "", x=input$meals)))
        
        # print(paste0("Plane: ", plane.distance))
        # print(paste0("Bus: ", bustrain.distance))
        # print(paste0("Car: ", car.distance))
        # print(paste0("Hotel: ", hotel.nights))
        # print(paste0("Meals: ", meals))

        calculated_cf <- C.f(
            hotel.nights=ifelse(is.na(hotel.nights), 0, hotel.nights),
            plane.distance=ifelse(is.na(plane.distance), 0, plane.distance),
            bustrain.distance=ifelse(is.na(bustrain.distance), 0, bustrain.distance),
            car.distance=ifelse(is.na(car.distance), 0, car.distance),
            number.car.sharing=input$number.car.sharing,
            meals=ifelse(is.na(meals), 0, meals)
        )
        output$cf.hotel <- renderText(paste0("Carbon footprint for hotel: ", 
            signif(carbon.params.mod()$C.hotel[1] * hotel.nights + carbon.params.mod()$C.hotel[2], 3),
            " tonnes"))
        output$cf.plane <- renderText(paste0("Carbon footprint for airplane travel: ", 
            signif(carbon.params.mod()$C.plane[1] * plane.distance + carbon.params.mod()$C.plane[2], 3),
            " tonnes"))
        output$cf.bustrain <- renderText(paste0("Carbon footprint for bus and train travel: ", 
            signif(carbon.params.mod()$C.bustrain[1] * bustrain.distance + carbon.params.mod()$C.bustrain[2], 3),
            " tonnes"))
        output$cf.car <- renderText(paste0("Carbon footprint for car travel (per passenger): ", 
            signif((carbon.params.mod()$C.car[1] * car.distance + carbon.params.mod()$C.car[2]) / input$number.car.sharing, 3),
            " tonnes"))
        output$cf.meal <- renderText(paste0("Carbon footprint for meals: ", 
            signif((carbon.params.mod()$C.meal[1] * meals + carbon.params.mod()$C.meal[2]) * carbon.params.mod()$C.meal.discount, 3), 
            " tonnes"))
        output$cf <- renderText(paste0("Total estimated carbon footprint: ", signif(calculated_cf, 3), " tonnes"))
    })

    ## Second panel: 
    ## CSV reader and multi-person app

    # Provide ICES data as a template
    template_df <<- data.frame(TESAcarbon::ICES)
    output$downloadTemplate <- downloadHandler(
        # Returns a string which indicates what name to use when saving the file
        filename = "TESACarbonCalculator_multiple_traveller_template.csv",
        content = function(file) {
            write.table(template_df, file, sep = ",", row.names = FALSE)
        }
    )
    # Uploading data
    upload.df <<- reactive({
        req(input$uploadedCSV)
        df <- read.csv(input$uploadedCSV$datapath,
            header=TRUE,
            sep=","
        )
    })
    # File upload 
    output$uploaded <- renderTable({
        req(input$uploadedCSV)
        return(head(upload.df()))
    })

    observeEvent(upload.df(), {
        # If there is a file uploaded:
        if(!is.null(nrow(upload.df()))){
            fixed_df <<- upload.df()
            fixed_df$citycountry <- paste0(fixed_df$origin, fixed_df$origin.country)
            origin.vector <- unique(fixed_df$citycountry)
            destination.vector <- unique(fixed_df$destination)
            # Track multiples and missing cities
            bad_origin <<- vector()
            multiple_origin <<- vector()
            # Track which rows in world.cities are representative
            fixed_df$origin_row <- NaN
            fixed_df$destination_row <- NaN

            # Same for destination cities
            multiple_destination <<- vector()
            bad_destination <<- vector()
            
            # lookup distances between locations
            cities <- as.data.table(maps::world.cities)[country.etc %in% unique(fixed_df$origin.country)]
            cities$citycountry <- paste0(cities$name,cities$country.etc)
            for(cc in origin.vector){
                origin_present <- which(cities$citycountry == cc)
                if(length(origin_present) == 0){
                    bad_origin <- append(bad_origin, cc)
                } else if(length(origin_present) > 1){
                    multiple_origin <- append(multiple_origin, cc)
                } else {
                    fixed_df$origin_row[which(fixed_df$citycountry == cc)] <- origin_present
                }
            }
            for(dest in destination.vector){
                destination_present <- which(maps::world.cities == dest)
                if(length(destination_present) == 0){
                    bad_destination <- append(bad_destination, dest)
                } else if(length(destination_present) > 1){
                    multiple_destination <- append(multiple_destination, dest)
                } else {
                    fixed_df$destination_row[which(fixed_df$destination == dest)] <- destination_present
                }
            }
            # re-establish global data frame
            fixed_df <<- fixed_df
            # now, if any of those are duplicates, have the user input and fix
            if(length(multiple_destination) > 0){
                print(paste0("...Length of multiple destination: ", length(multiple_destination)))
                output$mult_dest <- renderUI({
                    lapply(1:length(multiple_destination), function(val) {
                        test <- data.frame(world.cities[which(maps::world.cities == multiple_destination[val]),c("name", "country.etc","lat","long")])
                        test$choices <- paste0(test$name, ", ", test$country.etc, " (lat: ",test$lat, ", lon:", test$long, ")")
                        choice_list <- list(rownames(test))[[1]]
                        names(choice_list) <- test$choices
                        fluidRow(column(12,
                            selectInput(inputId=paste0("dest_", val), label=paste0("The destination city ", multiple_destination[val], " matches multiple cities. Please select the correct one:"), choices=choice_list, selected="select below...")
                        ))
                    })
                })
            }
            if(length(bad_destination) > 0){
                output$bad_dest <- renderUI({
                    lapply(1:length(bad_destination), function(val) {
                        h4(paste0("Destination ", bad_destination[val], " matches no known cities. Please verify that you are using English spelling. If the city has less than 1000 people, it is unlikely to be in the database: please choose a larger city that is close by."))
                    })
                })
            }
            # Check for multiple origins, too
            if(length(multiple_origin) > 0){
                output$mult_orig <- renderUI({
                    lapply(1:length(multiple_origin), function(val) {
                        test <- data.frame(world.cities[which(paste0(maps::world.cities$name, maps::world.cities$country.etc) == multiple_origin[val]),c("name", "country.etc","lat","long")])
                        test$choices <- paste0(test$name, ", ", test$country.etc, " (lat: ",test$lat, ", lon:", test$long, ")")
                        choice_list <- list(rownames(test))[[1]]
                        names(choice_list) <- test$choices
                        fluidRow(column(12,
                            selectInput(inputId=paste0("origin_", val), label=paste0("The origin city ", multiple_origin[val], " matches multiple cities. Please select the correct one:"), choices=choice_list, selected="select below...")
                        ))
                    })
                })
            }
            if(length(bad_origin) > 0){
                output$bad_orig <- renderUI({
                    lapply(1:length(bad_origin), function(val) {
                        h4(paste0("Origin ", bad_origin[val], " matches no known cities. Please verify that you are using English spelling. If the city has less than 1000 people, it is unlikely to be in the database: please choose a larger city that is close by."))
                    })
                })
            }
        }
    })

    ### Then run localization, etc. with the revised `fixed_df`
    observeEvent(input$runMultipleTravellers, {
        # print("Fixed DF:")
        # print(fixed_df)
        for(m in 1:length(multiple_destination)){
            req(parse(text=paste0("input$dest_",m)))
            new_dest <- eval(parse(text=paste0("input$dest_",m)))
            # print(paste0("New dest: ", new_dest))
            orig_dest <- maps::world.cities[new_dest, "name"]
            # 
            # print(paste0("Orig dest: ", orig_dest))
            # print("Which rows?")
            # print(fixed_df$destination_row[which(fixed_df$destination == orig_dest)])
            fixed_df$destination_row[which(fixed_df$destination == orig_dest)] <- new_dest
        }
        for(m in 1:length(multiple_origin)){
                        req(parse(text=paste0("input$origin_",m)))
            new_origin <- eval(parse(text=paste0("input$origin_",m)))
            # print(paste0("New origin: ", new_origin))
            orig_origin <- maps::world.cities[new_origin, "name"]
            # 
            # print(paste0("Orig dest: ", orig_origin))
            # print("Which rows?")
            # print(fixed_df$origin_row[which(fixed_df$origin == orig_origin)])
            fixed_df$origin_row[which(fixed_df$origin == orig_origin)] <- new_origin
        }
        # print("Fixed DF2:")
        # print(fixed_df)
        ### Origin locations
        fixed_df$lat.origin <- maps::world.cities[fixed_df$origin_row,]$lat
        fixed_df$long.origin <- maps::world.cities[fixed_df$origin_row,]$long
        fixed_df$lat.dest <- maps::world.cities[fixed_df$destination_row,]$lat
        fixed_df$long.dest <- maps::world.cities[fixed_df$destination_row,]$long
        # dest_locations <- maps::world.cities[unique(fixed_df$destination_row),]
        fixed_df$distances <- round(distGeo(fixed_df[,c("long.origin","lat.origin")],
                                      fixed_df[,c("long.dest", "lat.dest")]
        )/1000, 0)*fixed_df$flying
        # print(fixed_df$distances)
        # Run carbon footprint
        origin.locations <- as.data.table(maps::world.cities[unique(fixed_df$origin_row),])
        origin.locations$citycountry <- paste0(origin.locations$name, origin.locations$country.etc)
        destination.locations <- as.data.table(maps::world.cities[unique(fixed_df$destination_row),])
        destination.locations$citycountry <- paste0(destination.locations$name, destination.locations$country.etc)
        localisation <<- list(distance=fixed_df$distances, origin.locations=origin.locations,
          destination.locations=destination.locations)
        output$mappedOutput <- renderPlot(carbon.footprint.fmod(as.data.table(fixed_df), localisation))
    })
}

shiny::shinyApp(ui = ui, server = server)