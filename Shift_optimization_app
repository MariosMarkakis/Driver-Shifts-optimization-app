# if pacman (package manager) is not in your system, this way it will be downloaded on behalf
if (!require("pacman")) install.packages("pacman")
# load pacman, and it will do the rest
library(pacman)
# install and/or load all necessary libraries
pacman::p_load(shiny,data.table,dplyr,janitor
               ,sqldf,stringi,shinyWidgets
               ,shinydashboard,shinycssloaders,xlsx,DT,openxlsx
               ,ROI,ROI.plugin.glpk,ROI.plugin.symphony,ompr,ompr.roi,gtools,tidyr,beepr,reshape
               ,highcharter,stringr,purrr)

#   rm(list = ls())
#   .rs.restartR()

#creating the ui of the app
ui <- 
  dashboardPage(
    dashboardHeader(title = "Loonshot optimization tool",titleWidth = 300), #dashboard title
    dashboardSidebar(width = 300, #dashboard sidebar 
                     sidebarMenu( #sidebar options
                       br(), #blank row
                       menuItem("Vehicle Scheduling module",tabName = "vehicle_allocation",icon = icon("car")), # first feature - find minimum required drivers that satisfy a given vehicle schedule
                       menuItem("Driver Planning module",tabName = "num_drivers_optimization",icon = icon("user")), # first feature - find minimum required drivers that satisfy a given vehicle schedule
                       menuItem("Driver Shift Scheduling module",tabName = "driver_allocation",icon = icon("bar-chart-o")) # second feature - allocate the drivers of your input to shifts
                       )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "shift_vehicle_generator",            
                tabsetPanel( #create 2 tabs (1 for prius one for EV) in the dashboard of the first feature (driver optimization) 
                  tabPanel("Prius Vehicles"),
                  tabPanel("EV Vehicles")
                )
        ),
        tabItem(tabName = "num_drivers_optimization",
                sidebarLayout( 
                  sidebarPanel(
                    #upload the shift input in xlsx format
                    fileInput("shifts_data",label = tags$div(HTML('<i class="fa fa-cloud-upload fa-1x" aria-hidden="true" style = "color:#23d2aa;"></i> Import Prius & EV Schedule ',accept = ".xlsx"))),
                    uiOutput("shift_durations_ev"), #here we insert a dynamically created ui input where the user inserts the duration of EV shiftS
                    br(),
                    radioButtons("same_shift_week", "Driver has the same shift per week", c("Yes","No"), inline=TRUE), #driver has the same shift across the week
                    sliderInput("min_max_days_per_week", "Min and Max days per week the driver is allowed to work",1, 7, c(1,6), step = 1), #minimum and maximum days that the driver should work per week
                    sliderInput("min_max_hours_per_week", "Min and Max hours per week the driver is allowed to work",1, 70, c(8,48), step = 1), #minimum and maximum hours that the driver should work per week
                    tags$b("Generate pseudo-driver schedule"),
                    switchInput(inputId = "show_schedule",value=FALSE),
                    # radioButtons("show_schedule","Generate pseudo-driver schedule", c("Yes","No"), selected = "No" ,inline=TRUE), #driver has the same shift across the week
                    menuItem("Advanced options",startExpanded = TRUE,
                             #radioButtons("solver", "Select solver", c("symphony","glpk"), inline=TRUE,selected = "symphony"), #select the solver to solve the problem
                             radioButtons("first_solution", "Stop when first feasible solution found", c("Yes","No"), inline=TRUE, selected = "No"), # give me the first solution you find. if no go to time limit
                             conditionalPanel( condition = "input.first_solution == 'No'",
                                               numericInput("time_limit", "Time Out Limit in seconds:" , 180,min=1,max=100000)) #if first_solution is 'no' then you can define the time limit
                    ),
                    #go button to start running the Prius algo for Prius
                    actionButton("go","Find minimum required Drivers",icon = icon("play-circle"),width = "100%", style="color: #333333; background-color: #23d2aa; border-color: #007bb8;font-weight: bold;")
                    ,width = 3),
                  mainPanel(
                    fluidPage(
                      fluidRow(
                        valueBoxOutput("num_periods",width = 6), #how many periods the input file includes 
                        valueBoxOutput("num_shifts",width = 6) %>% withSpinner(type=6,color="#23d2aa") #how many shifts are included in input file
                      ),
                      fluidRow(
                        infoBoxOutput("drivers_utilized",width = 12) #how many drivers are required to satisfy the prius vehicle schedule
                      ),
                      # 2 tables: 1 is the input vehicle schedule and the second is the optimization output - driver schedule
                      DTOutput('table1'), #Input vehicle schedule is presented in a data table to the user
                      br(),
                      fluidRow(splitLayout(cellWidths = c("50%","50%"), 
                                           highchartOutput("driver_utilization_days_graph"), #graph with the prius driver days utilization
                                           highchartOutput("driver_utilization_hours_graph") #graph with required drivers per shift
                      )),
                      uiOutput("download_results_module2"),                    
                      DTOutput('table2'),
                      uiOutput("download_pseudo_1")
                    )
                    ,width=9)
                )
        ),
        tabItem(tabName = "driver_allocation",            
                tabsetPanel( #create 2 tabs (1 for prius one for EV) in the dashboard of the first feature (driver optimization) 
                  tabPanel("Preferences",
                           sidebarLayout(
                             sidebarPanel(
                               #upload the shift input in xlsx format
                               fileInput("shifts_data2",label = tags$div(HTML('<i class="fa fa-cloud-upload fa-1x" aria-hidden="true" style = "color:#23d2aa;"></i> Import Prius & EV Schedule ',accept = ".xlsx"))),
                               uiOutput("shift_durations_ev2"), #here we insert a dynamically created ui input where the user inserts the duration of EV shiftS
                               br(),
                               fileInput("driver_data2",label = tags$div(HTML('<i class="fa fa-cloud-upload fa-1x" aria-hidden="true" style = "color:#23d2aa;"></i> Import Driver Data ',accept = ".xlsx"))),
                               radioButtons("get_vehicle_help2", "Help me create the vehicle constraints file input", c("Yes","No"), selected="No", inline=TRUE), #select if you want to automatically generate vehicle constraints file
                               conditionalPanel(condition = "input.get_vehicle_help2 == 'Yes'",
                                  fileInput("vehicle_input_help2",label = tags$div(HTML('<i class="fa fa-cloud-upload fa-1x" aria-hidden="true" style = "color:#23d2aa;"></i> Import Vehicle Plates/Ids ',accept = ".xlsx"))),
                                  uiOutput("download_vehicle_help3"),
                                  br()
                               ),
                               fileInput("vehicle_input2",label = tags$div(HTML('<i class="fa fa-cloud-upload fa-1x" aria-hidden="true" style = "color:#23d2aa;"></i> Import Vehicle Availability ',accept = ".xlsx"))),
                               radioButtons("same_shift_week2", "Driver has the same shift per week", c("Yes","No"), inline=TRUE), #driver has the same shift across the week
                               menuItem("Fairness options (applicable only for 4 weeks schedules)",startExpanded = TRUE,
                                        radioButtons("apply_fairness2", "Apply Fairness", c("Yes","No"), inline=TRUE, selected = "Yes"), # give me the first solution you find. if no go to time limit
                                        conditionalPanel(condition = "input.apply_fairness2 == 'Yes'",
                                                         sliderInput("num_iter2", "Number of Fairness iterations:" , 100,10000,1000,step=100)) #if first_solution is 'no' then you can define the time limit

                               ),
                               #go button to start running the Prius algo for Prius
                               actionButton("go2","Allocate Drivers to shifts",icon = icon("play-circle"),width = "100%", style="color: #333333; background-color: #23d2aa; border-color: #007bb8;font-weight: bold;")
                               ,width = 3),
                             mainPanel(
                               fluidPage(
                                 fluidRow(
                                   valueBoxOutput("num_periods2",width = 4), #how many periods the input file includes 
                                   valueBoxOutput("num_shifts2",width = 4) %>% withSpinner(type=6,color="#23d2aa"), #how many shifts are included in input file
                                   infoBoxOutput("drivers_imported2",width = 4) #how many drivers are required to satisfy the vehicle schedule
                                 ),
                                 fluidRow(
                                   infoBoxOutput("drivers_utilized2",width = 12) #how many drivers were imported
                                 ),
                                 # 2 tables: 1 is the input vehicle schedule and the second is the optimization output - driver schedule
                                 DTOutput('table1_2'), #Input vehicle schedule is presented in a data table to the user
                                 br(),
                                 fluidRow(splitLayout(cellWidths = c("50%","50%"), 
                                                      highchartOutput("driver_utilization_days_graph2"), #graph with the prius driver days utilization
                                                      highchartOutput("driver_utilization_hours_graph2") #graph with required drivers per shift
                                 )),
                                 uiOutput("download_results_module3")
                               )
                               ,width=9)
                           )
                  ),
                  tabPanel("Driver Schedule",
                           br(),
                           DTOutput('table2_2'),
                           uiOutput("download_schedule_2")
                  )
                )
        )
      )
    ),
    
    # html tags to customize colors and hover reactions
    tags$head(tags$style(HTML("
        section.sidebar .shiny-input-container {
        padding: 1px 15px 0px 15px;
        white-space: normal;
        }
        
        .tabbable > .nav > li > a[data-value='Preferences'] {background-color: #333333;   color:white}
        .tabbable > .nav > li > a[data-value='Driver Schedule'] {background-color: #333333;  color:white}
        .tabbable > .nav > li.active > a[data-value='Preferences'] {background-color: #23d2aa;  color:black}
        .tabbable > .nav > li.active > a[data-value='Driver Schedule'] {background-color: #23d2aa;  color:black}
        
        .small-box.bg-yellow { background-color: #6f6987 !important; color: #fff !important; }
        .small-box.bg-aqua { background-color: #6f6987 !important; color: #fff !important; }
        .small-box.bg-blue { background-color: #6f6987 !important; color: #fff !important; }
        
        
        .shiny-notification{
          position: fixed;
          top: 10%;
          right: 0;
          width: 400px;
        }


        .info-box-text {
          font-size: 22px;
          }
          
        .info-box-number {
        font-size: 22px;
        }
        
        .info-box-icon.bg-green {
        color: 23d2aa !important;
        background-color: #23d2aa !important;
        }
    
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #333333;
                              color:#ffffff;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #333333;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #333333;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #23d2aa;
                              color: #333333;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #333333;
                              color: #ffffff;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #23d2aa;
                              color: #ffffff;
         }
         
         /* make input corners round instead of square */
          .form-control {border-radius: 4px;}
          
          
        ")))
  )

server = function(input, output,session) {

  ########################################################################
  ########## first feature outputs (Driver planning module) ##############
  ########################################################################
  
  #Vehicle schedule file input imported by the user
  Shifts_data<-eventReactive(input$shifts_data,{
    data0 <- as.data.frame(openxlsx::read.xlsx(input$shifts_data$datapath,1))
    data0$date<-excel_numeric_to_date(data0$date)
    return(data0)
  })
  
  #create the dynamic UI of ev vehicles where the user selects the duration of each shift
  output$shift_durations_ev <-
    renderUI({
      dd <- Shifts_data()
      if (max(dd$vehicle)>=2) {# activate the dynamic ui if more than 1 vehicle is imported
        shifts_columns <- colnames(dd[,grepl("SHIFT",toupper(names(dd)))]) #keep only columns that indicate shifts
        lapply(1:length(shifts_columns), function(i) { #create as many numericinputs as there are shifts in input file
          fluidRow( #put the lebal next to inputbox not below for saving space
            tags$head(
              tags$style(type="text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: middle;}
                #inline .form-group { display: table-row;}")
            ),
            column(1),
            tags$div(id = "inline", numericInput(paste0("shift_",i,"_duration_ev"), label = paste0("EV Shift Duration (excl. charging), shift ",i,":"), 8,min=1,max=8))
          )
        })
      }
    })
  
  #Value box with number of periods in dataset
  output$num_periods <- renderValueBox({
    req(results())
    data1<-Shifts_data()
    valueBox(paste0("Period: ",as.numeric(length(unique(data1$date)))), subtitle = "Days",
             icon = icon("calendar-alt"),color='yellow')
  })
  
  #Value box with number of shifts in dataset
  output$num_shifts <- renderValueBox({
    req(results())
    data2<-Shifts_data()
    valueBox(paste0("Scheduled: ", as.numeric(length(names(data2[ , grepl( "SHIFT" , toupper(names(data2)))]))) ), subtitle = " Shifts",
             icon = icon("rocket"),color='aqua')
  })
  
  #Info box with required drivers
  output$drivers_utilized<- renderInfoBox({
    req(results())
    model=model()
    solution=solution()
    DerivedDrivers = data.table(get_solution(model, x[i]))[value == 1]
    if (nrow(DerivedDrivers)>1) {
      infoBox(paste0(solution, " Solution Found!"), paste0(nrow(DerivedDrivers), " drivers are required to satisfy the imported vehicle schedule"), icon = icon("thumbs-up"),color = "green")
    } else {infoBox("No feasible Solution Found!", paste0("Try changing the preferences or the vehicle schedule"), icon = icon("times"),color = "red")
    }
  })
  
  #vehicle input table transposed for reporting reasons
  tabl1 <- eventReactive(input$go,{
    d1<- as.data.table(Shifts_data())
    d2 <- melt(d1,id.vars = c("date","vehicle")) #transposing for better visual
    d3 <- reshape(d2,timevar = "date",idvar = c("variable","vehicle"), direction = "wide")
    setnames(d3,"variable","shift")
    setnames(d3, names(d3), gsub("value.", "", names(d3)))
    d3 <- d3[order(rank(vehicle,shift))] #ordering for better visual
    d3$vehicle[d3$vehicle==1] <- "Prius" #Replacing vehicle codes with names 
    d3$vehicle[d3$vehicle==2] <- "EV"
    return(d3)
  })
  
  #table output of vehicle schedule input data
  output$table1 <- renderDataTable({
    req(results())
    d3<-tabl1()
    DT::datatable(
      d3,
      selection = "single",
      filter = list(position = 'top', clear = FALSE),
      caption = 'Vehicle Schedule Imported',
      extensions = c('ColReorder', 'FixedHeader', 'Scroller','KeyTable'),
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        searching = T, #search the table
        searchHighlight = TRUE, #highlight the search in the table
        columnDefs = list(list(className = 'dt-center',targets="_all")), #allign column in center of cell
        #buttons = c('copy', 'excel', 'print'), #add download buttons
        colReorder = TRUE, # order the columns as you wish
        keys = TRUE, #navigate to table like excel
        scrollX = TRUE #scroll the table horizontically
      )
    )
  })
  
  #pseudo driver schedule table generation - transposing fof better visual
  table2 <- eventReactive(input$go,{
    req(results())
    d1<-results() #optimization results for EV drivers and vehicles
    #d1$date<-excel_numeric_to_date(d1$date)
    d1<-d1[,-c(2,6)] #removing day column keeping onlydate column
    d1$shift=paste0("shift ",d1$shift)
    d1<-reshape(d1,timevar="date",idvar=c("Driver","Vehicle"),direction="wide") #transposing for better visual
    d1$Vehicle[d1$Vehicle==1] <- "Prius" #Replacing vehicle codes with names 
    d1$Vehicle[d1$Vehicle==2] <- "EV"
    d1<-setorder(d1,Driver,-Vehicle)
    names(d1) <- gsub(names(d1), pattern = "shift.", replacement = "") #fix column names - only dates  
    return(d1)    
  })
  
  #table output of vehicle schedule input data
  output$table2 <- renderDataTable({
    req(input$show_schedule == TRUE)
    req(table2())
    d1 <- table2()
    DT::datatable(
      d1,
      selection = "single",
      filter = list(position = 'top', clear = FALSE),
      caption = 'Driver Schedule Generated',
      extensions = c('ColReorder', 'FixedHeader', 'Scroller','KeyTable'),
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        searching = T, #search the table
        searchHighlight = TRUE, #highlight the search in the table
        columnDefs = list(list(className = 'dt-center',targets="_all")), #allign column in center of cell
        #buttons = c('copy', 'excel', 'print'), #add download buttons
        colReorder = TRUE, # order the columns as you wish
        keys = TRUE, #navigate to table like excel
        scrollX = TRUE #scroll the table horizontically
      )
    )  %>% formatStyle( #shift conditional coloring in the final schedule - maximum 6 shifts, have to make it dynamic
      names(d1),
      backgroundColor = styleEqual(c("shift 1","shift 2","shift 3","shift 4","shift 5","shift 6"), c('#23d2aa', '#9e9bae','#ff6600','#6f6987','green','blue'))
    )
  })
  
  #download the above table 2
  output$download_pseudo <- downloadHandler(
    filename = function() {paste0("Driver Schedule",Sys.time(),".xlsx")},
    content = function(file) {
      write.xlsx(results(),file,row.names = FALSE)
    }
  )
  
  #make the download button appear when results are generated
  output$download_pseudo_1<- renderUI({
    req(input$show_schedule == TRUE)
    req(results())
    downloadButton("download_pseudo",'Download Driver Schedule')
  })
  
  #create the first excel tab of results output 
  out_tab_1 <- eventReactive(input$go,{
    req(results())
    model=model()
    solution=solution()
    data = Shifts_data()
    Days = as.numeric(length(unique(data$date))) #getting number of days
    Shifts = as.numeric( length(names (data[ , grepl( "SHIFT",toupper(names(data)))] )) ) #getting number of shifts
    Vehicles = length(unique(data$vehicle))
    DerivedDrivers = data.table(get_solution(model, x[i]))[value == 1]
    d3 <- data.frame( minimum_days = input$min_max_days_per_week[1]
                      ,maximum_days = input$min_max_days_per_week[2]
                      ,mininum_hours = input$min_max_hours_per_week[1]
                      ,maximum_hours = input$min_max_hours_per_week[2]
                      ,same_shift_per_week = input$same_shift_week
                      ,number_of_shifts = Shifts
                      ,number_of_days = Days
                      ,number_of_vehicles = Vehicles
                      ,solution_found = solution
                      ,required_drivers = nrow(DerivedDrivers)
    )
    d3<-t(d3)
    colnames(d3)[1] <- "Value"
    return(d3)
  })
  
  #create the third excel tab of results output (driver day utilization)
  out_tab_3 <- eventReactive(input$go,{
    req(results)
    model=model()
    DerivedDrivers = data.table(get_solution(model, x[i]))[value == 1]
    if (nrow(DerivedDrivers)>=1) {
      final_shifts <- results()
      #buidling the chart data
      data <- data.frame(sqldf("select days||' days' as Working_days ,count(*) as Drivers from (
                                  select Driver,count(distinct day) as days from final_shifts group by 1)
                                  group by 1 order by 1"))
      return(data)
    } else {return(NULL)}
  })
  
  #create the forth excel tab of results output (driver hour utilization)  
  out_tab_4 <- eventReactive(input$go,{
    req(results)
    model=model()
    DerivedDrivers = data.table(get_solution(model, x[i]))[value == 1]
    if (nrow(DerivedDrivers)>=1) {
      final_shifts <- results()
      
      data <- data.frame(sqldf("select hours||' hours' as Working_hours ,count(*) as Drivers from (
                          select Driver,sum(shift_hours) as hours from final_shifts group by 1)
                          group by 1 order by 1 desc"))
      
      return(data)
    } else {return(NULL)}
  })
  
  #download the unified module 2 results in multiple tabs
  output$download_results_module2_2 <- downloadHandler(
    filename = function() {paste0("Results run_",Sys.time(),".xlsx")},
    content = function(file) {
      xlsx::write.xlsx(out_tab_1(),file,sheetName = "Results" ,row.names = TRUE) #parameters and optimization output
      xlsx::write.xlsx(tabl1(),file,sheetName = "Vehicle schedule imported" ,append = TRUE ,row.names = FALSE) #imported vehicle schedule
      xlsx::write.xlsx(out_tab_3(),file,sheetName = "Driver Day Utilization" ,append = TRUE ,row.names = FALSE) #driver day utilization
      xlsx::write.xlsx(out_tab_4(),file,sheetName = "Driver Hour Utilization" ,append = TRUE ,row.names = FALSE)#driver hour utilization
    }
  )
  
  #make the unified module 2 results download button appear when prius results are generated
  output$download_results_module2<- renderUI({
    req(results())
    downloadButton("download_results_module2_2",'Download Results')
  })
  
  #unifying shift hours prius and ev in a data table to use later
  shifthours<-eventReactive(input$go,{
    data<-Shifts_data()
    Shifts = as.numeric( length(names (data[ , grepl( "SHIFT",toupper(names(data)))] )) ) #getting number of shifts
    shifthours_prius=c()
    for (i in 1:Shifts) {
      shifthours_prius[i]=8
    }
    shifthours_ev = c()
    for (i in 1:Shifts) {
      input_name=paste0("shift_",i,"_duration_ev")
      shifthours_ev[i]=input[[input_name]]
    }
    shifthours=data.table(cbind(shifthours_prius,shifthours_ev))
    shifthours$shift<-row.names(shifthours)
    return(shifthours)
  })
  
  #here we build and customize the model for the given vehicles schedule
  model<-eventReactive(input$go ,{
    data<-Shifts_data()
    shifthours<-shifthours()
    GetWeekRequests <- function(j,k,l) {mapply(function(D,S,V) {RequestsDF[day == D & shift == S & vehicle == V]$total},j,k,l)} # function to use in model to get rid of ordering data
    Shifts = as.numeric( length(names (data[ , grepl( "SHIFT",toupper(names(data)))] )) ) #getting number of shifts
    shifthours_prius = shifthours[,shifthours_prius]
    shifthours_ev = shifthours[,shifthours_ev]

    # Days of planning ahead
    Days = as.numeric(length(unique(data$date)))
    
    # making the optimization dataset
    RequestsDT = as.data.table(data)
    RequestsDT <- sqldf("select a.*,b.day from RequestsDT a
                    inner join (select date,rank() over(order by date) day from (select distinct date as date from RequestsDT order by 1)) b
                    on a.date=b.date")
    RequestsDF = melt(RequestsDT,id.vars = c("date","vehicle","day"))
    setnames(RequestsDF,c("variable","value"),c("shift","total"))
    RequestsDF$shift = as.integer(gsub('shift_', '', RequestsDF$shift))
    RequestsDF <- setorder(RequestsDF,vehicle,shift,day)
    RequestsDF = as.data.frame(RequestsDF)

    #number of drivers to initiate the optimization -> 2 * max daily vehicles if maximum allowed working days >=4 else 4 * max
    sumveh=data.frame(date=data[,1], Sum1=rowSums(data[,3:ncol(data)]))
    sumveh2=sqldf("select date,sum(sum1) as sum_vehicles from sumveh group by 1")
    if (input$min_max_days_per_week[2]>=4) {Drivers = 2*max(sumveh2[2])} else {Drivers = 4*max(sumveh2[2])}
    
    # Create and solve optimization model, which consists of, decision variables, Objective and constraints
    model = MILPModel() %>%  
      # Driver decision variable
      add_variable(x[i],     i = 1:Drivers,type = "binary") %>%
      # Driver - Shift decision variable
      add_variable(z[i,j,k,l], i = 1:Drivers, j = 1:7, k = 1:Shifts, l = 1:2, type = "binary")%>%
      # Driver - Day decision variable
      add_variable(zd[i,j],  i = 1:Drivers, j = 1:7, type = "binary")%>%
      # One Shift per Day 
      add_constraint(sum_expr(z[i,j,k,l], k = 1:Shifts,l = 1:2) == zd[i,j], i = 1:Drivers,j = 1:7) %>%
      # One Vehicle per Shift
      add_constraint(z[i,j,k,1] + z[i,j,k,2] <= 1, i = 1:Drivers,j = 1:7,k = 1:Shifts) %>%
      # Minimum Working Days
      add_constraint(sum_expr(zd[i,j], j = 1:7) >= x[i]*input$min_max_days_per_week[1], i = 1:Drivers)%>%
      # Maximum working Days 
      add_constraint(sum_expr(zd[i,j], j = 1:7) <= x[i]*input$min_max_days_per_week[2], i = 1:Drivers)%>%
      # Minimum Working Hours
      add_constraint(colwise(rep(c(shifthours_prius,shifthours_ev),each = 7)) * sum_expr(z[i,j,k,l],j = 1:7,k = 1:Shifts,l=1:2)  >=  x[i]*input$min_max_hours_per_week[1] ,i = 1:Drivers)%>%
      # Maximum Working Hours
      add_constraint(colwise(rep(c(shifthours_prius,shifthours_ev),each = 7)) * sum_expr(z[i,j,k,l],j = 1:7,k = 1:Shifts,l=1:2)  <=  x[i]*input$min_max_hours_per_week[2] ,i = 1:Drivers)%>%
      # comply with shift planning
      add_constraint(sum_expr(z[i,j,k,l], i = 1:Drivers) == RequestsDF[RequestsDF$day %in% j & RequestsDF$shift %in% k  & RequestsDF$vehicle %in% l,]$total, j = 1:7, k = 1:Shifts, l = 1:2)
      # add_constraint(sum_expr(z[i,j,k,l], i = 1:Drivers) == GetWeekRequests(j,k,l),j = 1:7, k = 1:Shifts,l = 1:2)
    
    # same shift for both FT & PT
    if (input$same_shift_week=="Yes") {
      model = model %>%
        # Driver - week - Shift utilization decision variable for all drivers
        add_variable(b[i,k],   i = 1:Drivers, k = 1:Shifts,type = "binary")%>%
        # driver can be assigned to only one shift
        add_constraint(sum_expr(b[i,k], k = 1:Shifts) <= x[i], i = 1:Drivers) %>%
        # same shift across all days 
        add_constraint(z[i,j,k,l] <= b[i,k], i = 1:Drivers,j = 1:7, k = 1:Shifts,l = 1:2)
    } else {      
      model = model %>%
        # never assign adjacent shifts i.e night->morning for both FT & PT
        add_constraint(z[i,j-1,k+2,l] + z[i,j,k,ll]<=1,j = 2:7, i = 1:Drivers, k = 1,l=1:2,ll = 1:2)
    }
    
    # define objective function: Minimize number of drivers
    model = model %>%
      set_objective(sum_expr(x[i],i=1:Drivers),sense = "min")
    
    # solve the optimization problem based on advanced options selections (first feasible, solver, time_limit)
    if (input$first_solution=="No") {
      model = model %>%
        solve_model(with_ROI(solver = "symphony", control = list(verbosity = -1,time_limit = input$time_limit)) ) # time_limit = 100 or first_feasible = TRUE regardless of time execution
    } else {
      model = model %>%
        solve_model(with_ROI(solver = "symphony", control = list(verbosity = -1,first_feasible=TRUE)) )
    }
    
    beep(10) #play a sound to know that it is ready
    return(model)
  })
  
  solution <- eventReactive(input$go ,{
    model=model()
    DerivedDrivers = data.table(get_solution(model, x[i]))[value == 1]
    if (nrow(DerivedDrivers)==0) {sol_found="Infeasible"}
    else if (model$status=="optimal") {sol_found="Optimal"}
    else {sol_found="Feasible"}
    return(sol_found)
  })
  
  #generate results and output file
  results <- eventReactive(input$go ,{
    model=model()
    data<-Shifts_data()
    shifthours<-shifthours()
    DriverOut = data.table(get_solution(model, x[i]))[value == 1]
    
    RequestsDT = as.data.table(data)
    RequestsDT <- sqldf("select a.*,b.day from RequestsDT a
                    inner join (select date,rank() over(order by date) day
                    from (select distinct date as date from RequestsDT order by 1)) b
                    on a.date=b.date")
    
    # Shift appointments per driver per day
    ShiftPerDriver = data.table(get_solution(model, z[i,j,k,l]))[i %in% DriverOut$i & value==1]
    setnames(ShiftPerDriver,old = c("i","j","k","l"), new = c("Driver","Day","shift","Vehicle"))
    
    final_shifts <- sqldf("select b.date,a.Day,a.Driver,a.vehicle,a.shift
                      ,case when a.vehicle = 1 then c.shifthours_prius
                      when a.vehicle = 2 then c.shifthours_ev else null end as shift_hours
                      from ShiftPerDriver a 
                      left join (select distinct date,day from RequestsDT) b on a.day=b.day
                     left join shifthours c on a.shift=c.shift
                     order by driver,a.day")
    # return(ShiftPerDriver)
    return(final_shifts)
  })
  
  #bargraph with driver utilization
  output$driver_utilization_days_graph<-renderHighchart({
    model=model()
    DerivedDrivers = data.table(get_solution(model, x[i]))[value == 1]
    if (nrow(DerivedDrivers)>=1) {
      final_shifts <- results()
      #buidling the chart data
      data <- sqldf("select days||' days' as Working_days ,count(*) as Drivers from (
                                  select Driver,count(distinct day) as days from final_shifts group by 1)
                                  group by 1 order by 1")
      
      #creating the donut char chart
      highchart() %>%
        hc_title(text = "Driver - Day Utilization") %>%
        hc_chart(type = "pie") %>%
        hc_add_series_labels_values(data$Working_days,data$Drivers,name="Number of drivers",innerSize =  '50%'
                                    ,dataLabels = list(enabled = TRUE,format = '{point.name}: {point.percentage:.1f} %')) %>%
        # hc_plotOptions(pie = list(size = 400)) %>% #size of graph
        hc_colors(c('#ff6600','#23d2aa','#9e9bae','#6f6987')) #defining color options
    } else {return(NULL)}
  })
  
  #bargraph with required drivers per shift
  output$driver_utilization_hours_graph<-renderHighchart({
    model=model()
    DerivedDrivers = data.table(get_solution(model, x[i]))[value == 1]
    if (nrow(DerivedDrivers)>=1) {
      final_shifts <- results()
      
      data <- sqldf("select hours||' hours' as Working_hours ,count(*) as Drivers from (
                          select Driver,sum(shift_hours) as hours from final_shifts group by 1)
                          group by 1 order by 1 desc")
      
      #creating the bar char chart
      hchart(data, "column",name="Number of drivers", hcaes(x = Working_hours, y = Drivers),color='#ff6600',
             dataLabels = list(enabled = TRUE)) %>%  hc_title(text = "Driver Hour Utilization") #adding labels
    } else {return(NULL)}
  })
  
  ########################################################################
  ########## Module 3 outputs (Driver allocation module) #################
  ########################################################################
  
  #Vehicle schedule file input imported by the user
  Shifts_data2<-eventReactive(input$shifts_data2,{
    data0 <- as.data.frame(openxlsx::read.xlsx(input$shifts_data2$datapath,1))
    data0$date<-excel_numeric_to_date(data0$date)
    return(data0)
  })
  
  weeks2<-eventReactive(input$shifts_data2,{
    data<-Shifts_data2()
    if (length(unique(data$date))==7) {Weeks=1} else if (length(unique(data$date))==28) {Weeks=4
    } else {stopApp()}
    return(Weeks)
  })
  
  #driver file input (drivers available) imported by the user
  driver_data2<-eventReactive(input$go2,{
    data0 <- as.data.table(openxlsx::read.xlsx(input$driver_data2$datapath,1))
    data0<-setorder(data0,id_driver)
    data0$rn<-row.names(data0)
    return(data0)
  })
  
  #create the dynamic UI of ev vehicles where the user selects the duration of each shift
  output$shift_durations_ev2 <-
    renderUI({
      dd <- Shifts_data2()
      if (max(dd$vehicle)>=2) {# activate the dynamic ui if more than 1 vehicle is imported
        shifts_columns <- colnames(dd[,grepl("SHIFT",toupper(names(dd)))]) #keep only columns that indicate shifts
        lapply(1:length(shifts_columns), function(i) { #create as many numericinputs as there are shifts in input file
          fluidRow( #put the lebal next to inputbox not below for saving space
            tags$head(
              tags$style(type="text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: middle;}
                #inline .form-group { display: table-row;}")
            ),
            column(1),
            tags$div(id = "inline", numericInput(paste0("shift_",i,"_duration_ev2"), label = paste0("EV Shift Duration (excl. charging), shift ",i,":"), 8,min=1,max=8))
          )
        })
      }
    })
  
  #Vehicle constraints file input imported by the user
  vehicle_input2 <- eventReactive(input$vehicle_input2,{
    data0 <- data.table(openxlsx::read.xlsx(input$vehicle_input2$datapath,1))
    #melt the file to the required format
    data0 <- melt(data0, id = c("vehicle_id","plate","vehicle_type","shift"))
    colnames(data0)[c(5,6)]=c("date","availability") #renaming the melted columns
    data0$date<-as.numeric(as.character(data0$date)) #converting dates to characters and then to numeric for later converting to actual dates
    data0$date<-excel_numeric_to_date(data0$date)
    setorder(data0,vehicle_type,date,shift,vehicle_id)
    data0[, day := rleid(date), by = .(plate,shift)] #adding day and week in the dataset 
    data0[,week := ((day-1) %/% 7) + 1]  
    data0[,day := (day%%7) + (day%/%(week*7)*7)]
    return(data0)
  })
  
  #Vehicle help file to help the user create the final constraints file input
  vehicle_input_help2 <- eventReactive(input$vehicle_input_help2,{
    req(Shifts_data2())
    data <- as.data.frame(openxlsx::read.xlsx(input$shifts_data2$datapath,1)) #i m not using the already imported file because we need dates as excel numeric not as dates
    veh_input_help <- data.table(openxlsx::read.xlsx(input$vehicle_input_help2$datapath,1))
    unique_dates_number = length(unique(data$date))
    unique_dates = data.table(unique(data$date))
    colnames(veh_input_help)[c(1,2,3)]=c("vehicle_id","plate","vehicle_type")
    colnames(unique_dates)[1]<-"date"
    shifts_columns <- colnames(as.data.frame(data[,grepl("SHIFT",toupper(names(data)))])) #getting shift columns from data
    unique_shifts <- data.table(gsub("shift_","",shifts_columns))
    colnames(unique_shifts)[1]<-"shift"
    vehicle_input_template <- data.table(sqldf("select a.*,b.date,c.shift,1 as availability from veh_input_help a
                                           cross join unique_dates b
                                           cross join unique_shifts c"))
    vehicle_input_template$shift<-as.integer(vehicle_input_template$shift)
    #transposing to calendar view
    vehicle_input_template_calendar <- reshape(vehicle_input_template, timevar = "date",idvar = c("vehicle_id","plate","vehicle_type","shift"), direction = "wide")
    names(vehicle_input_template_calendar) <- gsub(names(vehicle_input_template_calendar), pattern = "availability.", replacement = "")  
    return(vehicle_input_template_calendar)
  })  
  
  #download the above vehicle input help template
  output$download_vehicle_help2 <- downloadHandler(
    filename = function() {paste0("Vehicle_input_template",Sys.time(),".xlsx")},
    content = function(file) {
      write.xlsx(vehicle_input_help2(),file,row.names = FALSE,check.names=F)
    }
  )
  
  #make the download button appear when results are generated
  output$download_vehicle_help3<- renderUI({
    req(vehicle_input_help2())
    downloadButton("download_vehicle_help2",'Download Vehicle Availability template')
  })
  
  #Value box with number of periods in dataset
  output$num_periods2 <- renderValueBox({
    req(results2())
    data1<-Shifts_data2()
    valueBox(paste0("Period: ",as.numeric(length(unique(data1$date)))), subtitle = "Days",
             icon = icon("calendar-alt"),color='yellow')
  })
  
  #Value box with number of shifts in dataset
  output$num_shifts2 <- renderValueBox({
    req(results2())
    data2<-Shifts_data2()
    valueBox(paste0("Scheduled: ", as.numeric(length(names(data2[ , grepl( "SHIFT" , toupper(names(data2)))]))) ), subtitle = " Shifts",
             icon = icon("rocket"),color='aqua')
  })
  
  #Value box with number of shifts in dataset
  output$drivers_imported2 <- renderValueBox({
    req(results2())
    data2<-driver_data2()
    valueBox(paste0("Drivers Imported: ", as.numeric(nrow(data2)) ), subtitle = " Drivers",
             icon = icon("user"),color='aqua')
  })
  
  #Info box with required drivers
  output$drivers_utilized2 <- renderInfoBox({
    req(results2())
    model=model2()
    solution=solution2()
    DerivedDrivers = data.table(get_solution(model, x[i]))[value == 1]
    if (is.null(input$vehicle_input2)) {
      if (nrow(DerivedDrivers)>1) {
        infoBox(paste0(solution, " Solution Found!"), paste0("All ",nrow(DerivedDrivers), " drivers have been utilized to satisfy the imported vehicle schedule"), icon = icon("thumbs-up"),color = "green")
      } else {infoBox("No feasible Solution Found!", paste0("Try changing the preferences or the vehicle schedule"), icon = icon("times"),color = "red")
      }
    } else {
      final_shifts<-results2()
      na_vehs <- sum(is.na(final_shifts$plate))
      if (nrow(DerivedDrivers)>1) {
        infoBox(paste0(solution, " Solution Found!"), paste0("All ",nrow(DerivedDrivers), " drivers have been utilized to satisfy the imported vehicle schedule. "
                                                             ,"There are ",na_vehs," driver-shifts without assigned vehicle")
                , icon = icon("thumbs-up"),color = "green")
      } else {infoBox("No feasible Solution Found!", paste0("Try changing the preferences or the vehicle schedule"), icon = icon("times"),color = "red")
      }
    }
  })
  
  #vehicle input table transposed for reporting reasons
  tabl1_2 <- eventReactive(input$go2,{
    d1<- as.data.table(Shifts_data2())
    d2 <- melt(d1,id.vars = c("date","vehicle")) #transposing for better visual
    d3 <- reshape(d2,timevar = "date",idvar = c("variable","vehicle"), direction = "wide")
    setnames(d3,"variable","shift")
    setnames(d3, names(d3), gsub("value.", "", names(d3)))
    d3 <- d3[order(rank(vehicle,shift))] #ordering for better visual
    d3$vehicle[d3$vehicle==1] <- "Prius" #Replacing vehicle codes with names 
    d3$vehicle[d3$vehicle==2] <- "EV"
    return(d3)
  })
  
  #table output of vehicle schedule input data
  output$table1_2 <- renderDataTable({
    # req(results2())
    d3<-tabl1_2()
    DT::datatable(
      d3,
      selection = "single",
      filter = list(position = 'top', clear = FALSE),
      caption = 'Vehicle Schedule Imported',
      extensions = c('ColReorder', 'FixedHeader', 'Scroller','KeyTable'),
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        searching = T, #search the table
        searchHighlight = TRUE, #highlight the search in the table
        columnDefs = list(list(className = 'dt-center',targets="_all")), #allign column in center of cell
        #buttons = c('copy', 'excel', 'print'), #add download buttons
        colReorder = TRUE, # order the columns as you wish
        keys = TRUE, #navigate to table like excel
        scrollX = TRUE #scroll the table horizontically
      )
    )
  })
  
  #shift scheduling table transposed for reporting reasons
  tabl2_2 <- eventReactive(input$go2,{
    req(results2())
    driver_data<-driver_data2() #imported file from user including available drivers info
    d1<-results2() #optimization results for drivers and vehicles
    sum_hours<-sqldf("select rn,sum(shift_hours) as sum_hours from d1 group by 1") #keeping the sum of hours each driver worked
    if (!is.null(input$vehicle_input2)) { #if vehicle allocation is required include plate number in shift columns
      d1$shift=paste0("shift ",d1$shift," (",d1$plate,")")
      d3<-d1[,-c(2,3,4,7,9,10)] #removing useless columns
    } else { d3<-d1[,-c(2,3,4,7)]
    d3$shift=paste0("shift ",d1$shift)
    }
    # d3<-d1[,-c(2,3,4,7)]
    # d3$shift=paste0("shift ",d1$shift)
    
    d3<-setorder(d3,date,rn,Vehicle)
    d4<-reshape(d3,timevar="date",idvar=c("rn","Vehicle"),direction="wide") #transposing for better visual
    names(d4) <- gsub(names(d4), pattern = "shift.", replacement = "") #fix column names - only dates
    d4<- sqldf("select b.sum_hours as utilized_hours,a.* from d4 a left join sum_hours b on a.rn=b.rn")
    d5 <- merge(driver_data,d4,by="rn",all.x = TRUE,all.y = TRUE) #merging imported drivers info with allocation output
    d5<-d5[,-c(1)] #removing the index we used for joining
    d5<-setorder(d5,id_driver,Vehicle)
    d5$Vehicle[d5$Vehicle==1] <- "Prius" #Replacing vehicle codes with names
    d5$Vehicle[d5$Vehicle==2] <- "EV"
    return(d5)
  })
  
  #shift scheduling ui table output
  output$table2_2 <- renderDataTable({
    req(tabl2_2)
    d3<-tabl2_2()
    # color background in table per shift
    js <- "(/shift 1/).test(value) ? '#23d2aa' : (/shift 2/).test(value) ? '#9e9bae' : (/shift 3/).test(value) ? '#ff6600' : (/shift 4/).test(value) ? '#6f6987' : (/shift 5/).test(value) ? 'green' : (/shift 6/).test(value) ? 'blue' : (/(NA)/).test(value) ? 'red' :''"
    DT::datatable( #defining data table options
      d3,
      selection = "single",
      filter = list(position = 'top', clear = FALSE), #filters on top of table
      caption = 'Driver Schedule generated', #table title
      extensions = c( 'ColReorder', 'FixedHeader', 'Scroller','KeyTable'), #calling relevant extensions
      rownames = FALSE, #no rownames
      options = list( 
        #dom = 'Bfrtip',
        searching = T, #search the table
        searchHighlight = TRUE, #highlight the search in the table
        columnDefs = list(list(className = 'dt-center',targets="_all")), #allign column in center of cell
        #buttons = c('copy', 'excel', 'print'), #add download buttons
        colReorder = TRUE, # order the columns as you wish
        keys = TRUE, #navigate to table like excel
        scrollX = TRUE #scroll the table horizontically 
      )
    ) %>% formatStyle( #shift conditional coloring in the final schedule - maximum 6 shifts, have to make it dynamic
      names(d3), backgroundColor = JS(js)
      )
  })
  
  #download the above table 2
  output$download_schedule <- downloadHandler(
    filename = function() {paste0("Driver Schedule",Sys.time(),".xlsx")},
    content = function(file) {
      write.xlsx(tabl2_2(),file,row.names = FALSE)
    }
  )
  
  #make the download button appear when results are generated
  output$download_schedule_2<- renderUI({
    req(tabl2_2())
    downloadButton("download_schedule",'Download Driver Schedule')
  })
  
  #create the first excel tab of results output
  out_tab_1_2 <- eventReactive(input$go2,{
    req(results2())
    model=model2()
    driver_data<-driver_data2()
    solution=solution2()
    data = Shifts_data2()
    final_shifts = results2()
    Days = as.numeric(length(unique(data$date))) #getting number of days
    Shifts = as.numeric( length(names (data[ , grepl( "SHIFT",toupper(names(data)))] )) ) #getting number of shifts
    Vehicles = length(unique(data$vehicle))
    DerivedDrivers = data.table(get_solution(model, x[i]))[value == 1]
    
    veh_num = 0 #setting the vehicle related data equal to zero in case no vehicle allocation is required
    veh_util = 0
    shifts_uncovered = 0
    if (!is.null(input$vehicle_input2)) { #if vehicle allocation applicable then update results related to vehicles
      veh_Data = vehicle_input2()
      veh_num = length(unique(veh_Data$plate))
      veh_util = sum(!is.na(unique(final_shifts$plate)))
      shifts_uncovered = sum(is.na(final_shifts$plate))
      }

    d3 <- data.table(  same_shift_per_week = input$same_shift_week2
                       ,number_of_shifts = Shifts
                       ,number_of_days = Days
                       ,number_of_vehicles_types = Vehicles
                       ,solution_found = solution
                       ,imported_drivers = nrow(driver_data)
                       ,utilized_drivers = nrow(DerivedDrivers)
                       ,imported_vehicles = veh_num
                       ,utilized_vehicles = veh_util
                       ,uncovered_shifts = shifts_uncovered
    )
    d3<-t(d3)
    colnames(d3)[1] <- "Value"
    return(d3)
  })
  
  #create the third excel tab of results output (driver day utilization)
  out_tab_3_2 <- eventReactive(input$go2,{
    req(results2())
    model=model2()
    DerivedDrivers = data.table(get_solution(model, x[i]))[value == 1]
    if (nrow(DerivedDrivers)>=1) {
      final_shifts <- results2()
      #buidling the data
      data <- sqldf("select days||' days' as Working_days ,count(*) as Drivers from (
                                  select Driver,count(day) as days from final_shifts group by 1)
                                  group by 1 order by 1")
      return(data)
    } else {return(NULL)}
  })
  
  #create the forth excel tab of results output (driver hour utilization)
  out_tab_4_2 <- eventReactive(input$go2,{
    req(results2())
    model=model2()
    DerivedDrivers = data.table(get_solution(model, x[i]))[value == 1]
    if (nrow(DerivedDrivers)>=1) {
      final_shifts <- results2()
      
      data <- sqldf("select hours||' hours' as Working_hours ,count(*) as Drivers from (
                          select Driver,sum(shift_hours) as hours from final_shifts group by 1)
                          group by 1 order by 1 desc")
      
      return(data)
    } else {return(NULL)}
  })
  
  #download the unified module 2 results in multiple tabs
  output$download_results_module3_2 <- downloadHandler(
    filename = function() {paste0("Results run_",Sys.time(),".xlsx")},
    content = function(file) {
      xlsx::write.xlsx(out_tab_1_2(),file,sheetName = "Results" ,row.names = TRUE) #parameters and optimization output
      xlsx::write.xlsx(tabl1_2(),file,sheetName = "Vehicle schedule imported" ,append = TRUE ,row.names = FALSE) #imported vehicle schedule
      xlsx::write.xlsx(shifthours2(),file,sheetName = "Shift Duration" ,append = TRUE ,row.names = FALSE) #imported vehicle schedule
      xlsx::write.xlsx(out_tab_3_2(),file,sheetName = "Driver Day Utilization" ,append = TRUE ,row.names = FALSE) #driver day utilization
      xlsx::write.xlsx(out_tab_4_2(),file,sheetName = "Driver Hour Utilization" ,append = TRUE ,row.names = FALSE)#driver hour utilization
      xlsx::write.xlsx(tabl2_2(),file,sheetName = "Driver Shift allocation" ,append = TRUE ,row.names = FALSE)#driver hour utilization
    }
  )
  
  #make the unified module 2 results download button appear when results are generated
  output$download_results_module3<- renderUI({
    req(results2())
    downloadButton("download_results_module3_2",'Download Results')
  })
  
  #unifying shift hours prius and ev in a data table to use later
  shifthours2<-eventReactive(input$go2,{
    data<-Shifts_data2()
    Shifts = as.numeric( length(names (data[ , grepl( "SHIFT",toupper(names(data)))] )) ) #getting number of shifts
    shifthours_prius=c()
    for (i in 1:Shifts) {
      shifthours_prius[i]=8
    }
    shifthours_ev = c()
    for (i in 1:Shifts) {
      input_name=paste0("shift_",i,"_duration_ev2")
      shifthours_ev[i]=input[[input_name]]
    }
    shifthours=data.table(cbind(shifthours_prius,shifthours_ev))
    shifthours$shift<-row.names(shifthours)
    return(shifthours)
  })
  
  RequestsDF<-eventReactive(input$go2 ,{
    data<-Shifts_data2()
    # Weeks = weeks2()
    if (length(unique(data$date))==7) {Weeks=1} else if (length(unique(data$date))==28) {Weeks=4
    } else {stopApp()}
    #creating the day and week columns
    makeweek <- setorder(data.table(date= unique(data$date)),date)
    makeweek2 <- setorder(data.table(week = rep(1:Weeks,7),day=rep(1:7,Weeks)),week,day)
    makeweek3 <- cbind(makeweek,makeweek2)
    RequestsDF <- merge(data,makeweek3,all.x = T, all.y = T, by="date")
    RequestsDF <- as.data.frame(RequestsDF)
    shifts_columns <- colnames(as.data.frame(RequestsDF[,grepl("SHIFT",toupper(names(RequestsDF)))]))
    RequestsDF<-data.table(RequestsDF)
    RequestsDF <- reshape(RequestsDF,varying = shifts_columns,
                          direction="long", idvar=c("date","vehicle","week"),timevar = "shift", v.names="total")
    RequestsDF <- setorder(RequestsDF,vehicle,shift,day,week)
    return(RequestsDF)
  })
  
  #here we build and customize the model for the given vehicles schedule
  model2<-eventReactive(input$go2 ,{
    data <- Shifts_data2()
    driver_input <- driver_data2()
    shifthours <- shifthours2()
    Weeks <- weeks2()
    RequestsDF <- RequestsDF()
    GetMonthRequests <- function(w,j,k,l) {mapply(function(W,D,S,V) {RequestsDF[week == W & day == D & shift == S & vehicle == V]$total}, w,j,k,l)}
    if (length(unique(data$date))==7) {Weeks=1} else if (length(unique(data$date))==28) {Weeks=4
    } else {stopApp()}
    
    #ordering driver input and assigning row numbers
    driver_input<-setorder(driver_input,id_driver)
    driver_input$rn=row.names(driver_input)
    
    shifthours_prius = shifthours[,shifthours_prius]
    shifthours_ev = shifthours[,shifthours_ev]
    
    # Days of planning ahead, number of shifts and number of drivers available
    Days = length(unique(RequestsDF$day))
    Shifts = as.numeric( length(names (data[ , grepl( "SHIFT",toupper(names(data)))] )) ) #getting number of shifts
    Drivers = nrow(driver_input)
  
    #building the driver constrainsts table including PTOs etc.
    DriverConstr = NULL
    
    if(any(str_detect(colnames(driver_input),pattern = regex("^day|^shift",ignore_case = T)))){
      ChangeColNames = colnames(driver_input)[str_detect(colnames(driver_input),pattern = regex("^shift|^day",ignore_case = T))]
      ChangeColNamesNew = str_replace_all(ChangeColNames,"\\."," ")
      setnames(driver_input,ChangeColNames,ChangeColNamesNew)
      ShiftColNames = colnames(driver_input)[str_detect(colnames(driver_input),pattern = regex("^shift",ignore_case = T))]
      DayColNames= colnames(driver_input)[str_detect(colnames(driver_input),pattern = regex("^day",ignore_case = T))]
      driver_input[, `Shift Unavailability`:=as.character(`Shift Unavailability`)]
      driver_input[, `Day Unavailability`:= str_remove_all(`Day Unavailability`,"\\s")]
      driver_input[, `Shift Unavailability`:= str_remove_all(`Shift Unavailability`,"\\s")]
      if(any(!is.na(driver_input$`Day Unavailability`))|any(!is.na(driver_input$`Shift Unavailability`))){
        Day_Shift_driver_constraints = melt(data = driver_input[,c("id_driver","rn",ChangeColNamesNew), with = F],measure.vars = ShiftColNames,variable.factor = F,value.factor = F,variable.name = "Shift",value.name = "Date",)
        Day_Shift_driver_constraints[!is.na(`Day Unavailability`), DayShiftDate := paste(unlist(Map(function(x) paste0(x,"-",seq_len(Shifts)),unlist(strsplit(as.character(`Day Unavailability`), ',' )))),collapse = ","), by = .(id_driver,`Day Unavailability`)][]
        Day_Shift_driver_constraints[, Date := paste(Date,DayShiftDate,sep = ",")]
        Day_Shift_driver_constraints[, Date:= str_remove_all(Date,"NA,NA")]
        Day_Shift_driver_constraints_TTL = Day_Shift_driver_constraints[,.(Date = unlist(strsplit(as.character(Date), ',' ))), by = .(id_driver,rn,Shift)]
        Day_Shift_driver_constraints_TTL[,Shift := as.numeric(str_extract(Date,"(?<=[-])\\d+"))]
        Day_Shift_driver_constraints_TTL[, Date := unlist(str_split(Date,"-",n = 2))[1], by = .(id_driver,Date)]
        Day_Shift_driver_constraints_TTL[, Date := as.Date(Date, "%m/%d/%Y")]
        if(Weeks >1){
          DriverConstr = unique(RequestsDF[,.(Date = date,Week = week,Day = day)])[Day_Shift_driver_constraints_TTL[!is.na(Date)],on = c("Date")]
          if(nrow(DriverConstr)==0){print("No matching dates among Vehicle Schedule and Drivers' Constraints")}
          DriverConstr = unique(DriverConstr[Shift %in% seq_len(Shifts)][!is.na(Day) & !is.na(Week)])
        }else{
          DriverConstr = unique(RequestsDF[,.(Date = date,Day = day,Week = 1)])[Day_Shift_driver_constraints_TTL[!is.na(Date)],on = c("Date")] 
          if(nrow(DriverConstr)==0){print("No matching dates among Vehicle Schedule and Drivers' Constraints")}
          DriverConstr = unique(DriverConstr[Shift %in% seq_len(Shifts)][!is.na(Day)])
        }
        setorder(DriverConstr,rn)
      }
      DriverConstr$rn<- as.integer(DriverConstr$rn)
    }
    
  withProgress(message = 'Allocation in progress', #progress bar
                detail = "This may take a while..", value=0,{
    # Create and solve optimization model, which consists of, decision variables, Objective and constraints
    model = MILPModel() %>%
      # Driver decision variable
      add_variable(x[i],     i = 1:Drivers,type = "binary") %>%
      # Driver - Shift - Vehicle
      add_variable(z[i,w,j,k,l], i = 1:Drivers, w = 1:Weeks, j = 1:Days, k = 1:Shifts,l = 1:2, type = "binary") %>%
      # Driver - Day 
      add_variable(zd[i,w,j],  i = 1:Drivers, w = 1:Weeks, j = 1:Days, type = "binary") %>%
      # Driver - Week 
      add_variable(zw[i,w],    i = 1:Drivers, w = 1:Weeks, type = "binary") %>%
      # One Shift per Day 
      add_constraint(sum_expr(z[i,w,j,k,l], k = 1:Shifts,l = 1:2) == zd[i,w,j], i = 1:Drivers, w = 1:Weeks,j = 1:Days) %>%
      # One Vehicle per Shift
      add_constraint(z[i,w,j,k,1] + z[i,w,j,k,2] <= 1, i = 1:Drivers,w = 1:Weeks,j = 1:Days,k = 1:Shifts) %>%
      # Maximum working Days per Week
      add_constraint(sum_expr(zd[i,w,j], j = 1:Days) <= zw[i,w]*driver_input[i,MaxDays], i = 1:Drivers, w = 1:Weeks) %>%
      # Minimum working Days per Week
      add_constraint(sum_expr(zd[i,w,j], j = 1:Days) >= zw[i,w]*driver_input[i,MinDays], i = 1:Drivers, w = 1:Weeks) %>%
      # Maximum Working Hours
      add_constraint(colwise(rep(c(shifthours_prius,shifthours_ev), each = Days)) * sum_expr(z[i,w,j,k,l], j = 1:Days, k = 1:Shifts, l=1:2) <=  x[i]*driver_input[i,MaxHours], i = 1:Drivers, w = 1:Weeks) %>%
      # Minimum Working Hours
      add_constraint(colwise(rep(c(shifthours_prius,shifthours_ev), each = Days)) * sum_expr(z[i,w,j,k,l], j = 1:Days, k = 1:Shifts, l=1:2) >=  x[i]*driver_input[i,MinHours], i = 1:Drivers, w = 1:Weeks) %>%
      # comply with shift planning
      add_constraint(sum_expr(z[i,w,j,k,l], i = 1:Drivers) == GetMonthRequests(w,j,k,l), w = 1:Weeks,j = 1:Days, k = 1:Shifts,l = 1:2) %>% 
      # define objective function: Minimize cost or number of drivers
      set_objective(sum_expr(x[i],i=1:Drivers),sense = "max")
    
    # Driver/Day/Shift level constraints
    if(!is.null(DriverConstr)){
      for(ii in seq_len(nrow(DriverConstr))){
        model = model %>% 
          add_constraint(z[i,w,j,k,l] == 0, i = DriverConstr[ii]$rn, w = DriverConstr[ii]$Week, j = DriverConstr[ii]$Day, k = DriverConstr[ii]$Shift,l=1:2)
      }
    }
    
    if (Weeks==4) {
      # Work all weeks
      model = model %>% 
        # never permit a morning shift if previous week last shift was at night (intra month) extra constraints for inter-month shifts
        add_constraint(z[i,w-1,j,3,l] + z[i,w,j-6,k,ll] <=1,i = 1:Drivers, w = 2:Weeks, j = Days, k = 1:2, l=1:2, ll = 1:2)
    }
    
    # same shift for both FT & PT
    if (input$same_shift_week2=="Yes") {
      model = model %>%
        # Driver - week - Shift utilization decision variable for all drivers
        add_variable(b[i,w,k],i = 1:Drivers, w = 1:Weeks, k = 1:Shifts,type = "binary")%>%
        # driver can be assigned to only one shift per week
        add_constraint(sum_expr(b[i,w,k], k = 1:Shifts) <= zw[i,w], i = 1:Drivers, w = 1:Weeks) %>%
        # same shift across all days
        add_constraint(sum_expr(z[i,w,j,k,l],l = 1:2) <= b[i,w,k], i = 1:Drivers, w = 1:Weeks,j = 1:Days, k = 1:Shifts)
    } else {      
      model = model %>%
        # never assign adjacent shifts i.e night->morning for both FT & PT
        add_constraint(z[i,w,j-1,3,l] + z[i,w,j,k,ll]<=1,j = 2:Days, i = 1:Drivers,w = 1:Weeks, k = 1:2,l=1:2,ll = 1:2)
    }
    
    # solve the optimization problem, allways first feasible solution here
    model = model %>%
      solve_model(with_ROI(solver =  "symphony", control = list(verbosity = -1,first_feasible=TRUE)) )
  })
    
    beep(10) #play a sound to know that it is ready
    return(model)
  })
  
  solution2 <- eventReactive(input$go2 ,{
    model=model2()
    DerivedDrivers = data.table(get_solution(model, x[i]))[value == 1]
    if (nrow(DerivedDrivers)==0) {sol_found="Infeasible"}
    else if (model$status=="optimal") {sol_found="Optimal"}
    else {sol_found="Feasible"}
    return(sol_found)
  })
  
  #apply fairness if required
  ShiftPerDriver2 <-  eventReactive(input$go2 ,{
    model <- model2()
    data <- Shifts_data2()
    shifthours <- shifthours2()
    RequestsDF <- RequestsDF()
    Weeks <- weeks2()
    driver_input <- driver_data2()
    
    shifthours_prius = shifthours[,shifthours_prius]
    shifthours_ev = shifthours[,shifthours_ev]
    DriverOut = data.table(get_solution(model, x[i]))[value == 1]
    
    #apply fairness in case monthly vehicle schedule imported
    if (Weeks==1) {    # Shift appointments per driver per day
      ShiftPerDriver2 = data.table(get_solution(model, z[i,w,j,k,l]))[value==1]
    } else if (Weeks==4 & input$apply_fairness2=="No") {
      ShiftPerDriver2 = data.table(get_solution(model, z[i,w,j,k,l]))[value==1]
    } else if (Weeks==4 & input$apply_fairness2=="Yes") {
      ShiftPerDriver = data.table(get_solution(model, z[i,w,j,k,l]))[i %in% DriverOut$i]
      ShiftHours = data.table(k = rep(c(1,2,3),2),l = rep(c(1,2),each = 3),hours = c(shifthours_prius,shifthours_ev))
      TotalHoursPerDriver = setorder(ShiftHours[ShiftPerDriver[value == 1], on = c("k","l")][, .(TotalHours = sum(hours),TotalDays = length(unique(j))), by = .(i,w)],i,w)[]
      TotalHoursPerDriver[, Overall := sum(TotalHours), by = i]
      TotalHoursPerDriverPerWeek = dcast(TotalHoursPerDriver, "i + Overall ~ w",value.var = "TotalHours")
      setkeyv(driver_input,cols = c("MinHours","MaxHours"))
      # Unique Contracts
      NoC = unique(driver_input[,.(MinHours,MaxHours)])
      ScenarioTables = list()
      ScenarioSchedules = list()
      
      NoIter = input$num_iter2
      ii = 0
      
      withProgress( value = 0, {
        while(ii < NoIter){
          ii = ii + 1
          incProgress(1/NoIter,message = paste0("Fairness in progress: ", "Step ",ii," of ",NoIter))
          ScenarioTable = NULL
          for(jj in 2:Weeks){
            DriversAll = NULL
            SampledDriversAll = NULL
            for (jjj in 1:nrow(NoC)){
              NightShiftDrivers = ShiftPerDriver[j == 7 & w == (jj - 1) & k == 3 & value == 1 & i %in% driver_input[NoC[jjj],rn], i]
              NightSample = sample(ShiftPerDriver[j == 1 & w == jj & k == 1 & value == 0 & i %in% driver_input[NoC[jjj],rn] , unique(i)],size = length(NightShiftDrivers),replace = F)
              DayShiftDrivers = setdiff(unique(ShiftPerDriver[i %in% driver_input[NoC[jjj],rn]]$i),NightShiftDrivers)
              DaySample = sample(setdiff(unique(ShiftPerDriver[i %in% driver_input[NoC[jjj],rn]]$i),NightSample),size = length(DayShiftDrivers),replace = F)
              DriversAll = c(DriversAll,NightShiftDrivers,DayShiftDrivers)
              SampledDriversAll = c(SampledDriversAll,NightSample,DaySample)
            }
            Table = data.table( col1= DriversAll, col2 = SampledDriversAll)
            colnames(Table) = c(paste0("Week",c(jj-1, jj)))
            if(jj > 2){
              ScenarioTable = Table[ScenarioTable, on = eval(paste0("Week",jj-1))]
            }else{
              ScenarioTable = cbind(ScenarioTable,Table)
            }
          }
          ScenarioTable[, ID:= Week1]
          ScenarioTableMelt = melt(ScenarioTable,id.vars = "ID",variable.name = "w",value.name = "i",variable.factor = F,value.factor = F)
          ScenarioTableMelt[, w :=as.numeric(str_extract(w,"[[:digit:]]+"))]
          ScenarioSchedules[[ii]] = ScenarioTableMelt
          ScenarioSchedules[[ii]][, Scenario := ii]
          ScenarioTableFull = TotalHoursPerDriver[,.(i,w,WeekHours = TotalHours)][ScenarioTableMelt, on = c("i","w")]
          ScenarioTableFull[, TotalHours := sum(WeekHours), by = ID]
          ScenarioTableFullCast = dcast(ScenarioTableFull,formula = "ID+TotalHours~w", value.var = "WeekHours")
          ScenarioTableFullCast[,Scenario := ii]
          ScenarioTables[[ii]] = ScenarioTableFullCast
        }
      })
      
      withProgress(message = 'Making Reports',
                   detail = "This may take a while..", value=0,{
                     setnames(TotalHoursPerDriverPerWeek,c("i","Overall"),c("ID", "TotalHours"))
                     TotalHoursPerDriverPerWeek[, Scenario:= NoIter+1]
                     ScenarioTables[[NoIter+1]] = TotalHoursPerDriverPerWeek
                     AllScenariosTable = rbindlist(l = ScenarioTables,use.names = T,fill = T)
                     AllScenariosTableHoursAgg = AllScenariosTable[ ,.N, by = .(TotalHours,Scenario)][, UniqueHours := length(unique(TotalHours)), by = .(Scenario)][]
                     DistanceCheck = data.table(Scenario = 1:length(ScenarioTables), Dist = unlist(lapply(1:length(ScenarioTables),function(x) setkey(ScenarioTables[[x]][,.(k=1,ID,TTL = TotalHours)],k)[ScenarioTables[[x]][,.(k=1,ID2 = ID,TTL2 = TotalHours)],allow.cartesian=TRUE][,k:=NULL][,sum(abs(TTL-TTL2))/2])))
                     AllScenariosTableHoursAgg[Scenario == DistanceCheck[which.min(Dist)]$Scenario]
                     BestScenario = DistanceCheck[which.min(Dist)]$Scenario
                     ShiftPerDriverNew = copy(ShiftPerDriver)
                     ShiftPerDriverNew = ScenarioSchedules[[BestScenario]][ShiftPerDriverNew, on = c("i","w")]
                     ShiftPerDriverNew<-ShiftPerDriverNew[,i:=ID][,c("ID","Scenario"):=NULL]
                     ShiftPerDriver2<-copy(ShiftPerDriverNew)
                     ShiftPerDriver2<-ShiftPerDriver2[value==1]
                   })
    }
    
    return(ShiftPerDriver2)
  })
  
  #generate results and output file
  results2 <- eventReactive(input$go2 ,{
    data <- Shifts_data2()
    shifthours <- shifthours2()
    RequestsDF <- RequestsDF()
    ShiftPerDriver2 <- ShiftPerDriver2()
    ShiftPerDriverVehAll <- copy(ShiftPerDriver2)
    shifthours_prius = shifthours[,shifthours_prius]
    shifthours_ev = shifthours[,shifthours_ev]

    setnames(ShiftPerDriver2,old = c("i","j","k","l","w"), new = c("Driver","Day","shift","Vehicle","week"))
    #ordering and joining
    ShiftPerDriver2 <- setorder(ShiftPerDriver2,Driver)
    help1<-tibble(Driver=unique(ShiftPerDriver2$Driver))
    help1<-setorder(help1,Driver)
    help1$rn<-row.names(help1)
    ShiftPerDriver2<- sqldf("select a.*,b.rn from ShiftPerDriver2 a
                       inner join help1 b on a.Driver=b.Driver")
    
    #adding date
    shifthours = data.table(cbind(shifthours_prius,shifthours_ev))
    shifthours$shift<-row.names(shifthours)
    
    final_shifts <- sqldf("select b.date,a.Day,a.week,a.Driver,a.vehicle,a.shift
                      ,case when a.vehicle = 1 then c.shifthours_prius
                      when a.vehicle = 2 then c.shifthours_ev else null end as shift_hours
                      ,a.rn
                      from ShiftPerDriver2 a
                     left join (select distinct date,week,day from RequestsDF) b on a.day=b.day and a.week=b.week
                     left join shifthours c on a.shift=c.shift
                     order by driver,a.day")
    
  # applying vehicle allocation if required
  if (!is.null(input$vehicle_input2)) {
    withProgress( value = 0, { #progress bar
        VehicleConstraints <- vehicle_input2() #loading vehicle input file
        VehicleConstraints[, ":="(w = week,j = day, k = shift,l = vehicle_type)] 
        Dates = unique(VehicleConstraints[,.(date,w,j,k,l)])
        ShiftPerDriverVehAll = Dates[ShiftPerDriverVehAll,on = colnames(Dates)[-1]]
        head(ShiftPerDriverVehAll)
        lapply(list(VehicleConstraints,ShiftPerDriverVehAll), setkeyv, cols = colnames(Dates)[-c(2,3)])
        VehicleAlloctionTable = vector(mode = "list", length = nrow(Dates))
        for (i in seq_len(nrow(Dates))){ #allocating the vehicles
          incProgress(1/nrow(Dates),message = paste0("Allocating Vehicles: ", "Step ",i," of ",nrow(Dates)))
          keypart = Dates[i,.(date,k,l)]
          VehicleAlloctionTable[[i]] = setkey(VehicleConstraints[date == keypart$date & k == keypart$k & l == keypart$l,.(plate,vehicle_id,availability,ind = .I)],ind)[setkey(ShiftPerDriverVehAll[date == keypart$date & k == keypart$k & l == keypart$l,.(date,w,j,k,l,i)][,ind := .I],ind)][,ind := NULL][!is.na(i)]
        }
      
        VehicleAlloctionFinal = rbindlist(VehicleAlloctionTable,use.names = T,fill = T)
        VehicleAlloctionFinal<- data.table(setorder(VehicleAlloctionFinal,i,l,w,j,k)) #ordering accordingly
      
        #adding the vehicles in the final shift file
        final_shifts <- data.table(sqldf("select a.*,case when availability=0 then null else b.vehicle_id end as vehicle_id
                                      ,case when availability=0 then null else b.plate end as plate
                                      from final_shifts a
                                     left join VehicleAlloctionFinal b on a.Driver=b.i and a.date=b.date and a.Shift=b.k and a.Vehicle=b.l
                                     "))
      })
  }

    return(final_shifts)
  })
  
  #bargraph with driver utilization
  output$driver_utilization_days_graph2<-renderHighchart({
    model=model2()
    DerivedDrivers = data.table(get_solution(model, x[i]))[value == 1]
    if (nrow(DerivedDrivers)>=1) {
      final_shifts <- results2()
      #buidling the chart data
      data <- sqldf("select days||' days' as Working_days ,count(*) as Drivers from (
                                  select Driver,count(day) as days from final_shifts group by 1)
                                  group by 1 order by 1")
      
      #creating the donut char chart
      highchart() %>%
        hc_title(text = "Driver - Day Utilization") %>%
        hc_chart(type = "pie") %>%
        hc_add_series_labels_values(data$Working_days,data$Drivers,name="Number of drivers",innerSize =  '50%'
                                    ,dataLabels = list(enabled = TRUE,format = '{point.name}: {point.percentage:.1f} %')) %>%
        # hc_plotOptions(pie = list(size = 400)) %>% #size of graph
        hc_colors(c('#ff6600','#23d2aa','#9e9bae','#6f6987')) #defining color options
    } else {return(NULL)}
  })
  
  #bargraph with required drivers per shift
  output$driver_utilization_hours_graph2<-renderHighchart({
    model=model2()
    DerivedDrivers = data.table(get_solution(model, x[i]))[value == 1]
    if (nrow(DerivedDrivers)>=1) {
      final_shifts <- results2()
      
      data <- sqldf("select hours||' hours' as Working_hours ,count(*) as Drivers from (
                          select Driver,sum(shift_hours) as hours from final_shifts group by 1)
                          group by 1 order by 1 desc")
      
      #creating the bar char chart
      hchart(data, "column",name="Number of drivers", hcaes(x = Working_hours, y = Drivers),color='#ff6600',
             dataLabels = list(enabled = TRUE)) %>%  hc_title(text = "Driver Hour Utilization") #adding labels
    } else {return(NULL)}
  })

}

shinyApp(ui, server)
