#############################################
#
#  link to ShinyApps https://innavays.shinyapps.io/Air_Pollution_App
# 
#############################################

#install.packages("DT")
#install.packages("RCzechia")

library('rmarkdown')
library('RCzechia')
library('sf')
library("dplyr")
library("ggplot2")
library("plotly")
library("tidyr" )
library("DT" )

# PAGE
my_css <- " 
@import url('//fonts.googleapis.com/css?family=Roboto+Condensed&display=swap');

.body{
 background: white;
 font-size: 15px;
 font-family: 'Roboto Condensed'
}
.container-fluid {
 background: #E8D8DF;
 font-size: 15px;
 font-family: 'Roboto Condensed'
}
.col-sm-8 {
 background: white;
 font-size: 15px;
 font-family: 'Roboto Condensed'
}

.col-sm-8:active {
  background-color: white
}

.well {
 background: #C7B9BE;
 font-size: 15px;
 font-family: 'Roboto Condensed','Arial';
 font-color: black;
 font-weight: bold
}

#draw {
 background: #E64747;
 font-size: 20px;
 font-weight: bold;
 font-family: 'Roboto Condensed','Arial';
 margin-left: auto;
 margin-right: auto
}

#download {
 font-weight: bold;
 font-size: 15px;
 font-family: 'Roboto Condensed','Arial'
}
#HTMLreport {
 font-weight: bold;
 font-size: 15px;
 font-family: 'Roboto Condensed','Arial'
}
#WordReport {
 font-weight: bold;
 font-size: 15px;
 font-family: 'Roboto Condensed','Arial'
}
#table {
 font-size: 13px;
 font-family: 'Roboto Condensed','Arial'
}
#table_title {
 font-size: 20px;
 font-weight: bold;
 color: #444fa6;
 font-family: 'Roboto Condensed'
}
#standart {
 font-size: 17px;
 font-weight: bold;
 font-family: 'Roboto Condensed'
}
#message {
 font-size: 13px;
 font-weight: bold;
 font-family: 'Roboto Condensed'
}
#message_copy {
 font-size: 13px;
 font-weight: bold;
 font-family: 'Roboto Condensed'
}
"

ST<-read.csv('Czeck_Rdata/__Stations.csv')
path<-"Czeck_Rdata/"

#######################
# AGGREGATION FUNCTIONS
#######################

### CALENDAR TIME
agg1f <- function(x,th) {
  x_a<- x %>% mutate(Y=Concentration) %>% select(-Concentration)
  x_a 
}
### DAILY AVARAGE  
agg2f <- function(x,th) {
  x_a<- x %>% group_by(Year,Month,Day) %>% summarise(Y=mean(Concentration)) %>% na.omit() 
  x_a 
}
### DAILY MAXIMUM
agg3f <- function(x,th) {
  x_a<- x %>% group_by(Year,Month,Day) %>% summarise(Y=max(Concentration)) %>% na.omit() 
  x_a 
}
### Number of hours per day for which a given threshold is exceeded
agg4f <- function(x,th) {
  x_a<- x %>% group_by(Year,Month,Day,Hour) %>% summarise(hour_max=max(Concentration)) %>% na.exclude() %>%
    group_by(Year,Month,Day) %>% summarise(Y=sum(hour_max>th))
  x_a 
}
### Number of hours per year for which a given threshold is exceeded
agg5f <- function(x,th) {
  x_a<- x %>% group_by(Year,Month,Day,Hour) %>% summarise(hour_max=max(Concentration)) %>% na.exclude() %>%
    group_by(Year) %>% summarise(Y=sum(hour_max>th))
  x_a 
}
### Number of days per year for which the daily average concentration exceeds a given threshold
agg6f <- function(x,th) {
  x_a<- x  %>% select(-5,-6) %>% group_by(Year,Month,Day) %>% summarise(day_mean=mean(Concentration)) %>% na.exclude() %>%
    group_by(Year) %>% summarise(Y=sum(day_mean>th))
  x_a 
}

##############################
### CRAZY TIME HANDLING
#############################

## CASE 1. CALEDAR TIME
time1f<- function(x) {
  if ('Hour' %in% colnames(x)) {
    x_a <- x %>% 
      mutate(Date=paste(as.character(Year),as.character(Month),as.character(Day),as.character(Hour))) %>%
      mutate(Date=as.POSIXct(Date, format="%Y %m %d %H")) %>% na.omit()
    x_a <-x_a[c("Date", "Y")] %>% unique.array()
  } else {
    if ('Day' %in% colnames(x)) {
      x_a <- x %>%
        mutate(Date=paste(as.character(Year),as.character(Month),as.character(Day))) %>%
        mutate(Date=as.POSIXct(Date, format="%Y %m %d")) %>% na.omit()
      x_a <-x_a[c("Date", "Y")] %>% unique.array()
    } else 
      x_a <- x %>% mutate(Date=Year)
    x_a <-x_a[c("Date", "Y")] %>% unique.array()
  }
  x_a 
}

## CASE 2. YEARS OVERALL
time2f<- function(x) {
  x_a <- x %>% mutate(Date=paste(as.character(Month),as.character(Day))) %>%  
    mutate(Date=as.POSIXct(Date, format="%m %d")) %>% na.omit()
  x_a <-x_a[c("Date","Y")] %>% unique.array()
  x_a
}
## CASE 3. WEEK OVERALL
time3f<- function(x) {
  x_a <- x %>% mutate(Date=paste(as.character(Year),
                                 as.character(Month),
                                 as.character(Day))) %>% 
    mutate(Date=as.POSIXct(Date, format="%Y %m %d")) %>%
    mutate(Date=factor(format(Date, "%A"), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) 
  #%>% arrange(factor(Date, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) 
  x_a <-x_a[c("Date","Y")] %>% unique.array()
  x_a
}

## CASE 4. DAY OVERALL
time4f<- function(x) {
  x_a <- x %>% mutate(Date=Hour) %>% na.omit()
  x_a <-x_a[c("Date","Y")] %>% unique.array()
  x_a
}

########################
# INPUTS
########################

stList <- ST %>% transmute( col = paste(EoICode,StationName,sep="-"))
stList <- cbind(ST[1],stList)
colnames(stList)<-c("code","name")
stList<-as.data.frame(stList)

pollutant_mtx <- cbind(c('Fine particulates (PM2.5)', 'Particulates (PM10)', 'Sulphur dioxide (SO2) ', 'Nitrogen dioxide (NO2)'),
                       c("PM2.5","PM10","SO2","NO2"))
colnames(pollutant_mtx)<-c("name","code")
pollutant_mtx<-as.data.frame(pollutant_mtx)

statistic_name <- c("Raw hourly data (no aggregation)",
                    "Daily averages",
                    "Daily maxima",
                    "Number of hours/day for which the threshold is exceeded",
                    "Number of hours/year for which the threshold is exceeded",
                    "Number of days/year for which the daily average exceeds the threshold")
time_choice <- c("Calendar time",
                 "Date within the year (going from Jan 1st to Dec 31st)",
                 "Day within the week (going from 0 to 7 days)",
                 "Hour in the day (going from 0 to 24 hours)")


agg_mtx<-cbind(statistic_name, c(agg1f, agg2f, agg3f, agg4f, agg5f, agg6f))
colnames(agg_mtx)<-c("name","func")
agg_mtx<-as.data.frame(agg_mtx)

time_mtx<-cbind(time_choice, c(time1f,time2f,time3f,time4f))

colnames(time_mtx) <-c("name","func")
time_mtx<-as.data.frame(time_mtx)

un_mtx<-rbind(c(TRUE,TRUE,TRUE,TRUE),
              c(TRUE,TRUE,TRUE,FALSE),
              c(TRUE,TRUE,TRUE,FALSE),
              c(TRUE,TRUE,TRUE,FALSE),
              c(TRUE,FALSE,FALSE,FALSE),
              c(TRUE,FALSE,FALSE,FALSE))

EUs<-rbind(c("PM10",2,50),
           c("PM10",6,35),
           c("SO2",1,350),
           c("SO2",2,125),
           c("SO2",3,350),
           c("SO2",5,24),
           c("SO2",6,3),
           c("NO2",1,200),
           c("NO2",5,18))

EUs_limits<-rbind(c("PM10",6,50),
                  c("SO2", 5,350),
                  c("SO2", 6,125),
                  c("SO2", 4,125),
                  c("NO2", 5,200),
                  c("NO2", 4,200))

# ROW FILE GENERATION FUNCTION

file_gen<-function(name,pol) {
  outcome<-"no data"
  if ( !identical(name,"None")) {
    file_name<-paste(name,"_",pol,".RData", sep="")
    outcome <- tryCatch(
      suppressWarnings({
      load(paste(path,file_name, sep=""))
      data
      }),
      error = function(e) "no data")
  }
  outcome
}

# CALCULATION FUNCTION

file_alt<- function(x,a,t,th) {
  suppressWarnings({
    flag <- un_mtx[a,t]
    func_t <- time_mtx[t,2][[1]]
    func_a <- agg_mtx[a,2][[1]]
    if ( !identical(x,"no data")  & flag) {
      M <- func_a(x,th) %>% func_t()
    } else 
      M<-"no data"
  })
  M
}

#######################
# THEMES AND STYLING
######################

# GGPLOT
theme_bluewhite <- function (base_size = 13, base_family = 'Arial') {
  theme(
    text = element_text(family = "Arial", color = "black"),
    panel.grid.major  = element_line(color = '#E6E6E6'),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "white", fill = NA),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "white"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(angle=45),
    plot.title=element_text(size=13, hjust = 0.5)
  )
}

# PLOTLY
t <- list(
  family = 'Roboto Condensed', 
  size = 13,
  color = 'black')

#######################
# UI
######################

ui <- fluidPage( 
  tags$style( my_css),
  headerPanel(
    h1("Analysis of air pollution data in the Czech Republic", style = "font-family: 'Roboto Condensed'; 
       font-weight: bold; line-height: 0.8; color: black; font-size: 30px; padding: 10px 0; text-align: center")
    ),
  titlePanel( h2("Data collected by European Environmental Agency in the Czeck Republic 2013-2019", 
              style = "font-family: 'Roboto Condensed'; font-size: 16px; font-weight: bold; 
              color: #444fa6; text-align: left")),
  tags$hr(),
  sidebarLayout(
   sidebarPanel(
      radioButtons(inputId = "pol", label = "Pollutant", choices = pollutant_mtx$name, 
                       selected=pollutant_mtx$name[1]), 
      tags$hr(),   
      selectInput("st1", "Station 1",choices = stList$name, selected=stList$name[126]),  
      selectInput("st2", "Station 2",choices = c("None",stList$name), selected="None"),                    
      selectInput("st3", "Station 3",choices = c("None",stList$name), selected="None"),                    
      selectInput("st4", "Station 4",choices = c("None",stList$name), selected="None"),
      tags$hr(),
      selectInput("stat", "Statistic", choices = agg_mtx$name, selected = agg_mtx$name[1]),
      
      conditionalPanel( condition = "output.agn == 1",    
                           radioButtons(inputId = "time1", label = "Time",
                                        choices = time_mtx$name,
                                        selected = time_mtx$name[1])
      ),
      conditionalPanel( condition = "output.agn==2 || (output.agn==3 || output.agn==4)",    
                       radioButtons(inputId = "time2", label = "Time representatin",
                                        choices = time_mtx$name[-4],
                                        selected = time_mtx$name[1])
      ),
      conditionalPanel( condition = "output.agn==5 || output.agn==6",    
                        radioButtons(inputId = "time3", label = "Time representatin",
                                        choices = time_mtx$name[1],
                                        selected = time_mtx$name[1])
      ),
      conditionalPanel( condition = "output.agn==4 || (output.agn==5 || output.agn==6)", 
                       helpText("Note: a default value (if exists) \n        
                                 presents a current air quality standard \n
                                in the EU"),
                        uiOutput("thresControl")
      ),
      tags$hr(),
      actionButton("draw", "SUBMIT", style="position: 'centre'"),
      tags$hr(), 
      downloadButton("download", "Download CSV file"),
      br(),
      downloadButton("HTMLreport", "Generate report HTML"),
      br(),
      downloadButton("WordReport", "Generate report Word")
      ),
   mainPanel(
        br(),
        plotlyOutput("plot"),
        textOutput("message"),
        tags$hr(),    
        textOutput("standart"),
        tags$hr(),
        plotOutput("map"),
        tags$hr(),
        textOutput("table_title"),
        textOutput("message_copy"),
        dataTableOutput("table"),
        tags$hr(),
    titlePanel( h3("*Inna Vays*", style = "font-family: 'Roboto Condensed'; font-size: 14px;
                   font-weight: bold; color: #444fa6; text-align: right "))
  )
 )
)
#######################
# SERVER
######################

server <- function(input, output, session) {

##################################################
# INPUTS ARE HERE
##################################################
  values <- reactiveValues()

# GETTING STATIONS CODES  
    
  observe( values$name_1<- ifelse( !identical(input$st1,"None"), stList[which(stList['name']==input$st1),'code']%>%
                                     as.character(), "None") )
  observe( values$name_2<- ifelse( !identical(input$st2,"None"), stList[which(stList['name']==input$st2),'code']%>%
                                     as.character(), "None") )
  observe( values$name_3<- ifelse( !identical(input$st3,"None"), stList[which(stList['name']==input$st3),'code']%>%
                                     as.character(), "None") )
  observe( values$name_4<- ifelse( !identical(input$st4,"None"), stList[which(stList['name']==input$st4),'code']%>%
                                     as.character(), "None") )
# GETTING STATISTIC CODE 

  observe( values$aggn <- which(agg_mtx['name']==input$stat) )
  output$agn <- reactive( {values$aggn} )                     # VALUE FOR OUTPUT
  outputOptions(output, "agn", suspendWhenHidden = FALSE)

# SETTING OF TIME CASE. GETTING TIME CODES 

  observe( values$pollut <- pollutant_mtx[which(pollutant_mtx['name']==input$pol),'code'] )
  observe( values$tn<- { 
                        tt<-0
                        if (values$aggn==1 ) {
                          tt<-input$time1
                        }
                        if (values$aggn %in% c(2,3,4) ) {
                          tt<-input$time2
                        }
                        if (values$aggn %in% c(5,6) ) {
                          tt<-input$time3
                        }
                        which(time_mtx['name']==tt)
                        } )
# SETTING THRESHOLD 

  output$thresControl <- renderUI({
    init_value<- as.double(EUs_limits[ EUs_limits[,1]==values$pollut & EUs_limits[,2]==values$aggn ][3])
    init_value<- ifelse( !is.na(init_value), init_value, 0)
    
  numericInput(inputId="thr", label="Threshold", value= init_value, min = 0)
 })

##################################################
# PLOTS AND TABLES
##################################################

# DOWNLOADING DATA FILES
  file_1 <- reactive({ 
    f1<-file_gen( values$name_1, values$pollut )
    f1
  }) 
  file_2 <- reactive({ 
    f2<-file_gen( values$name_2,values$pollut )
    f2
  }) 
  file_3 <- reactive({ 
    f3<-file_gen( values$name_3, values$pollut )
    f3
  }) 
  file_4 <- reactive({ 
    f4<-file_gen( values$name_4, values$pollut )
    f4
  }) 

# GETTING NAMES OF VALID INPUTS  
  notempty<-reactive({
    notempty<-numeric()
    kk<-list(file_1(), file_2(), file_3(), file_4())
    for (i in 1:4) {
      if ( !identical(kk[[i]] , "no data")) {
        notempty <- c(notempty,i)
      }}
    notempty
    })
  
  valid_codes<-reactive({
    valid_codes<- c(values$name_1, values$name_2, values$name_3, values$name_4 )
    valid_codes<-valid_codes[ notempty() ]
    valid_codes
  })

# LIST OF VALID FILES
  All_files <- eventReactive(input$draw, {
    All<-list(file_1(), file_2(), file_3(), file_4())
    All <- All[ notempty() ]
    if ( length(All)!= 0 ) { 
      for (i in 1:length(All)) { 
        All[[i]] <- All[[i]] %>% 
          file_alt(values$aggn, values$tn, input$thr) %>% unique.array() %>% 
          group_by( Date ) %>% 
          mutate( ObsNum=seq(n()))
        names(All[[i]])[2] <- paste("St_",valid_codes()[i], sep="")
      }
    }
    All
  })

# MURGED TABLE OF VALID DATA
  merged_file <- eventReactive(input$draw, {
    mf <- All_files()[[1]]
    if (length(All_files())>1) {
      i<-2
      while (i<=length(All_files()) ) {
        mf<- full_join(mf, All_files()[[i]], by=c("Date", "ObsNum"))          
        i<-i+1
        }
      }
    mf<-mf[-3]
    mf
   })

# MESSAGE TEXT WITH INVALID NAMES   
  message_text<- eventReactive(input$draw, {
    mt<-""
    mt<-ifelse( (identical(file_1(),'no data') & input$st1!="None"), paste(mt, values$name_1), mt)
    mt<-ifelse( (identical(file_2(),'no data') & input$st2!="None"), paste(mt,",",values$name_2), mt)
    mt<-ifelse( (identical(file_3(),'no data') & input$st3!="None"), paste(mt,",", values$name_3), mt)
    mt<-ifelse( (identical(file_4(),'no data') & input$st4!="None"), paste(mt,",", values$name_4), mt)
    mt<-ifelse( mt!="", paste("NOTE: no data on",input$pol,"concentration at station(s):", mt), mt)
  })

# PLOTS 
  
# title 
  title_plot <- reactive({
    v_title<-c("Raw hourly data (no aggregation)",
               "Daily averages",
               "Daily maxima",
               "Number of hours/day for which \n the threshold is exceeded",
               "Number of hours/year for which \n the threshold is exceeded",
               "Number of days/year for which \n the daily average exceeds the threshold")
    nv<-v_title[values$aggn]
    nv
  })
  
# X axis title
  xtitle <- reactive({
    v_title<-c("Date",
               "Seasonal effect",
               "Weekly effect",
               "Daily effect")
    nv<-v_title[values$tn]
    nv
  }) 
  
# Plots mode SELECTOR 
  plot_type <- reactive({ 
    if (values$tn==1 ) 
      t<-'lines'
    if (values$tn==2 ) 
      t<-'points'
    if (values$tn %in% c(3,4) ) 
      t<-'points_jitter'
    t
  })

# MAIN INTERACTIVE PLOT
 
  p_plotly <- eventReactive(input$draw, { 

  # SWITCH function for a plot mode    
    plot_mod<- function(type=plot_type(), num, col, opc=0.7) {
             switch (plot_type(),
               'lines' = add_trace( p_a, x = ~All_files()[[num]][[1]], line = list(color = col, size = 0.1, opacity=opc), mode="lines" , 
                              y = ~All_files()[[num]][[2]], name=valid_codes()[num], type="scatter"),
                'points'= add_markers( p_a, x = ~All_files()[[num]][[1]], marker = list(color = col, size = 3, opacity=opc), mode="markers",
                              y = ~All_files()[[num]][[2]], name=valid_codes()[num], type="scatter"),
                'points_jitter'= add_markers( p_a, x = ~jitter(as.numeric(All_files()[[num]][[1]] )), 
                                     marker = list(color = col, size = 3, opacity=opc), mode="markers",
                              y = ~All_files()[[num]][[2]], name=valid_codes()[num], type="scatter")
             )}
    
# horisontal line of EU standart value
    hline <- {
     line_value <- as.double(EUs[ EUs[,1]==values$pollut & EUs[,2]==values$aggn ][3])
     line_value<- ifelse( !is.na(line_value), line_value, 0)
     list( line_value, list(type = "line", x0 = 0, x1 = 1, xref = "paper", name = "EU standart", 
                   line = list(color = 'black', dash='dash'), y0 = line_value, y1 = line_value ) )
    }

# AXIS  
    axis_yaxis <- list(title = paste("Concentration ",values$pollut, ", µg/m3"))
    axis_xaxis <- {
      if (values$tn==1) { 
        v2<-list(title = "Year",tickangle = 45,type = 'date', tickformat = "%Y") }
      if (values$tn==2) { 
        v2<-list(title = "Seasonal effect",tickangle = 45,type = 'date', tickformat = "%B") }
      if (values$tn==3) { 
        v2<-list(title = "Weekly effect",tickangle = 45, tickmode = 'array',
                 ticktext = list("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                 tickvals = list(1,2,3,4,5,6,7)
                 )}
      if (values$tn==4) { 
        v2<-list(title = "Daily effect", tickangle = 45, nticks = 12) }
      v2
    }

# LINES UPLOADED ONE BY ONE. MURGED FILE IS SLOWER
    p_a<- plot_ly() %>% layout( title=title_plot(), yaxis=axis_yaxis, xaxis=axis_xaxis, showlegend = TRUE, font=t )
    if ( length(All_files()) >= 1  ) { 
      p_a <- plot_mod(plot_type, 1, '#444fa6',opc=0.8)
    }
    if ( length(All_files()) >= 2  ) { 
      p_a <- plot_mod(plot_type, 2, 'purple',opc=0.7)
    }
    if ( length(All_files()) >= 3  ) { 
      p_a <- plot_mod(plot_type, 3, '#E64747',opc=0.6)
    }
   if ( length(All_files()) >= 4  ) { 
     p_a <- plot_mod(plot_type, 4, 'steelblue',opc=0.5)
   }
   if ( hline[[1]]!=0 )
     p_a<- p_a %>% layout(shapes = list(hline[[2]]) )
    
   p_a   # FINAL PLOT
   })
  
# STATIC GGPLOT FOR WORD_DOC
  
  p_ggplot<- eventReactive(input$draw, {
    
# LONG FORM DATA FRAME
   merged_file_long <- tryCatch({ 
     s<- merged_file() %>% gather(key = "Stations", value = "Concentration", -Date)
     if (values$tn==2) s$Date<-as.Date(s$Date)
     s
    }, error = function(e) data.frame()
  )
    
# SWITCH FUNCTION FOR PLOT MODE
    ggplot_type_function <- function(type) {
      switch(type,
             "lines" = geom_line(aes(x=Date, y=Concentration, colour=Stations), size=0.2),
             "points" = geom_point(aes(x=Date, y=Concentration, colour=Stations), size=0.09),
             "points_jitter" = geom_point(aes(x=jitter(as.numeric(Date)), y=Concentration, colour=Stations), size=0.09)
             )
    }
# horisontal line of EU standart value    
    hline <- {
      line_value <- as.double(EUs[ EUs[,1]==values$pollut & EUs[,2]==values$aggn ][3])
      line_value<- ifelse( !is.na(line_value), line_value, 0)
      line_value
    }
    
    p_a <- ggplot(data = merged_file_long) +
      theme_bluewhite() +
      ggtitle( title_plot()  ) + 
      ylab(paste("Concentration ", values$pollut, ", µg/m3"))+
      xlab( xtitle())
    if (values$tn==3) { 
      p_a <- p_a + 
        scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                         labels = c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))
    }
    if (values$tn==2) { 
      p_a <- p_a + scale_x_date( date_breaks = "1 month", date_labels ="%B")
    }
    if ( hline[[1]]!=0 )
      p_a <- p_a + geom_hline(yintercept = hline)
   try( p_a <- p_a + ggplot_type_function(plot_type()) )
    
    p_a    # FINAL PLOT
 })

# MAP OF THE REPUBLIC
  
  Big_map <- eventReactive(input$draw, {   
    borders <- RCzechia::republika("low")
    rivers <- subset(RCzechia::reky(), Major == T)
    
    regions <- RCzechia::kraje("low") %>% select(NAZ_CZNUTS3, geometry)
    capital <- regions %>% filter(NAZ_CZNUTS3=='Hlavní město Praha')    # CAPITAL DISTRICT
    regions <- regions %>% filter(NAZ_CZNUTS3!='Hlavní město Praha')    # REGIONS WHITHOUT CAPITAL
    
    # CENTROIDS OF REGIONS AND CAPITAL
    centroids <- regions$geometry %>% st_centroid() %>% unlist()
    center.x <- centroids[ c(TRUE,FALSE)  ]
    center.y <- centroids[ !c(TRUE,FALSE)  ]
    center.name<-regions$NAZ_CZNUTS3
    Praha <- capital$geometry %>% st_centroid() %>% unlist() 
    
# MAP OF CZECK REPUBLIC WITH RIVERS AND REGIONAL BORDERS 
    Big_map<- ggplot() +
      geom_sf(data = borders, col = "black", fill = '#b9faaf')+
      geom_sf(data = regions, color = "black", fill = NA, size = 0.2) +
      geom_sf(data = capital, color = "black", fill = 'pink', size = 0.2) +
      geom_sf(data = rivers, color = "#444fa6", alpha = 0.5) +
      labs(title = "Czeck Republic map") +
      geom_text(aes(label = center.name, x = center.x, y = center.y), size=3) +
      geom_text(aes(label = "Praha", x = Praha[1], y = Praha[2]), size=6, 
                vjust = 0, nudge_y = 0.05, fontface = "bold")

# SETTING OF STATIONS GEOLOCATION
   st_names_full<-c(values$name_1,values$name_2,values$name_3,values$name_4)
    
   St_location <- ST %>% filter( EoICode %in% st_names_full )%>% select(EoICode, Latitude, Longitude )

# FULL MAP
   Big_map <-  Big_map + geom_point( aes(x=St_location$Longitude, 
                                         y=St_location$Latitude, label = St_location$EoICode), pch=16, col="#E64747")+
                          geom_text(aes(label = St_location$EoICode, x = St_location$Longitude, 
                                        y = St_location$Latitude), size=4, hjust = 0, col="#E64747", fontface = "bold")+
                          ylab('') +
                          xlab('')
    Big_map
  }
)
  
# DATA TABLE FOR OUTPUT
  data_set <- eventReactive(input$draw, {
    tryCatch({
      d <- merged_file() 
      if (values$tn==2) { 
        d <- d %>% mutate(Date1=format(Date, "%B, %d"))
        d <-d[-1]
        d <-d[seq(ncol(merged_file()),1)]
        names(d)[1]='Date'
      }
      d
   },
    error = function(e) data.frame() )
  })
#####################
# FINAL OUTPUTS  
#####################

# EU standart for currant pollutant choice
  standart <- eventReactive(input$draw, {
    if (values$pollut=='PM2.5')
      stand <- "EU standart for PM2.5: Yearly average of at most 25µg/m3."
    if (values$pollut=='PM10')
      stand <- "EU standart for PM10: Daily average exceeding 50µg/m3 observed on at most 35 days a year, and yearly average of at most 40µg/m3."
    if (values$pollut=='SO2')
      stand <- "EU standart for SO2: Hourly concentration exceeding 350µg/m3 for at most 24 hours per year, and average daily concentration exceeding 125µg/m3 on at most 3 days per year."
    if (values$pollut=='NO2')
      stand <- "EU standart for NO2: Hourly concentration exceeding 200µg/m3 for at most 18 hours per year, and average yearly concentration of at most 40µg/m3."
    stand
  })
  output$standart  <- renderText({
    standart()
  })

# table
  table_title <- eventReactive(input$draw, {
    paste("Concentration", input$pol,'.',input$stat,".",time_mtx[values$tn,'name'],'\n')
  })
  output$table_title  <- renderText({
    table_title()
  })
  output$table <- DT::renderDataTable({
    data_set()
  })
  
# interactive plot
  output$plot <- renderPlotly({
    p_plotly()
    #p_ggplot()
  })
  output$message <- renderText({
    message_text()           
  })
  output$message_copy <- renderText({
    message_text()           
  })

# map plot  
  output$map <- renderPlot({
    Big_map()
  })
  
# DOWNLOAD BUTTONS
 
  output$download <- downloadHandler(
    filename = function() {
      paste("data_table",".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_set(), file, row.names = FALSE)
    }
  )
  
  output$HTMLreport <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      parameters_h <- list(finPlot=p_plotly(), message=message_text(), map=Big_map(),
                         station_1=input$st1, station_2=input$st2, station_3=input$st3,station_4=input$st4, 
                         pollutant=input$pol, time=time_mtx$name[values$tn], aggr=input$stat, threshold=input$th, 
                         data=data_set(), standart=standart())
      rmarkdown::render("report_HTML.Rmd", output_format="html_document", output_file=file, params =parameters_h )
    }
  )
  output$WordReport <- downloadHandler(
    filename = "report.docx",
    content = function(file) {
      parameters_w <- list(finPlot=p_ggplot(), message=message_text(), map=Big_map(),
                         station_1=input$st1, station_2=input$st2, station_3=input$st3,station_4=input$st4, 
                         pollutant=input$pol, time=time_mtx$name[values$tn], aggr=input$stat, threshold=input$th, 
                         data=data_set(), standart=standart())
      rmarkdown::render("report_Word.Rmd", output_format="word_document", output_file=file, params =parameters_w )
    }
  )
}

shinyApp(ui, server)
