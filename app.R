
############################################################
#### Corona Virus Shiny App
############################################################


# Load packages
library(shiny)
library(shinythemes)
library(shinydashboard)
require(dplyr)
require(tidyr)
require(rvest)
require(ggdark)

require(rvest)
require(tidygeocoder)
require(tmap)
require(tmaptools)
require(ggplot2)
require(plotly)
#require(tidygeocoder)
require(plotly)
require(leaflet)
############################################################
#### Data Handling
############################################################
# Specify the Github links to the different files
#------------------ Data ------------------
url<- read_html("http://covid19.ncdc.gov.ng/")
#Total_cases<-html_table(url,fill=T)[3]
#Total_cases<-data.frame(Total_cases)

Total_cases<-url %>% 
  html_nodes("span") %>%
  html_text()

#confirmed cases
b1=Total_cases[33]
b1=gsub(",", "", b1, fixed = TRUE)
Confirmed<-as.numeric(b1)

#active cases
b4=Total_cases[34]
b4=gsub(",", "", b4, fixed = TRUE)
Active<-as.numeric(b4)

#discharged
b2=Total_cases[35]
b2=gsub(",", "", b2, fixed = TRUE)
Recovered<-as.numeric(b2)

#Death
b3=Total_cases[36]
b3=gsub(",", "", b3, fixed = TRUE)
Death<-as.numeric(b3)
df<-data.frame(Confirmed,Active,Recovered,Death)

#html("http://covid19.ncdc.gov.ng/") %>% html_nodes(".card-body") %>% html_nodes("table") %>%html_table 

covid_state1<-html_table(url,fill=T)
covid_state1<-data.frame(covid_state1)
names(covid_state1)<-c("state","confirmed_cases","admission","discharged","death")
#covid_state1<-gsub("[^[:alnum:][:blank:]?&/\\>]",">","covid_state1",covid_state1)



covid_state<-covid_state1
#covid_state<-covid_state%>%drop_na()

covid_state$confirmed_cases<-gsub(",","",covid_state$confirmed_cases)
covid_state$admission<-gsub(",","",covid_state$admission)
covid_state$discharged<-gsub(",","",covid_state$discharged)


lat<-c(6.443261653,	9.083333149,	7.970016092,	6.340477314,	9.929973978,
       4.810002257,	11.0799813,	11.99997683,	5.890427265,	7.160427265,
       7.250395934,	6.867034321,	6.25,	8.490010192,	11.5203937,	7.629959329,
       5.532003041,	10.62042279,	10.29044293,	11.68040977,	5.492997053,
       7.190399596,	8.490423603,	4.66403,	11.7991891,	5.007996056,	10.4003587,
       7.630372741,	10.2703408,	6.210433572,	13.06001548,	12.45041445,
       7.870409769,	4.960406513,	12.1704057,	11.74899608,	7.800388203
)

long<-c(3.391531071,	7.533328002,	3.590002806,	5.620008096,	8.890041055,	7.010000772,	7.710009724,	8.5200378,	5.680004434,	3.350017455,	5.199982054,	7.383362995,	8.0833,	4.549995889,
        7.320007689,	4.179992634,	7.486002487,	12.18999467,	11.16995357,
        10.19001339,	7.026003588,	8.129984089,	8.5200378,	6.036987,	
        9.350334607,	7.849998524,	5.469939737,	5.219980834,	13.2700321,
        7.06999711,	5.240031289,	4.199939737,	9.780012572,	8.330023558,
        6.659996296,	11.96600457,	6.739939737
)


rx<-data.frame(state=covid_state$state, 
               confirmed_cases=covid_state$confirmed_cases,
               admission=covid_state$admission,
               discharged=covid_state$discharged,
               death=covid_state$death,
               lat=c(lat),long=c(long))


#rx<-geocode(covid_state,state,method = "osm")
rx$confirmed_cases<-as.character(rx$confirmed_cases)
rx$confirmed_cases<-as.integer(rx$confirmed_cases)

rx$discharged<-as.character(rx$discharged)
rx$discharged<-as.integer(rx$discharged)

rx$death<-as.character(rx$death)
rx$death<-as.integer(rx$death)

rx$admission<-as.character(rx$admission)
rx$admission<-as.integer(rx$admission)
rx$state<-as.character(rx$state)

############################################################
#### User Interface
############################################################
ui <- dashboardPage(
  
  # Add title of the App
  dashboardHeader(title = "Nigeria CoronaVirus Tracker"),
  
  # Add sidebar with several entries
  dashboardSidebar(
    sidebarMenu(
      
      # Add an entry for the worldwide view
      menuItem("Spatial Spread"
               , tabName = "spatial"
               , icon    = icon("globe-africa")
      ),
      
      # Add an entry for the country specific view
      menuItem("States"
               , tabName = "States"
               , icon    = icon("chart-line")
      )
    )
  ),
  
  # Add the main panel
  dashboardBody(theme=shinytheme("cyborg"),
                
                # We want to have multiple tabs
                tabItems(
                  
                  # Add a tab for the worldwide view
                  tabItem(tabName = "spatial",
                          
                          # A nice title
                          fluidRow(
                            column(12, align = "center", h1("Covid19 Daily Updates"))
                          ),
                          
                          # Some overview numbers
                          fluidRow(
                            
                            # Infobox about infections
                            infoBox("Total Confirmed Cases"
                                    , df$Confirmed
                                    , icon = icon("bug")
                                    , col = "purple",fill = TRUE),
                            
                            # Infobox about infections
                            infoBox("Active"
                                    , df$Active
                                    , icon = icon("bug")
                                    , col = "purple",fill = TRUE),
                            
                            # Infobox about deaths
                            infoBox("Deaths"
                                    , df$Death
                                    , icon = icon("skull")
                                    , col = "black",fill=TRUE),
                            
                            # Infobox about recoveries
                            infoBox("Recoveries"
                                    , df$Recovered
                                    , icon = icon("heart")
                                    , col = "red",fill=TRUE)
                          ),
                          
                          # Add a spatial visualization of the virus-spread
                          fluidRow(
                            column(12, leafletOutput("map"))
                          ),
                          
                          tabItem(tabName = "States",
                                  # Add a timeline for the cases by country
                          fluidRow(
                            column(12, plotOutput("countryPlot"))
                          )
                  )))))
 
############################################################
#### Server
############################################################
server <- function(input, output){
  
    output$map <- renderLeaflet({
      mypalette <- colorBin( palette="YlOrBr", domain=rx$confirmed_cases, na.color="transparent")
      
      # Prepare the text for the tooltip:
      mytext <- paste(
        "Recovered: ", rx$discharged, "<br/>", 
        "Deaths: ", rx$death, "<br/>", 
        "State: ", rx$state, "<br/>",
        "Confirmed: ", rx$confirmed_cases, sep="") %>%
        lapply(htmltools::HTML)
      
      # Final Map
       leaflet(rx) %>% 
        addTiles()  %>% 
        setView( lat=9.0820, lng=8.6753 , zoom=4) %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addCircleMarkers(~long, ~lat, 
                         fillColor = ~mypalette(confirmed_cases), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~confirmed_cases, opacity=0.9, title = "Confirmed_Cases", position = "bottomright" )
      
      
       
      })
  
  
  output$countryPlot <- renderPlot({
      ggplot(rx,aes(x=reorder(state,confirmed_cases),y=confirmed_cases))+
      geom_bar(stat="identity",fill=rainbow(n=length(rx$state)))+ylab("Confirmed Cases")+xlab("states")+
      coord_flip()+geom_text(aes(x=reorder(state,confirmed_cases),y=confirmed_cases,
                                  label=round(confirmed_cases)),position=position_dodge(width=0.3),hjust=0)+guides(fill=TRUE)+labs(title = "Total Covid19 Cases in Nigeria States")+
      dark_theme_light()
      
      
     
  })
}


############################################################
#### Run App
############################################################
shinyApp(ui = ui, server = server)











