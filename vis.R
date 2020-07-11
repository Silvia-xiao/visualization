#devtools::install_github("mattflor/chorddiag")

## app.R ##
library(shinydashboard)
library(chorddiag)
library(gpclib)
library(rgeos)
library(rgdal)
library(broom)
library(ggplot2)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(networkD3)
library(igraph)
library(tidygraph)
library(tidyverse)
library(plotly)
library(tableHTML)
ui <- dashboardPage(
  
  skin = "purple",
  dashboardHeader(title = "Viewing and Exploring!"),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
    menuItem("Overall", tabName = "overall", icon = icon("dashboard")),
    menuItem("Money & Happiness", tabName = "money", icon = icon("th")),
    menuItem("Health & Happiness", tabName = "health", icon = icon("th"))
  )),
  dashboardBody(
    tags$style(make_css(list('.box', 
                             c('font-size', 'font-family', 'color'), 
                             c('18px', 'Georgia', 'black')))),
    tabItems(
      # First tab content
      tabItem(tabName = "overall",
              fluidRow(
                h1("Happiness"),
                # Introduction of the whole visualization
                box(
                  title = "INTRODUCTION","Happiness is a very important index for everyone's life. 
                  We always say that 'I feel happy' or 'The people in some area cannot have happy life'. 
                  But actually, when thinking about happiness seriously, you will find it is really hard to say which make you happy.
                  In this data visualisation, I will try to show you some interesting relationship of happiness according to different factors.",br(),
                  "First of all, let's look at the self-reported happiness around the world and the trend in each region changed during these years. 
                    These charts are based on the data from World Happiness Report. In this report, data comes from a survey named Gallup World Poll.
                    In the survey, it asked some questions to evaluate the self-reported happiness score in different countries around the world.",br(),
                  "Now let's enter the happiness world!",
                  width = 12, status = "primary", solidHeader = TRUE
                )
              ),
              fluidRow(
                tabBox(
                  width=12,id = "tabset1",height = "700px",title = "WORLD HAPPINESS",
                  tabPanel("Map", 
                           "This map shows the 'happiness scores' published in the report, country by country. Different colors shows the happiness level--The bluer the happier.",br(),
                           "To interact with the map, you can use the time slicer to see the map in selected year and you can also click on any country you want to see the detail information",
                           br(),
                           "From the data showed in the map, we can know that people in Europe, Oceania and America are happier than people in other continents. And Africa is really not a happy place.",br(),
                           "You can also check the trend in each region during these years by clicking the second tab",br(),
                           "What affects the happiness?? You can check on the menu on the left to see the Money and Health condition both effects the happiness feeling.",
                           sliderInput("slider_map","Please select year:", 2005,2018,2018),
                           leafletOutput("tabset1Selected"),
                           "*Data source:Personal report happiness: https://worldhappiness.report/ed/2019/"
                           ),
                  tabPanel("Trend", 
                           "In this line chart, you can have a look on how is the happiness varies during recent years in different regions.",br(),
                           "To be honest, happy regions keep happy all the time and unhappy regions
                           didn't manage to be happier.Each region just fluctuates.",br(),
                           "In this line chart, you can choose the region you want to have a look or choose some regions to do the comparison",br(),
                           checkboxGroupInput("region", "Please choose regions to show:", 
                                              c("Australia and New Zealand","Central and Eastern Europe",
                                              "Eastern Asia","Latin America and Caribbean",
                                              "Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia",
                                              "Sub-Saharan Africa","Western Europe"),selected = 'Australia and New Zealand',inline = TRUE),
                           plotOutput("line"),
                           "*Data source:Personal report happiness: https://worldhappiness.report/ed/2019/"
                  ))),
              fluidRow(
                box(
                  title = "MIGRATION FLOW",
                  "In addition, if you have a look on the migration chord chart, it is obviously that the destinations people want to migrate to are Europe, 
                  North America and Oceania. For example, the number of migrating from Africa to Europe is 2107883, however only 176905 people move from Europe to Aferica.
                  This can also reflect people's opinions about which place is a better choice to have a happier life.",br(),
                  "In this chord chart, different colors represent different regions and there are four time button you can choose. 
                  And you can also click on the region name to see all the flows in that region or you can click on one flow to see the detailed information.",br(),
                  h1("Conclusion"),
                  "As we can see from these three charts, self-reported life satisfaction correlates with other measures of well-being—richer and healthier countries tend to have higher average happiness scores.",
                  width = 4,status = "primary",solidHeader = TRUE
                ),
                box(
                  title = "Migration FLOW","", width = 8,status = "primary",solidHeader = TRUE,
                  radioButtons('chord_select_year',"Select Year",inline = TRUE,
                               choices = c("1990","1995","2000","2005"),
                               selected = '2005'),
                  chorddiagOutput("chord", height = 600),
                  "*Data source: migration data: http://download.gsb.bund.de/BIB/global_flow/"
                )
              )
      ),
    
      # Second tab content
      tabItem(tabName = "money",
              h1("Money & Happiness"),
              fluidRow(
                box(
                  title = "HOW IS MONEY$ AFFECTS","People income affects Happiness","If you ask anybody questions about money, like 'Would you like to earn more money?' or
                  'Do you think you can have a better life with more money?'. The answer probably be positive. 
                  So let's have a look on how is the income affects people's feeling about happiness.",
                  width = 12,status = "primary", solidHeader = TRUE
                )
              ),
              fluidRow(
                box(
                  title = "PERSONAL INCOME WITH HAPPINESS",
                  "This scatter chart shows the relationship between personal income and happiness score. The colors represent different regions and each point represents one country. ",br(),
                  "There are some buttons represent different years and you can choose the year you want to see the data.",br(),
                  "You can single click on the point in front of region to remove some regions and do comparison or double click on the region you want to see to keep it only.",br(),
                  "You can also hover on the points to see the details",br(),
                  "As we can imagine, it is true that richer people are happier countries in most cases. But there is a really interesting thing.",br(),
                  "You can have a look on the Latin America region, you can find they are earier to be happy haha. Their gdp is quite low but they are really happy. Maybe the culture there also matters a lot.If you are interested, you can search more resources online. ",
                  status = "primary",solidHeader = TRUE,
                  radioButtons('gdpScatter_select_year',"Select Year",inline = TRUE,
                               choices = c("2005","2006","2007","2008","2009", "2010","2011","2012", "2013"),
                               selected = '2013'), width = 12,plotlyOutput("gdpScatter",height = 700),
                  "*Data source: Personal report happiness: https://worldhappiness.report/ed/2019/",br(),
                  "GDP: http://data.worldbank.org/data-catalog/world-development-indicators"
                )
              ),
              fluidRow(
                box(
                  title = "HOUSE PRICE",
                  "What about house price? People may think the house price may have adverse effects on happiness.Let's have a look.",br(),
                  "This map shows the average price per square meter for city centre flat in 2019, country by country. Different colours shows the house price level--The bluer the higher.",br(),
                  "To interact with the map, you can click on any country you want to see the detail information",br(),
                  "I think you got it! The data proves that there is a positive correlation between house price and happiness reported.
                  In the happiest region as we explored in last page—Western Europe, Oceania and North America, 
                  the house price is much higher. Why is this? You can find the bar chart below, it shows the house affordability in different areas.",
                  br(),width = 12,status = "primary", solidHeader = TRUE,
                  leafletOutput("housePriceMap"),
                  "*Data source: House price: https://www.bis.org/statistics/pp.htm",br(),
                )
              ),
              fluidRow(
                box(
                  title = "HOUSE AFFORDABILITY", 
                  "This bar chart shows how easy it is for the people in each region to buy a house. The top three regions also have higher house price.",br(),
                  "This means although house price is high, they are still eaiser to buy a house to live than other regions and they can own more property.",br(),
                  status = "primary", solidHeader = TRUE, width = 12,
                  imageOutput("picture", height = 600),
                  "*Data source: Affordability index: https://www.numbeo.com/property-investment/rankings_by_country.jsp",br(),
                )
              )
    
      ),
      # Third tab content
      tabItem(tabName = "health",
              fluidRow(
                h1("Health & Happiness"),
                box(
                  title = "3d Chart","Another big factor is Health issues. In this 3d chart, the data provides the evidence that when people's life expectancy is longer, healthcare expenditure is higher, people are happier.",br(), 
                  "This chart has 3 dimensions, one is self-reported happiness score, one is life expectancy in different countries, one is the health care expenditure.",br(),
                  "You can single click on the point in front of region to remove some regions and do comparison or double click on the region you want to see to keep it only.",br(),
                  "You can also hover on the points to see the details and select the year you want.",br(),
                  "The reason for this relationship is simple. For thousands of years, we human are trying to find cure for diseases so that we can live longer. 
                  Therefore, life expectancy can actually reflect the living condition of a region or country. If people have longer life, it means they have clean food and water, 
                  good health care, and also good mental health. In this kind of situation, no wonder people are happier.",br(),
                  radioButtons('scatter_select_year',"Select Year",inline = TRUE,
                               choices = c("2005","2006","2007","2008","2009", "2010","2011","2012", "2013"),
                               selected = '2013'),
                  plotlyOutput("exp", height = 1000),status = "primary", solidHeader = TRUE,
                  width = 12,
                  "*Data source: life expentancy: https://datasets.iisg.amsterdam/dataset.xhtml?persistentId=hdl:10622/LKYT53",br(),
                  "Healthcare expenditure: http://data.worldbank.org/data-catalog/world-development-indicators",br(),
                  "Personal report happiness: https://worldhappiness.report/ed/2019/"
                )
              )
              )
      
    )
  )
)

server <- function(input, output) {
  
  
  #MAP to show the overall happiness condition in different countries
  output$tabset1Selected <- renderLeaflet({
    #input$tabset1
    data <- read.csv("final.csv")
    initialData <- filter(data,year == input$slider_map)
    #initialData$location <- factor(initialData$location,levels=unique(initialData$location[order(initialData$latitude)]),ordered = TRUE)
    initialData$personal_report <-as.numeric(as.character(initialData$personal_report))/1 %>% round(2)
    world_spdf <- readOGR( 
      dsn=path.expand("./world_shape_file/"), 
      layer="TM_WORLD_BORDERS_SIMPL-0.3",
      verbose=FALSE
    )
    # Create a color palette with handmade bins.
    mybins <- c(2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,Inf)
    mypalette <- colorBin( palette="RdBu", domain=initialData$personal_report, na.color="transparent", bins=mybins)
    # Prepare the text for tooltips:
    mytext <- paste(
      "Country: ", initialData$NAME,"<br/>", 
      "Happiness: ", round(initialData$personal_report, 2), 
      sep="") %>%
      lapply(htmltools::HTML)
    # Final Map
    leaflet(world_spdf) %>% 
      addTiles()  %>% 
      setView( lat=10, lng=0 , zoom=2) %>%
      addPolygons( 
        fillColor =~mypalette(initialData$personal_report), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend( pal=mypalette, values=~initialData$personal_report, opacity=0.9, title = "Happiness Value", position = "bottomleft" )
  })
  
  
  #CHORD chart to show the migration flow
  output$chord <-renderChorddiag({
    mig_data <- read.csv("region_flow.csv")
    mig_data$region_orig <- as.factor(mig_data$region_orig)
    mig_data$region_dest <- as.factor(mig_data$region_dest)
    if(input$chord_select_year =="1990"){
      mig_data<-mig_data %>% select(region_orig, region_dest,regionflow_1990 )
      mig_data_filter<-as.matrix(as_adjacency_matrix(as_tbl_graph(mig_data),attr = "regionflow_1990"))
    }
    if(input$chord_select_year =="1995"){
      mig_data<-mig_data %>% select(region_orig, region_dest,regionflow_1995 )
      mig_data_filter<-as.matrix(as_adjacency_matrix(as_tbl_graph(mig_data),attr = "regionflow_1995"))
    }
    if(input$chord_select_year =="2000"){
      mig_data<-mig_data %>% select(region_orig, region_dest,regionflow_2000 )
      mig_data_filter<-as.matrix(as_adjacency_matrix(as_tbl_graph(mig_data),attr = "regionflow_2000"))
    }
    if(input$chord_select_year =="2005"){
      mig_data<-mig_data %>% select(region_orig, region_dest,regionflow_2005 )
      mig_data_filter<-as.matrix(as_adjacency_matrix(as_tbl_graph(mig_data),attr = "regionflow_2005"))
    }
    row.names(mig_data_filter) = c(colnames(mig_data_filter))
    # A vector of 10 colors for 10 groups
    haircolors <- c("North America",
                    "Africa",
                    "Europe",
                    "Fmr Soviet Union",
                    "West Asia",
                    "South Asia",
                    "East Asia",
                    "South-East Asia",
                    "Oceania",
                    "Latin America")
    dimnames(mig_data_filter) <- list(have = haircolors,
                                      prefer = haircolors)
    groupColors <- c("#CD3D0B","#EC8F00","#6EAE28","#684091","#B70375","#2158A5","#02A691","#009D3B","#388974","#FFCA00")
    # Build the chord diagram:
    chord <- chorddiag(mig_data_filter, groupColors = groupColors, 
                       groupnamePadding = 15,
                       groupPadding = 3,
                       groupnameFontsize = 12 ,
                       showTicks = FALSE,
                       margin=150,
                       tooltipGroupConnector = "    &#x25B6;    ",
                       chordedgeColor = "#B3B6B7")
    chord
  })
  
  
  #LINE chart to show the happiness trend in recent years.
  output$line <- renderPlot({
    data <- read.csv("final.csv")
    data$region<-as.character(data$region)
    data_new<-group_by(data,region,year)
    data_new_new <- summarise(data_new,happiness = mean(personal_report,na.rm=TRUE))
    plot_df<- filter(data_new_new, region %in% input$region)
    print(plot_df)
    plot1<-ggplot(plot_df,aes(x=year,y=happiness,group=region,color=region))+
      geom_line() +
      xlab("Year") + 
      ylab("Happiness score")
    plot1
  })
  
  
  #3D SCATTER plot to show the relationship among happiness, life expentancy and healthcare expenditure.
  output$exp <- renderPlotly({
    initialData <- read.csv("3d.csv")
    data_2015 <- initialData[which(initialData$year == input$scatter_select_year),]
    data_2015$size <- data_2015$personal_report
    print(data_2015)
    colors <- c("#CD3D0B","#EC8F00","#6EAE28","#684091","#B70375","#2158A5","#02A691","#009D3B","#388974","#FFCA00")
    fig <- plot_ly(data_2015, x = ~personal_report, y = ~life_expectancy, z = ~healthcare_expenditure, color = ~region, size = ~personal_report, colors = colors,
                   marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 55),
                   text = ~paste('Country:', NAME, '<br>Life Expectancy:', life_expectancy, '<br>Healthcare_exp:', healthcare_expenditure,
                                 '<br>Happiness.:', personal_report))
    fig <- fig %>% layout(title = 'Life Expectancy v. Happiness, 2015',
                          scene = list(xaxis = list(title = 'Happiness Score',
                                                    gridcolor = 'rgb(255, 255, 255)',
                                                    type = 'log',
                                                    zerolinewidth = 1,
                                                    ticklen = 5,
                                                    gridwidth = 2),
                                       yaxis = list(title = 'Life Expectancy (years)',
                                                    gridcolor = 'rgb(255, 255, 255)',
                                                    range = c(36.12621671352166, 91.72921793264332),
                                                    zerolinewidth = 1,
                                                    ticklen = 5,
                                                    gridwith = 2),
                                       zaxis = list(title = 'Expenditure per capita($)',
                                                    gridcolor = 'rgb(255, 255, 255)',
                                                    type = 'log',
                                                    zerolinewidth = 1,
                                                    ticklen = 5,
                                                    height = 4,
                                                    gridwith = 2)),
                          paper_bgcolor = 'rgb(243, 243, 243)',
                          plot_bgcolor = 'rgb(243, 243, 243)')
    
    fig
  })
  
  
  #SCATTER plot to show the relationship between people income and happiness.
  output$gdpScatter <- renderPlotly({
    gdpData <- read.csv("final.csv")
    filterData <- filter(gdpData,year == input$gdpScatter_select_year)
    colors <- c("#CD3D0B","#EC8F00","#6EAE28","#684091","#B70375","#2158A5","#02A691","#009D3B","#388974","#FFCA00")
    fig <- plot_ly(
      filterData, x = ~personal_report, y = ~gdp,
      # Hover text:
      text = ~paste("Country:",NAME,"<br>Happiness: ", filterData$personal_report, '<br>GDP($):', filterData$gdp),
      color = ~region, size = ~filterData$gdp, colors = colors,sizes = c(5, 55)
    )
    fig
  })
  
  output$housePriceMap <-renderLeaflet({
    #input$tabset1
    initialData <- read.csv("housepriceMap.csv")
    #initialData$location <- factor(initialData$location,levels=unique(initialData$location[order(initialData$latitude)]),ordered = TRUE)
    initialData$personal_report <-as.numeric(as.character(initialData$AveragePrice))/1 %>% round(2)
    world_spdf <- readOGR( 
      dsn=path.expand("./world_shape_file/"), 
      layer="TM_WORLD_BORDERS_SIMPL-0.3",
      verbose=FALSE
    )
    
    # Create a color palette with handmade bins.
    mybins <- c(500,1000,1500,2000,3000,5000,Inf)
    mypalette <- colorBin( palette="RdBu", domain=initialData$AveragePrice, na.color="transparent", bins=mybins)
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "Country: ", initialData$NAME,"<br/>", 
      "AveragePrice($): ", round(initialData$AveragePrice, 2), 
      sep="") %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>% 
      addTiles()  %>% 
      setView( lat=10, lng=0 , zoom=2) %>%
      addPolygons( 
        fillColor =~mypalette(initialData$AveragePrice), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend( pal=mypalette, values=~initialData$affordabilityIndex, opacity=0.9, title = "House price per square($)", position = "bottomleft" )
    
  })
  
  output$picture <- renderImage({
    filename <- normalizePath(file.path('./image',
                                        paste('house', input$n, '.jpeg', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         width = 1100,
         height = 500,
         alt = paste("Image number", input$n))
    
  }, deleteFile = FALSE)
  
  
}
shinyApp(ui, server)