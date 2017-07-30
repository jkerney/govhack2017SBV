###Shiny App Govhack 2017###
#Data: NEIS
#Shapefile ABS

library(shiny)
library(data.table)
library(shinythemes)
library(ggplot2)
library(leaflet)
library(rgdal)
library(tidyverse)
library(ggthemes)
library(scales)
library(stringr)
#install.packages("RColorBrewer")

# Read in Map Data 
shapes <- readOGR("map.GeoJSON")
shapes$states <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "Other")

indData <- fread("combined.csv")


# Read in neis data
df <- fread("Data.csv")
df$year <- str_sub(df$start_date,-2,9)
df <- filter(df, year %in% c(13:17))
df$metro <- as.factor(df$metro)

df <- select(df,
              successful,
              industry_type,
              state,
              metro,
              age_group)

## summarise success rates for each combination of variables
resultsSummary <- group_by(df, 
                           industry_type,
                           state,
                           metro,
                           age_group) %>%
  summarise(PercentSuccessful = sum(successful == "Y")/n(), Count = n())

stateSummary <- group_by(df, 
                           industry_type,
                           state,
                           metro) %>%
  summarise(PercentSuccessful = sum(successful == "Y")/n(), Count = n())

## Work out percentile
resultsSummary <- arrange(resultsSummary, PercentSuccessful)
resultsSummary$rank <- seq(1:nrow(resultsSummary))
resultsSummary$Percentile <- resultsSummary$rank / nrow(resultsSummary) * 100

Groups <- c("Businesses Similar\nTo You", "Your Industry", "Your Age Group", "Your State", "National")

# Define Input Choices
#Hours <- c("0 - 15", "15 - 30", "30 - 40", "40 - 50", "50+")
AgeGroup <- c("15-19" = "15_19", "20-24" = "20_24", "25-29" = "25_29",  "30-34" = "30_34", "35-39" = "35_39", "40-44" = "40_44","45-49" = "45_49", "50-54" = "50_54", "55-59" = "55_59", "60+")
Industry <- c("Accomodation, Cafe's and Restaurants" = "accommodation_cafes_restaurants", "Agriculture, Forestry and Fishing" = "agriculture_forestry_fishing",  "Communication" = "communication", "Construction" = "construction",  "Cultural and Recreational" = "cultural_and_recreational", 
              "Education" = "education", "Electricity, Gas and Water" = "electricity_gas_water", "Finance and Insurance" =  "finance_and_insurance", "Health and Community" = "health_and_community", "Manufacturing" = "manufacturing", "Mining" = "mining", "Other" = "other", "Personal and Other" = "personal_and_other", 
              "Property and Business" = "property_and_business", "Retail and Trade" = "retail_trade", "Transport" = "transport",
              "Wholesale Trade" = "wholesale_trade")


############################Start of App#################
ui <- fluidPage(theme =  shinytheme("cerulean"),
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                ),
                conditionalPanel(condition = 'input.goButton == 0',
                                 fluidRow(column(4, offset = 2, align = "centre",
                                                 img(src = "logo1.png", width = 450, height = 450)),
                                   column(4, align = "centre", h1("Details"),
                                                 selectInput("inputIndustry", "Choose Industry", choices = c("Choose an Industry", Industry)),
                                                 selectInput("inputState", "Choose State", choices = c("Choose a State", unique(df$state))),
                                                 selectInput("inputMetro", "Choose Metro Area", choices = c("Choose Urbanality", "Metropolitan" = "1", "Regional" = "0")),
                                                 selectInput("inputAge", "Choose Age Group", choices = c("Choose Age Group", AgeGroup)),
                                                 conditionalPanel('input.inputIndustry != "Choose an Industry" & input.inputState != "Choose a State" & input.inputMetro != "Choose Urbanality" & input.inputAge != "Choose Age Group" & input.inputHours != "Choose Hours Per Week"',
                                                                  actionButton("goButton", "Go", icon("suitcase")))                               
                                          )
                                   )
      ),
                #######2nd Page: Results##############
                conditionalPanel(condition = 'input.goButton >= 1',
                                 headerPanel(title = ""),
                                 fluidRow(
                                   column(12,align="center",h1(textOutput("score")))
                                 ),
                                 fluidRow(
                                   column(6, offset = 3, align="center",h2("Your Business Score"))
                                 ),
                                 fluidRow(
                                   column(12,align="center",h2("How Does It Compare?"))
                                 ),
                                 fluidRow(
                                   column(6, offset = 3, align="center",plotOutput("plotCompare"))
                                 ),
                                 fluidRow(
                                   column(12,align="center",h2(textOutput("forecastTitle")))
                                 ),
                                 fluidRow(
                                   column(6, offset = 3, align="center",plotOutput("plotForecast"))
                                 ),
                                 fluidRow(
                                   column(12,align="center",h2(textOutput("mapTitle")))
                                 ),
                                 fluidRow(
                                   column(6, offset = 3, align="center",leafletOutput("map"))
                                 ),
                                 fluidRow(
                                  column(12,align="center",h2(textOutput("compareIndTitle")))
                                 ),
                                 fluidRow(
                                   column(6, offset = 3, align="center",plotOutput("plotInd"))
                                 )
                )
)

server <- function(input, output) {
  
  user <- reactive(c(input$inputIndustry, input$inputState, input$inputMetro, input$inputAge))
  
  userResult <- reactive({filter(resultsSummary, industry_type == user()[1], 
                                 state == user()[2], 
                                 metro == user()[3],
                                 age_group == user()[4])
    })
  
  successScore <- reactive({
    round(userResult()$Percentile/10, 0)
    })
  
  successPlotDf <- reactive({
    successPlotDf <- data.frame(group = Groups, successRate = rep(0,5))
    successPlotDf$group = factor(successPlotDf$group,levels(successPlotDf$group)[c(1,4,3,5,2)])
    successPlotDf[1,]$successRate <- userResult()$PercentSuccessful
    successPlotDf[2,]$successRate <- filter(df, industry_type == user()[1]) %>%
      summarise(PercentSuccessful = sum(successful == "Y")/n(), Count = n())
    successPlotDf[3,]$successRate <- filter(df, age_group == user()[4]) %>%
      summarise(PercentSuccessful = sum(successful == "Y")/n(), Count = n())
    successPlotDf[4,]$successRate <- filter(df, state == user()[2]) %>%
      summarise(PercentSuccessful = sum(successful == "Y")/n(), Count = n())
    successPlotDf[5,]$successRate <- summarise(df, PercentSuccessful = sum(successful == "Y")/n(), Count = n())
    return(successPlotDf)
  })
  
  successPlotInds <- reactive({
    successPlotInd <- filter(df, state== user()[2]) %>%
      group_by(industry_type) %>%
      summarise(PercentSuccessful = sum(successful == "Y")/n(), Count = n()) %>%
      arrange(PercentSuccessful)
    
    successPlotInd$niceIndustry <- "test"
    
    ## Make industry names nicer
    successPlotInd[successPlotInd$industry_type == "electricity_gas_water",]$niceIndustry <- "Electricity, Gas & Water"
    successPlotInd[successPlotInd$industry_type == "other",]$niceIndustry <- "Other"
    successPlotInd[successPlotInd$industry_type == "transport",]$niceIndustry <- "Transport"
    successPlotInd[successPlotInd$industry_type == "finance_and_insurance",]$niceIndustry <- "Finance & Industry"
    successPlotInd[successPlotInd$industry_type == "retail_trade",]$niceIndustry <- "Retail/Trade"
    successPlotInd[successPlotInd$industry_type == "personal_and_other",]$niceIndustry <- "Personal & Other"
    successPlotInd[successPlotInd$industry_type == "health_and_community",]$niceIndustry <- "Health & Community"
    successPlotInd[successPlotInd$industry_type == "communication",]$niceIndustry <- "Communication"
    successPlotInd[successPlotInd$industry_type == "manufacturing",]$niceIndustry <- "Manufacturing"
    successPlotInd[successPlotInd$industry_type == "cultural_and_recreational",]$niceIndustry <- "Cultural & Recreactional"
    successPlotInd[successPlotInd$industry_type == "construction",]$niceIndustry <- "Construction"
    successPlotInd[successPlotInd$industry_type == "education",]$niceIndustry <- "Education"
    successPlotInd[successPlotInd$industry_type == "agriculture_forestry_fishing",]$niceIndustry <- "Agriculture, Forestry & Fishing"
    successPlotInd[successPlotInd$industry_type == "accommodation_cafes_restaurants",]$niceIndustry <- "Accomodation, Cafes, Restaurants"
    successPlotInd[successPlotInd$industry_type == "wholesale_trade",]$niceIndustry <- "Wholesale Trade"
    successPlotInd[successPlotInd$industry_type == "property_and_business",]$niceIndustry <- "Property & Business"
    
    successPlotInd <- filter(successPlotInd, niceIndustry != "test")
    
    temp <- select(successPlotInd, niceIndustry, PercentSuccessful) %>%
      arrange(PercentSuccessful)
    
    successPlotInd <- within(successPlotInd, 
                             niceIndustry <- factor(niceIndustry, 
                            levels=temp$niceIndustry))
    
    
    print(temp)
    
    print(successPlotInd)
    successPlotInd
  })
  
  indEmpDf <- reactive({
    indEmpDf <- select(indData, date, one_of(user()[1]))
    colnames(indEmpDf) <- c("date","numPeople")
    indEmpDf$row <- 1:nrow(indEmpDf)
    indEmpDf$forecast <- indEmpDf$numPeople
    indEmpDf[1:41,]$forecast <- NA
    indEmpDf[42:nrow(indEmpDf),]$numPeople <- NA
    indEmpDf$month <- rep(c("05","08","11","02"), ceiling(nrow(indEmpDf)/4))[1:53]
    indEmpDf$year <- str_sub(indEmpDf$date, -2,6)
    indEmpDf$date <- paste("1", indEmpDf$month, indEmpDf$year, sep="-")
    indEmpDf$date <- as.Date(indEmpDf$date, format="%d-%m-%y")
    indEmpDf
  })

  minGrowth <- reactive({min(successPlotDf()$successRate) - .05})
  maxGrowth <- reactive({max(successPlotDf()$successRate) + .05})
  
  output$score <- renderText(successScore())
  
  output$plotCompare <- renderPlot({
    A <- as.data.frame(successPlotDf())
    B <- data.frame(group = unlist(A[,1], recursive = TRUE, use.names = F),
                    successRate = unlist(A[,2], recursive = TRUE, use.names = F))
    ggplot(data=B) +
      geom_bar(mapping=aes(x=group, y=successRate, fill=rainbow(5)), stat="identity") +
      theme_fivethirtyeight() +
      scale_fill_discrete(guide=FALSE) +
      labs(title="Success Rate of NEIS Small Businesses\nin the Same Industry, Age Group, State,\n and Metro/Regional, 2013-2017", caption="Data Source: Historical New Enterprise Incentive Scheme Business Level Data") +
      scale_y_continuous(labels=percent) +
      coord_cartesian(ylim=c(0,min(c(max(B$successRate), 1)) + 0.5)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.caption = element_text(size=7, face="italic"))
    })
  
  output$forecastTitle <- renderText(paste(names(Industry[Industry == input$inputIndustry] ), "in 2020"))
  
  
  output$plotForecast <- renderPlot({ ###### Waiting for Stuff
    successPlotInds()
    ggplot(data=indEmpDf()) +
      geom_line(mapping=aes(x=date,y=numPeople), colour="darkblue") +
      geom_line(mapping=aes(x=date,y=forecast), colour="red", linetype="dashed") +
      geom_vline(mapping=aes(xintercept = as.numeric(as.Date("2017-06-15", format="%Y-%m-%d"))), colour="red") +
      theme_fivethirtyeight() +
      labs(title="Your Industry's Forecasted Size in 2020", x="Year",y="Total Employment (thousands)", caption="Data Source: ABS Labour Force Region - SA4 Data") +
      theme(plot.caption = element_text(size=7, face="italic"))
    
    })
  
  userStateResult <- reactive({filter(stateSummary, industry_type == user()[1])
  })
  
  output$compareIndTitle <- renderText(paste(input$inputState, "Industries by Success"))
  
  
  output$plotInd <- renderPlot({
    A <- data.frame(successPlotInds())
    print(A$Count)
    print(typeof(A$Count))
    ggplot(data=A) +
      geom_bar(mapping=aes(x=niceIndustry, y=PercentSuccessful, fill=industry_type==user()[1]), stat="identity") +
      theme_fivethirtyeight() +
      scale_fill_discrete(guide=FALSE) +
      labs(title="Success Rate of NEIS\nSmall Businesses Owners\nBy Industry For Your State\n2013-2017", caption="Data Source: Historical New Enterprise Incentive Scheme Business Level Data") +
      scale_y_continuous(labels=percent) +
      coord_cartesian(ylim=c(0,maxGrowth)) +
      #theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.caption = element_text(size=7, face="italic")) +
      coord_flip()
    
  })
  
  mapData <- reactive({
    A <- as.data.frame(userStateResult())
    A <- data.frame(state = unlist(A[,2], recursive = TRUE, use.names = F),
                    successRate = unlist(A[,4], recursive = TRUE, use.names = F))
    B <- data.frame(state = c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "Other"))
    B <- left_join(B, A, by="state")
    B[is.na(B$successRate),]$successRate <- mean(B$successRate, na.rm=TRUE)
    sqrt(B$successRate)
  })
  
  output$mapTitle <- renderText(paste(names(Industry[Industry == input$inputIndustry] ), "Small Business Success by State/Territory"))
  
  
  output$map <- renderLeaflet({
    cpal <- colorNumeric("YlGnBu", mapData())
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(137,-32,3.5)%>%
      addPolygons(color = ~cpal(mapData()), layerId=~states, weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5, label = ~states, data = shapes)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)