# Amend this to store all the variables needed as outputs, then simply call them back in to plot etc. 
# Add a navigation panel that holds all the code used to generate the outputs. 
library("shiny")
library("leaflet")
library("DT") 
# Set working directory folder
#setwd("C:/Users/medsleea/Downloads/Dissertation/Concise Files/Leeds_LSOA_Carbon/")
#Loading Data Section
library("dplyr") # load dplyr package for joining datasets
library("MASS")  # load the MASS package for chi-squared
library("RVAideMemoire") #load the RVAideMemoire package for cramers test
library("ipfp") # load the ipfp package for IPF process
library("hydroGOF") #load the hydroGOF package for RMSE
library("reshape2") #load reshape for joining tables
library("ggplot2")
library("plotly") 
library("shinycustomloader")
library("shinycssloaders")
library("tidyverse")
# Define server logic required
shinyServer(function(input, output, session) {
  #### Previous Method where Data Cleaned in Excel (Amending as not Truly Open Science) ####
  #Load in the Understanding Society Wave 9 Dataset after Data Cleaning in Excel.
  ind <-read.csv("data//USD.csv")
  #Load in the Understanding Society Wave 9 Dataset constraint Data after Data Cleaning in Excel.
  ind_cat <- read.csv("data//USD_CAT.csv",colClasses=c('numeric'))
  #Load in the 2011 Census Dataset from Data Cleaning in Excel.
  cons_full <- read.csv("data//CENSUS.csv")
  #Load the chi squared dataset for statistically testing
  ind_freq <- read.csv("data//USD_CHI.csv")
  #Load the PCA Budget Data for VPCA creation. 
  PCA <- read.csv("data//PCA_Budgets.csv")
  Housing<-read.csv("data//Demographics.csv")
  DemographicsData<-read.csv("data//Demographics2.csv")
  # delete columns 1,2,3, to match variable number of 41 with ind_cat.
  cons <- cons_full[, -c(1:3)]
  #### Imported Data Tables Page ####
  #### Imported and Cleaned Table Plotting ####
  # Print Dataset depending on user input.
  DatasetInput<- reactive( {
    dataset <- as.name(input$Dataset)
  })
  # Add Title based on different table inputted.
  TextInput<- reactive( {
    if (input$Dataset=="ind") {
      text <- ("Individual Level Data from Understanding Society Wave 9 Dataset (444 suitable respondents)")
    }
    else if (input$Dataset=="cons_full") {
      text <- ("2011 Condensed LSOA Census for Leeds Council Region")
    }
    else if (input$Dataset=="cons") {
      text <- ("Long 2011 Census Data Table for Spatial Microsimulation")
    }
    else if (input$Dataset=="ind_cat") { 
      text <-("Long Individual Level Data Table for Spatial Microsimulation ") 
    }
    else if (input$Dataset =="PCA") {
      text <- ("Personal Carbon Allowance Budgets Used for Vulnerability Measure Creation")
    }
  })
  
  # Output the table and title from the reactive elements.
  output$Tablepage<-renderDT({DatasetInput()}, rownames=FALSE, extensions="Responsive")
  output$Table_Text <- renderUI({TextInput()})
  
  #### Data Analysis and Histograms Page - help on this one pls! ####
  #### Update Variables based on table inputted #####
  TableInput <- reactive ( { 
    analyse <- eval(as.name(input$Analysis))
    analysevar <-colnames(analyse)
  })
  observe({
    updateSelectInput(session, "AnalysisVar",
                      choices = as.character((TableInput())) # update choices
    )
  })
  
  #### Collect variable and dataframe for analysis bar chart and descriptive stats ####
  BarInput <- reactive ( { 
    bardf <- get(input$Analysis)
    barvariable<- bardf[,which(names(bardf)==input$AnalysisVar)]
    #print(barvariable)
    plotvariable<- gsub(" ", "", paste(bardf, "$", barvariable, collapse=""), fixed=TRUE)
    plotdata <- bardf %>% count(get(input$AnalysisVar)) # count instances of each 
    colnames(plotdata) <- c("Variable", "N")
    #print(plotdata)
    })
  # Testing box at bottom of page to see dateframe$variable to plot 
  output$plotvariable <- renderText({ 
    BarInput()})
  # Text box above Button showing which dataset and variable is plotted!
  output$histplotvar <- renderText({ 
    paste("Dataset Chosen: ", input$Analysis, ". Plotting Variable: ", input$AnalysisVar)})
  
  #### Bar chart and summary table When button is clicked. ####
  observeEvent(input$histbutton, {
    GGplot_data()
    output$analysisplot1 <- renderPlot(ggplot(data=GGplot_data()) + geom_bar(aes(x=Variable,y=N, fill=Variable), stat="identity") + labs(y="Count", x="Variable", title=Title()) + theme(legend.position="none") + coord_flip())
    Table_Summary()
    output$analysistable<-renderPrint({summary(Table_Summary())
    #output$analysisplot1<- renderPlot(p)
    })
  })
  
  # Only update the Table Summarised when input button clicked 
  Table_Summary <- eventReactive(input$histbutton, { 
    summarise_var <- eval(as.name(input$Analysis))
    })
  
  # Update Title for Plot when input button clicked 
  Title <- eventReactive(input$histbutton, { 
    paste("Plot showing Dataset of: ", input$Analysis, ". Plotting Variable: ", input$AnalysisVar)})
  
  # Only update the ggplot when input button clicked
  GGplot_data <- eventReactive(input$histbutton, { 
    BarInput()
    bardf <- get(input$Analysis)
    barvariable<- bardf[,which(names(bardf)==input$AnalysisVar)]
    #print(barvariable)
    plotvariable<- gsub(" ", "", paste(bardf, "$", barvariable, collapse=""), fixed=TRUE)
    plotdata <- bardf %>% count(get(input$AnalysisVar)) # count instances of each
    colnames(plotdata) <- c("Variable", "N")
    plotdata <- plotdata
    })
  
  # Produce cramer T Test for table inputted by User
  output$collinearity <-renderPrint({ 
    cramer.test((eval(as.name(input$ModelFit))), nrep=1000, conf.level=95)
  })
  
  #### Chi Squared Testing for Multi-collinearity ####
  #Chi squared Section#
  #Created tables for all variables included against Commuting Time (important in Personal Carbon Usage)
  # # the contingency table for age and commuting time
  tbl_agecom <- table(ind$Age, ind$Commuting.Time)
  # # the contingency table for sex and commuting time
  tbl_sexcom <- table(ind$ï..Sex, ind$Commuting.Time)
  # # the contingency table for ltd and commuting time
  tbl_ltdcom <- table(ind$LTD, ind$Commuting.Time)
  # # the contingency table for employment and commuting time
  tbl_empcom <- table(ind$Employment.Sector, ind$Commuting.Time)
  # # the contingency table for travel and commuting time
  tbl_travcom <- table(ind$Travel, ind$Commuting.Time)
  # #Found that Sex, Employment and Travel significant as <0.05, so test against each other to measure collinearity.
  tbl_sexemp <- table(ind$ï..Sex, ind$Employment.Sector)
  tbl_sextrav <- table(ind$ï..Sex, ind$Travel)
  tbl_travemp <- table(ind$Travel, ind$Employment.Sector)
  tbl_ltdemp <- table(ind$LTD, ind$Employment.Sector)
  tbl_ltdtrav <- table(ind$LTD, ind$Travel)
  tbl_ltdsex <- table(ind$LTD, ind$ï..Sex)
  
  # Read in the Leeds LSOA shapefile
  LSOA_shp <- st_read('data//LEEDS.shp')
  VAGG <- read.csv('data//Vulnerability_LSOA.csv')
  INCOMEAGG <- read.csv('data//Income_LSOA.csv')
  Demographics <- read.csv('data//Demographics_LSOA.csv')
  
  # Merge Spatial Microsimulation Data with LSOA_shp
  LSOA_shp_VAGG<- merge(LSOA_shp, VAGG, by.x='zone', by.y='Zone')
  LSOA_shp_INCAGG<- merge(LSOA_shp, INCOMEAGG, by.x='zone', by.y='zone')
  LSOA_shp_Demog <- merge(LSOA_shp, Demographics, by.x='zone', by.y='zone')
  # Geographic Transform from BNG to WGS84 for Leaflet Mapping
  LSOA_shp_VAGG = st_transform(LSOA_shp_VAGG, 4326)
  LSOA_shp_INCAGG = st_transform(LSOA_shp_INCAGG, 4326)
  # Library
  library(leaflet)
  #install.packages('crosstalk')
  library(crosstalk)
  
  # #### Create the PopUp Data ####
  
  VPCAtext <- paste(
    "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>",
    "2019 VPCA: ", round(LSOA_shp_VAGG$VPCAAGG,2)," (",
    round((LSOA_shp_VAGG$VPCAAGG-87.22),2), ifelse(round((LSOA_shp_VAGG$VPCAAGG-87.22),2)>0, "% Above ", "% Below "), "Leeds Average)",
    sep="") %>%
    lapply(htmltools::HTML)
  
  FMtext <- paste(
    "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>",
    "Proportion of Family Homes: ", round(LSOA_shp_INCAGG$FAMFAM,2)," (",
    round(LSOA_shp_INCAGG$FAMFAM-mean(LSOA_shp_INCAGG$FAMFAM),2), ifelse(round(LSOA_shp_INCAGG$FAMFAM-mean(LSOA_shp_INCAGG$FAMFAM),2)>0, "% Above ", "% Below "), "Leeds Average)",
    sep="") %>%
    lapply(htmltools::HTML)
  
  housetext <- paste(
    "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>",
    "House Price: £", round((LSOA_shp_INCAGG$HouseP1 *1000), 0)," (",
    "£", round((LSOA_shp_INCAGG$HouseP1-(mean(LSOA_shp_INCAGG$HouseP1)*1000)),0), ifelse(round((LSOA_shp_INCAGG$HouseP1-(mean(LSOA_shp_INCAGG$HouseP1)*1000)),0)>0, " Above ", " Below "), "Leeds Average)",
    sep="") %>%
    lapply(htmltools::HTML)
  
  inctext <- paste(
    "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>",
    "Income: £", round(LSOA_shp_INCAGG$INC,2)," (",
    "£" ,round(LSOA_shp_INCAGG$INC-mean(LSOA_shp_INCAGG$INC),2), ifelse(round(LSOA_shp_INCAGG$INC-mean(LSOA_shp_INCAGG$INC),2)>0, " Above ", " Below "), "Leeds Average)",
    sep="") %>%
    lapply(htmltools::HTML)
  
  disttext <- paste(
    "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>",
    "Distance to Work: ", round(LSOA_shp_Demog$Dist,2)," (",
    round(LSOA_shp_Demog$Dist-mean(LSOA_shp_Demog$Dist),2), ifelse(round(LSOA_shp_Demog$Dist-mean(LSOA_shp_Demog$Dist),2)>0, " Miles Above ", " Miles Below "), "Leeds Average)",
    sep="") %>%
    lapply(htmltools::HTML)
  
  vpistext <- paste(
    "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>",
    "VPIS: ", round(LSOA_shp_INCAGG$VPIS,2)," (",
    round(LSOA_shp_INCAGG$VPIS-mean(LSOA_shp_INCAGG$VPIS),2), ifelse(round(LSOA_shp_INCAGG$VPIS-mean(LSOA_shp_INCAGG$VPIS),2)>0, "% Above ", "% Below "), "Leeds Average)",
    sep="") %>%
    lapply(htmltools::HTML)
  
  pctetext <- paste(
    "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>",
    "Person Emissions on Work Travel: ", round(LSOA_shp_INCAGG$VPCE,2)," (",
    round(LSOA_shp_INCAGG$VPCE-mean(LSOA_shp_INCAGG$VPCE),2), ifelse(round(LSOA_shp_INCAGG$VPCE-mean(LSOA_shp_INCAGG$VPCE),2)>0, "KGCO2/yr Above ", "KGCO2/yr Below "), "Leeds Average)",
    sep="") %>%
    lapply(htmltools::HTML)
  
  hhetext <- paste(
    "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>",
    "Household Emissions: ", round(LSOA_shp_INCAGG$GCO2,2)," (",
    round(LSOA_shp_INCAGG$GCO2-mean(LSOA_shp_INCAGG$GCO2),2), ifelse(round(LSOA_shp_INCAGG$GCO2-mean(LSOA_shp_INCAGG$GCO2),2)>0, "KGCO2/yr Above ", "KGCO2/yr Below "), "Leeds Average)",
    sep="") %>%
    lapply(htmltools::HTML)
  
  a75text <- paste(
    "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>",
    "Proportion of Over 75s: ", round(LSOA_shp_Demog$A75,2)," (",
    round(LSOA_shp_Demog$A75-mean(LSOA_shp_Demog$A75),2), ifelse(round(LSOA_shp_Demog$A75-mean(LSOA_shp_Demog$A75),2)>0, "% Above ", "% Below "), "Leeds Average)",
    sep="") %>%
    lapply(htmltools::HTML)
  
  ltdtext <- paste(
    "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>",
    "Proportion of Long-Term Disabled Residents: ", round(LSOA_shp_Demog$LTD,2)," (",
    round(LSOA_shp_Demog$LTD-mean(LSOA_shp_Demog$LTD),2), ifelse(round(LSOA_shp_Demog$LTD-mean(LSOA_shp_Demog$LTD),2)>0, "% Above ", "% Below "), "Leeds Average)",
    sep="") %>%
    lapply(htmltools::HTML)
  
  c40text <- paste(
    "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>",
    "VPCA 40% Reduction: ", round(LSOA_shp_VAGG$VPCA402050AGG,2)," (",
    round(LSOA_shp_VAGG$VPCA402050AGG-mean(LSOA_shp_VAGG$VPCA402050AGG),2), ifelse(round(LSOA_shp_VAGG$VPCA402050AGG-mean(LSOA_shp_VAGG$VPCA402050AGG),2)>0, "% Above ", "% Below "), "Leeds Average)",
    sep="") %>%
    lapply(htmltools::HTML)
  
  c60text <- paste(
    "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>",
    "VPCA 60% Reduction: ", round(LSOA_shp_VAGG$VPCA602050AGG,2)," (",
    round(LSOA_shp_VAGG$VPCA602050AGG-mean(LSOA_shp_VAGG$VPCA602050AGG),2), ifelse(round(LSOA_shp_VAGG$VPCA602050AGG-mean(LSOA_shp_VAGG$VPCA602050AGG),2)>0, "% Above ", "% Below "), "Leeds Average)",
    sep="") %>%
    lapply(htmltools::HTML)
  
  c80text <- paste(
    "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>",
    "VPCA 40% Reduction: ", round(LSOA_shp_VAGG$VPCA802050AGG,2)," (",
    round(LSOA_shp_VAGG$VPCA802050AGG-mean(LSOA_shp_VAGG$VPCA802050AGG),2), ifelse(round(LSOA_shp_VAGG$VPCA802050AGG-mean(LSOA_shp_VAGG$VPCA802050AGG),2)>0, "% Above ", "% Below "), "Leeds Average)",
    sep="") %>%
    lapply(htmltools::HTML)
  
  # #### Create bins for all figures from Dissertation ####
  binsPCA2019 <- c(75, 80,85, 90, 95, 100) # Ok
  binsFM <-c(34, 38, 40, 42, 44, 46, 48, 50, 52) # Returns same values as DISS! Ok
  binshouse <- c(0, 80, 140, 200, 275, 383, 630) # Returns same values at DISS! Ok
  binsinc <-c(19000, 19500, 20000, 20500, 21000, 21500, 22000) # Ok
  binsdist <-c(12, 13, 14, 15, 16, 17, 18, 19, 20) # Ok
  binsvpis <-c(17, 17.5, 18, 18.5, 19, 19.5, 20) # Ok
  binspcte <-c(200, 220, 230, 240, 250, 260, 270, 280, 290) # Ok
  binshhe <- c(2800, 2900, 3000, 3100, 3200, 3300, 3400, 3500, 3600, 3700) # Ok
  bins75 <- c(0, 1, 1.5, 2, 3, 4, 5, 6) # Ok
  binsltd <-c(25, 27.5, 30, 32.5, 35) # Ok
  bins40 <- c(100, 110, 120, 130, 140) # Ok
  bins60 <- c(160, 170, 180, 190, 200, 210) # Ok
  bins80 <- c(320, 330, 340, 350, 360, 370, 380, 390, 400, 420) # Ok
  
  
  # #### Create colour palettes for all figures from Dissertation ####
  palPCA2019 <- colorBin("YlOrRd", domain = LSOA_shp_VAGG$VPCAAGG, bins = binsPCA2019) #Value DOESN'T match, min should be 75.6, max of 79.99
  palFM <- colorBin("YlOrRd", domain = LSOA_shp_INCAGG$FAMFAM, bins = binsFM) #Value matches Diss!
  palhouse <- colorBin("YlOrRd", domain = LSOA_shp_VAGG$HouseP1, bins=binshouse) #Value matches Diss!
  palinc <- colorBin("YlOrRd", domain = LSOA_shp_INCAGG$INC, bins = binsinc) #Value DOESN'T match, min should be 17093, max of 19639
  paldist <- colorBin("YlOrRd", domain = LSOA_shp_Demog$Dist, bins = binsdist)
  palvpis <- colorBin("YlOrRd", domain = LSOA_shp_INCAGG$VPIS, bins=binsvpis) #Value DOESN'T match, min should be 9.04, max of 18.49
  palpcte <- colorBin("YlOrRd", domain = LSOA_shp_INCAGG$VPCE, bins=binspcte) #Value DOESN'T match, min should be 89.74, max of 202.34
  palhhe <- colorBin("YlOrRd", domain = LSOA_shp_INCAGG$GCO2, bins=binshhe) #Value DOESN'T match, min should be 2803, max of 3016
  pal75 <- colorBin("YlOrRd", domain = LSOA_shp_Demog$A75, bins=bins75) #Value DOESN'T match, min should be 2, max of 16.7
  palltd <- colorBin("YlOrRd", domain = LSOA_shp_Demog$LTD, bins=binsltd) #Value DOESN'T match, min should be 3.8, max of 37.4
  pal40 <- colorBin("YlOrRd", domain = LSOA_shp_VAGG$VPCA402050AGG, bins=bins40) # Value DOESN'T match, min should be 105, max of 111
  pal60 <- colorBin("YlOrRd", domain = LSOA_shp_VAGG$VPCA602050AGG, bins=bins60) # Value DOESN'T match, min should be 157.5, max of 166.5
  pal80 <- colorBin("YlOrRd", domain = LSOA_shp_VAGG$VPCA802050AGG, bins=bins80) # Value DOESN'T match, min should be 314.9, max of 333.1
  
 helpfulmapinfo <- "Alter the map layer displayed to uncover additional insight,\nand hover over an LSOA to see its % compared to the average!\n\n 
 Layers available are: 
 
  * 2019 Carbon Vulnerability
 * Proportion of Family Homes
 * House Price
 * Income per Year
 * Distance to Work
 * Income Spent on Work Travel
 * Emissions Spent on Work Travel
 * Household Emissions
 * Proportion of over 75s
 * Proportion of Disabled Residents
 * Vulnerablility to 40% Carbon Reduction
 * Vulnerablility to 60% Carbon Reduction
 * Vulnerablility to 80% Carbon Reduction
 "
 output$helpfulmapinfo <- renderText(helpfulmapinfo)
  
  # #### Create Leaflet Interactive Map ####
  output$map <- renderLeaflet({
    leaflet(LSOA_shp_VAGG) %>%
      addTiles() %>%
      setView(lat=53.788036,lng=-1.559830, zoom=11) %>%
      addPolygons(
        fillColor=~palPCA2019(VPCAAGG),
        stroke=TRUE,
        fillOpacity=0.9,
        color="black",
        weight=0.1,
        opacity=1,
        group="2019 Carbon Budget Vulnerability",
        label=VPCAtext,
      ) %>%
      addPolygons(
        fillColor=~palFM(LSOA_shp_INCAGG$FAMFAM),
        stroke=TRUE,
        fillOpacity=0.9,
        color="black",
        weight=0.1,
        opacity=1,
        group="Proportion of Family Homes",
        label=FMtext,
      ) %>%
      addPolygons(
        fillColor=~palhouse(LSOA_shp_INCAGG$HouseP1),
        stroke=TRUE,
        fillOpacity=0.9,
        color="black",
        weight=0.1,
        opacity=1,
        group="House Price",
        label=housetext,
      ) %>%
      addPolygons(
        fillColor=~palinc(LSOA_shp_INCAGG$INC),
        stroke=TRUE,
        fillOpacity=0.9,
        color="black",
        weight=0.1,
        opacity=1,
        group="Income per Year",
        label=inctext,
      ) %>%
      addPolygons(
        fillColor=~paldist(LSOA_shp_Demog$Dist),
        stroke=TRUE,
        fillOpacity=0.9,
        color="black",
        weight=0.1,
        opacity=1,
        group="Distance to Work",
        label=disttext,
      ) %>%
      addPolygons(
        fillColor=~palvpis(LSOA_shp_INCAGG$VPIS),
        stroke=TRUE,
        fillOpacity=0.9,
        color="black",
        weight=0.1,
        opacity=1,
        group="Income Spent on Work Travel",
        label=vpistext,
      ) %>%
      addPolygons(
        fillColor=~palpcte(LSOA_shp_INCAGG$VPCE),
        stroke=TRUE,
        fillOpacity=0.9,
        color="black",
        weight=0.1,
        opacity=1,
        group="Emissions on Work Travel",
        label=pctetext,
      ) %>%
      addPolygons(
        fillColor=~palhhe(LSOA_shp_INCAGG$GCO2),
        stroke=TRUE,
        fillOpacity=0.9,
        color="black",
        weight=0.1,
        opacity=1,
        group="Household Emissions",
        label=hhetext,
      ) %>%
      addPolygons(
        fillColor=~pal75(LSOA_shp_Demog$A75),
        stroke=TRUE,
        fillOpacity=0.9,
        color="black",
        weight=0.1,
        opacity=1,
        group="Proportion of Over 75s",
        label=a75text,
      ) %>%
      addPolygons(
        fillColor=~palltd(LSOA_shp_Demog$LTD),
        stroke=TRUE,
        fillOpacity=0.9,
        color="black",
        weight=0.1,
        opacity=1,
        group="Proportion of Disabled Residents",
        label=ltdtext,
      ) %>%
      addPolygons(
        fillColor=~pal40(LSOA_shp_VAGG$VPCA402050AGG),
        stroke=TRUE,
        fillOpacity=0.9,
        color="black",
        weight=0.1,
        opacity=1,
        group="Vulnerability to 40% Carbon Reduction",
        label=c40text,
      ) %>%
      addPolygons(
        fillColor=~pal60(LSOA_shp_VAGG$VPCA602050AGG),
        stroke=TRUE,
        fillOpacity=0.9,
        color="black",
        weight=0.1,
        opacity=1,
        group="Vulnerability to 60% Carbon Reduction",
        label=c60text,
      ) %>%
      addPolygons(
        fillColor=~pal80(LSOA_shp_VAGG$VPCA802050AGG),
        stroke=TRUE,
        fillOpacity=0.9,
        color="black",
        weight=0.1,
        opacity=1,
        group="Vulnerability to 80% Carbon Reduction",
        label=c80text,
      ) %>%
      addLayersControl(
        overlayGroups = c("2019 Carbon Budget Vulnerability", "Proportion of Family Homes", "House Price", "Income per Year", "Distance to Work", "Income Spent on Work Travel", "Emissions on Work Travel", "Household Emissions", "Proportion of Over 75s", "Proportion of Disabled Residents", "Vulnerability to 40% Carbon Reduction","Vulnerability to 60% Carbon Reduction","Vulnerability to 80% Carbon Reduction" ),
        options= layersControlOptions(collapsed=TRUE)
      ) %>%
      addLegend(pal=palPCA2019, values=~VPCAAGG, opacity=0.9, title="VPCA 2019 (% Budget Spent, >100%= Over Budget)", position= "bottomleft", group="2019 Carbon Budget Vulnerability") %>%
      addLegend(pal=palFM, values=~LSOA_shp_INCAGG$FAMFAM, opacity=0.9, title="Proportion of Family Homes (%)", position= "bottomleft", group="Proportion of Family Homes") %>%
      addLegend(pal=palhouse, values=~LSOA_shp_INCAGG$HouseP1, opacity=0.9, title="House Price (£/Thousands)", position= "bottomleft", group="House Price") %>%
      addLegend(pal=palinc, values=~LSOA_shp_INCAGG$INC, opacity=0.9, title="Income Per Year (£/Thousands)", position= "bottomleft", group="Income per Year") %>%
      addLegend(pal=paldist, values=~LSOA_shp_Demog$Dist, opacity=0.9, title="Distance to Work (Miles)", position= "bottomleft", group="Distance to Work") %>%
      addLegend(pal=palvpis, values=~LSOA_shp_INCAGG$VPIS, opacity=0.9, title="Income Spent on Work Travel", position= "bottomleft", group="Income Spent on Work Travel") %>%
      addLegend(pal=palpcte, values=~LSOA_shp_INCAGG$VPCE, opacity=0.9, title="Emissions on Work Travel (KGCO2/yr)", position= "bottomleft", group="Emissions on Work Travel") %>%
      addLegend(pal=palhhe, values=~LSOA_shp_INCAGG$GCO2, opacity=0.9, title="Household Emissions (KGCO2/yr)", position= "bottomleft", group="Household Emissions") %>%
      addLegend(pal=pal75, values=~LSOA_shp_Demog$A75, opacity=0.9, title="Proportion of Over 75s (%)", position= "bottomleft", group="Proportion of Over 75s") %>%
      addLegend(pal=palltd, values=~LSOA_shp_Demog$LTD, opacity=0.9, title="Proportion of Long-Term Disabled Residents (%)", position= "bottomleft", group="Proportion of Disabled Residents") %>%
      addLegend(pal=pal40, values=~LSOA_shp_VAGG$VPCA402050AGG, opacity=0.9, title="Vulnerability to 40% Carbon Reduction", position= "bottomleft", group="Vulnerability to 40% Carbon Reduction") %>%
      addLegend(pal=pal60, values=~LSOA_shp_VAGG$VPCA602050AGG, opacity=0.9, title="Vulnerability to 60% Carbon Reduction", position= "bottomleft", group="Vulnerability to 60% Carbon Reduction") %>%
      addLegend(pal=pal80, values=~LSOA_shp_VAGG$VPCA802050AGG, opacity=0.9, title="Vulnerability to 80% Carbon Reduction", position= "bottomleft", group="Vulnerability to 80% Carbon Reduction") %>% 
      hideGroup("Proportion of Family Homes") %>% hideGroup("House Price") %>% hideGroup("Income per Year") %>% hideGroup("Distance to Work") %>% hideGroup("Income Spent on Work Travel") %>% hideGroup("Emissions on Work Travel") %>% hideGroup("Household Emissions") %>% hideGroup("Proportion of Over 75s") %>% hideGroup("Proportion of Disabled Residents") %>% hideGroup("Vulnerability to 40% Carbon Reduction") %>% hideGroup("Vulnerability to 60% Carbon Reduction")%>% hideGroup("Vulnerability to 80% Carbon Reduction")

  })
  
})
