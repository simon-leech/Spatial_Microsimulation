library("shiny")
library("shinythemes")
library("DT")
library("leaflet")
library("sf")
# Define UI for application that draws a histogram
navbarPage(theme=shinytheme("flatly"), "Leeds LSOA Vulnerability",
           tabPanel("Homepage",
                    # Application title
                    titlePanel("This Dashboard is built using data collected and analysed from Understanding Society Wave 9, and Census 2011 Datasets"),
                    tags$a(href="https://github.com/simonl11/Spatial_Microsimulation", "Click here to view the R scripts used to generate the data for Spatial Microsimulation!"),
                    HTML("<ul><li>This data forms the basis of my University of Leeds Masters Dissertation 2019-2020.</li><li>The purpose of this was to investigate the Vulnerability of the Population of Leeds (~750,000) to the introduction of a Personal Carbon Allowance using Understanding Society Wave 9 Dataset.</li><li>The overall model fit was calculated at: 0.936, with RE of 14.4%.</li></ul>"),
                    tags$a(href="https://spatial-microsim-book.robinlovelace.net/index.html", "Click here for the Book used for Iterative Proportional Fitting (Spatial Microsimulation)"),
                         ),
           # In here I want to be able to simply view the imported table- to understand the data.
           tabPanel("Data Tables",
                    fluidRow(selectInput("Dataset", "Choose dataset to display: ", c("Individual Dataset"="ind", "Individual Dataset for Spatial Microsimulation"="ind_cat", "2011 Census Dataset"="cons_full", "2011 Census Dataset for Spatial Microsimulation"="cons", "Personal Carbon Allowance Budgets"="PCA"))),
                    span(uiOutput('Table_Text'), style="font-size: 30px"),
                    withSpinner(DTOutput('Tablepage')), type=8),
           # In here I want to produce histograms for each vulnerability measure.
           tabPanel("Data Plots", 
                    sidebarLayout(
                      sidebarPanel("Choose Dataset and Variable to Analyse",
                      fluidRow(selectInput("Analysis", "Choose dataset to analyse: ", c("Individual Dataset"="ind", "Individual Dataset for Spatial Microsimulation"="ind_cat", "2011 Census Dataset"="cons_full", "2011 Census Dataset for Spatial Microsimulation"="cons","Leeds LSOA Spatial Microsimulation Population"="PCA", "Leeds LSOA Spatial Microsimulation Demographics"="DemographicsData", "Leeds LSOA External Validation Data"="Housing" ))), 
                      fluidRow(selectInput("AnalysisVar", "Choose a variable to analyse: ", choices=NULL)),
                      uiOutput('histplotvar'),
                      actionButton("histbutton", "Click here to generate descriptive statistics and plots!")),
                    mainPanel("",
                      withSpinner(plotOutput("analysisplot1"), type=8),
                    verbatimTextOutput("analysistable"),
                    uiOutput("plotvar")))),
           # In here I want to show Model Fit variables, and multicollinerarity tables.
           tabPanel("Multi-Collinearity Check", 
                    sidebarLayout(
                    sidebarPanel("Select Variable for Analysis",
                      fluidRow(selectInput("ModelFit", "Choose Variables for Individual Dataset to compare: ",c("Age vs Commuting Time"="tbl_agecom", "Sex vs Commuting Time"="tbl_sexcom", "LTD vs Commuting Time"="tbl_ltdcom", "Employment Sector vs Commuting Time"="tbl_empcom", "Travel vs Commuting Time"= "tbl_travcom", "Sex vs Employment Sector"="tbl_sexemp", "Sex vs Travel"="tbl_sextrav", "Travel vs Employment Sector"="tbl_travemp", "LTD vs Employment Sector"="tbl_ltdemp", "LTD vs Travel"="tbl_ltdtrav", "LTD vs Sex"="tbl_ltdsex") ))),
                    mainPanel("Collinearity",
                    withSpinner(verbatimTextOutput("collinearity"),type=8))),
           ),
           
           # In here I want to be able to show the Leaflet map already created in Amended_Microsim.R
           tabPanel("Leaflet Visualisations", 
                    sidebarLayout(
                      sidebarPanel("Helpful Information",
                      verbatimTextOutput("helpfulmapinfo")),
                      mainPanel("",
                    withSpinner(leafletOutput("map", height="800px", width="1000px"),type=8))),
           ) 
)
