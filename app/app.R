# DTSC 630 - M01/Spring 2022
# Data Visualization
# Dr. Cheng
# Team Members: Michael Trzaskoma, Hui Chen, Bofan He
# webdemo: https://bofan.shinyapps.io/DTSC630/

############################################################################
# Project Name: Job skillset seeking recommender	

# Project Description:

# In this project, we are going to build a web-server based job skillset recommendation engine. 
# The dataset is from Kaggle 
# (URL: https://www.kaggle.com/code/rayjohnsoncomedy/job-skills/data?select=job_skills.csv) 
# with 1250 records and 7 features. The users would need to input their skillset(s) 
# in order to find the optimal job/title/position by our recommendation engine. 
# Also, the interactive visualization graphs will be used in this project are as follows:
# Word Cloud
# Pie chart
# Radar Charts

############################################################################
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
############################################################################
projectName <- c("Job Skillset Seeking Recommender")


library(shiny)
library(plotly)

##################################wordcloud#############################
# Load pkg
library(reshape)
library(tm)
library(wordcloud)






##################################wordcloud#############################





# Define UI for application that draws a histogram
ui <- navbarPage(title = "DTSC 630 - M01/Spring 2022",
                 ##################################Graghic Page#############################
                   tabPanel("Graphic", fluidPage(
                       
                       # Application title
                       titlePanel(projectName),
                       
                       # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                           sidebarPanel(
                               # Add weidgts
                               
                               # fluidRow(
                               #     column(3,
                               #            checkboxGroupInput("checkGroup",
                               #                               h3("Select Program languages skillset(s):"),
                               #                               choices = list("Python" = "Python",
                               #                                              "R" = "R",
                               #                                              "Java" = "Java",
                               #                                              "SQL" = "SQL",
                               #                                              "C++" = "C++"),
                               #                               selected = "Python")),
                               # ),
                               
                               
                               # multi sel dropdown
                               fluidRow(
                                   selectInput("skill", "Choose your skills:",multiple = TRUE,
                                               list(`Programing Language` = list("Python", "C++", "Java","R"),
                                                    `Machine learning` = list("OpenCV", "SVM", "CNN","NLP","RNN"))
                                   ),
                               ),
                               
                               ##################################wordcloud#############################
                               # Sidebar with a slider and selection inputs
                               fluidRow(
                                   selectInput("selection", "Choose a job title:",
                                               choices = jobs),
                                   actionButton("update", "Change"),
                                   hr(),
                                   sliderInput("freq",
                                               "Minimum Frequency:",
                                               min = 1,  max = 50, value = 15),
                                   sliderInput("max",
                                               "Maximum Number of Words:",
                                               min = 1,  max = 300,  value = 100)
                               ),
                               ##################################wordcloud#############################
                               
                           ),
                           
                           # Show a plot 
                           mainPanel(
                               tabsetPanel(
                                   tabPanel("user selection1",textOutput("selecteds_sk1")),
                                   tabPanel("user selection2",textOutput("result")), # multi sel dropdown
                                   tabPanel("word cloud",plotOutput("plot")), # word cloud
                                   tabPanel("radar chart",plotlyOutput("plot1", width = 800, height=700),
                                            p("To visualize the graph of the job, click the icon at side of names 
             in the graphic legend.",
                                              style = "font-size:25px")) # radar chart
                               )
                           )
                       )
                   )),
                 ##################################Graghic Page#############################
                 
                 ##################################About Page###############################
                   tabPanel("About", 
                            
                            h4("Group Project Info:"),
                            p("DTSC 630 - M01/Spring 2022"),
                            p("Data Visualization"),
                            p("Dr. Cheng"),
                            p(a("Demo Web", href="https://bofan.shinyapps.io/DTSC630/")),

                            
                            hr(),
                            
                            h4("Project Description:"),
                            
                            p("In this project, we are going to build a web-server based job skillset recommendation engine.
                            The dataset is from Kaggle", a("job-skills",href="https://www.kaggle.com/code/rayjohnsoncomedy/job-skills/data?select=job_skills.csv" , target="_blank"),
                            "with 1250 records and 7 features. The users would need to input their skillset(s) in order to find the optimal job/title/position by our recommendation engine. "),

                            hr(),
                            h5("Also, the interactive visualization graphs will be used in this project are as follows:"),
                            p("Word Cloud"),
                            p("Pie chart"),
                            p("Radar Charts"),

                            ),
                 
                 ##################################About Page###############################
                 
                   tabPanel("Team", 
                            p("Hui(Henry) Chen",style = "font-size:25px"),
                            p("email: hchen60@nyit.edu"),
                            p("Bofan He",style = "font-size:25px"),
                            p("email: bhe@nyit.edu"),
                            p("Michael Trzaskoma",style = "font-size:25px"),
                            p("email: mtrzasko@nyit.edu"),
                            )
                 )
    

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$selecteds_sk1 <- renderText({ 
        paste("You have selected", input$checkGroup)
    })
    
    # multi sel dropdown
    output$result <- renderText({
        paste("You chose", input$skill)
    })
    
    ##################################radar chart#############################
    output$plot1 <- renderPlotly({
        
        plot_ly(
            type = 'scatterpolar',
            r = c(39, 28, 8, 7, 28, 39),
            theta = c('programing','Data Analysis','Project Management', 'Degree', 'years of experience', 'Salary'),
            name = 'Job A',
            fill = 'toself'
        ) %>%
            add_trace(
                r = c(1.5, 10, 39, 31, 15, 1.5),
                theta = c('programing','Data Analysis','Project Management', 'Degree', 'years of experience', 'Salary'),
                name = 'Job B'
            ) %>%
        
        layout(
            polar = list(
                radialaxis = list(
                    visible = T,
                    range = c(0,50)
                )
            ),
            
            showlegend=TRUE
            
            
        )
    })
    
    ##################################radar chart#############################
    
    ##################################wordcloud#############################
    # Define a reactive expression for the document term matrix
    terms <- reactive({
        # Change when the "update" button is pressed...
        input$update
        # ...but not for anything else
        isolate({
            withProgress({
                setProgress(message = "Processing corpus...")
                getTermMatrix(input$selection)
            })
        })
    })
    
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
        v <- terms()
        wordcloud_rep(names(v), v, scale=c(4,0.5),
                      min.freq = input$freq, max.words=input$max,
                      colors=brewer.pal(8, "Dark2"))
    })
    
    ##################################wordcloud#############################
    
}

# Run the application 
shinyApp(ui = ui, server = server)
