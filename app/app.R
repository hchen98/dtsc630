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

################################# DF ###################################
<<<<<<< HEAD
key_words <- read.csv('Categorized List_2.csv')
=======
#key_words <- read.csv('/Users/miketrz/Downloads/DTSC_630_DAta/Categorized List_2.csv')
key_words <- Categorized.List_2

>>>>>>> 03fbcdc46c6312de024f8fa337342f326621bf2c

# key_words <- Categorized.List_2
major_words = key_words$Key_Words[key_words$Grouping == 'major']
tool_words = key_words$Key_Words[key_words$Grouping == 'tool']
trait_words = key_words$Key_Words[key_words$Grouping == 'trait']
spec_words = key_words$Key_Words[key_words$Grouping == 'specialty']
env_words = key_words$Key_Words[key_words$Grouping == 'environment']

list_of_words = c()
user_words = c()
list = c('computer science','something' ,'sales')


values = c(5,6,7,8,9,10,11)

<<<<<<< HEAD
data <- read.csv('Categories_KW_Normalized_test.csv')
# data <- Categories_KW_Normalized_test
=======
#data <- read.csv('/Users/miketrz/Downloads/DTSC_630_DAta/Categories_KW_Normalized_test.csv')
data <- Categories_KW_Normalized_test

>>>>>>> 03fbcdc46c6312de024f8fa337342f326621bf2c
df <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(df)<-c('Cateogry', 'Major', 'Major_W', 'Specialty', 'Specialty_W', 
                'Tool', 'Tool_W', 'Trait', 'Trait_W', 'Environment', 'Environment_W')

#New csv creation
for(i in 1:nrow(data)) {
  
  category<- data$Category[i]# for-loop over rows
  
  major<- as.list(strsplit(data$Major, "', '")[[i]]) 
  major<- c(lapply(major, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  #print(major)
  
  major_w<- as.list(strsplit(data$Major_Weights, "', '")[[i]]) 
  major_w<- c(lapply(major_w, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  specialty<- as.list(strsplit(data$Specialty, "', '")[[i]]) 
  specialty<- c(lapply(specialty, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  specialty_w<- as.list(strsplit(data$Specialty_Weights, "', '")[[i]]) 
  specialty_w<- c(lapply(specialty_w, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  tool<- as.list(strsplit(data$Tool, "', '")[[i]]) 
  tool<- c(lapply(tool, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  tool_w<- as.list(strsplit(data$Tool_Weights, "', '")[[i]]) 
  tool_w<- c(lapply(tool_w, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  trait<- as.list(strsplit(data$Trait, "', '")[[i]]) 
  trait<- c(lapply(trait, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  trait_w<- as.list(strsplit(data$Trait_Weights, "', '")[[i]]) 
  trait_w<- c(lapply(trait_w, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  env<- as.list(strsplit(data$Environment, "', '")[[i]]) 
  env<- c(lapply(env, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  env_w<- as.list(strsplit(data$Environment_Weights, "', '")[[i]]) 
  env_w<- c(lapply(env_w, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  df[i, ]<- list(category, list(major),list(as.numeric(major_w)), list(specialty), 
                 list(as.numeric(specialty_w)), list(tool), list(as.numeric(tool_w)), 
                 list(trait), list(as.numeric(trait_w)), list(env), 
                 list(as.numeric(env_w)))
}



################################ DF ####################################
################################Func RADAR####################################

radar_values <- function(user_list, category){
  #if category == None case
  m_words <- c(unlist(df$Major[df$Cateogry == category]))
  m_weights <- c(unlist(df$Major_W[df$Cateogry == category]))
  m_vals<-which(m_words %in% user_list)
  if (length(m_words) == 0)
  {
    m_total = 1
  }
  else if (length(m_vals) == 0){
    m_total = 0
  }
  else{
    m_total <- sum(m_weights[m_vals])
  }
  
  s_words <- c(unlist(df$Specialty[df$Cateogry == category]))
  s_weights <- c(unlist(df$Specialty_W[df$Cateogry == category]))
  s_vals<-which(s_words %in% user_list)
  if (length(s_words) == 0)
  {
    s_total = 1
  }
  else if (length(s_vals) == 0){
    s_total = 0
  }
  else{
    s_total <- sum(s_weights[s_vals])
  }
  
  to_words <- c(unlist(df$Tool[df$Cateogry == category]))
  to_weights <- c(unlist(df$Tool_W[df$Cateogry == category]))
  to_vals<-which(to_words %in% user_list)
  if (length(to_words) == 0)
  {
    to_total = 1
  }
  else if (length(to_vals) == 0){
    to_total = 0
  }
  else{
    to_total <- sum(to_weights[to_vals])
  }
  
  tr_words <- c(unlist(df$Trait[df$Cateogry == category]))
  tr_weights <- c(unlist(df$Trait_W[df$Cateogry == category]))
  tr_vals<-which(tr_words %in% user_list)
  if (length(tr_words) == 0)
  {
    tr_total = 1
  }
  else if (length(tr_vals) == 0){
    tr_total = 0
  }
  else{
    tr_total <- sum(tr_weights[tr_vals])
  }
  
  e_words <- c(unlist(df$Environment[df$Cateogry == category]))
  e_weights <- c(unlist(df$Environment_W[df$Cateogry == category]))
  e_vals<-which(e_words %in% user_list)
  if (length(e_words) == 0)
  {
    e_total = 1
  }
  else if (length(e_vals) == 0){
    e_total = 0
  }
  else{
    e_total <- sum(e_weights[e_vals])
  }
  return (c(m_total, s_total, to_total, tr_total, e_total))
}
################################ Func RADAR #################################

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
                               
                               # multi sel dropdown
                               fluidRow(
                                   selectInput("skill", "Choose your skills:",multiple = TRUE,
                                               list(`Major` = major_words,
                                                    `Specialty` = spec_words,
                                                    `Tool` = tool_words,
                                                    `Trait` = trait_words,
                                                    `Environment` = env_words)
                                   ),
                               ),
                               
                               ##################################wordcloud#############################
                               # # Sidebar with a slider and selection inputs
                               # fluidRow(
                               #     selectInput("selection", "Choose a job title:",
                               #                 choices = jobs),
                               #     actionButton("update", "Change"),
                               #     hr(),
                               #     sliderInput("freq",
                               #                 "Minimum Frequency:",
                               #                 min = 1,  max = 50, value = 15),
                               #     sliderInput("max",
                               #                 "Maximum Number of Words:",
                               #                 min = 1,  max = 300,  value = 100)
                               # ),
                               ##################################wordcloud#############################
                               
                           ),
                           
                           # Show a plot 
                           mainPanel(
                               tabsetPanel(
                                   tabPanel("raw datasets",textOutput("selecteds_sk1")),
                                   tabPanel("static plots",textOutput("result")), # multi sel dropdown
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
                 ##################################Team Page###############################
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
      list_of_words <- c(list_of_words, input$skill)
      print(list_of_words)
      vals<-which(list_of_words %in% list)
      #print(vals)
      paste("You chose", list(values[which(list_of_words %in% list)]))
    })
    
    ##################################radar chart#############################
    output$plot1 <- renderPlotly({
      user_words <- c(user_words, input$skill)
      category1 <- 'program management'
      category2 <- 'technical solutions'
      
        plot_ly(
            type = 'scatterpolar',
            r = radar_values(user_words, category1), #Returns vector of values to be used
            theta = c('Major', 'Speciality', 'Tools', 'Traits', 'Environment'),
            name = category1,
            fill = 'toself',
            mode = 'markers'
        ) %>%
          add_trace(
            r = radar_values(user_words, category2), #Returns vector of values to be used
            theta = c('Major', 'Speciality', 'Tools', 'Traits', 'Environment'),
            name = category2,
            mode = 'markers',
            visible = TRUE
           
          ) %>%
            
          
        layout(
            polar = list(
                radialaxis = list(
                    visible = T,
                    range = c(0,1)
                )
            ),
            
            showlegend=TRUE
            
            
        )
    })
    
    ##################################radar chart#############################
    
    ##################################wordcloud#############################
    # # Define a reactive expression for the document term matrix
    # terms <- reactive({
    #     # Change when the "update" button is pressed...
    #     input$update
    #     # ...but not for anything else
    #     isolate({
    #         withProgress({
    #             setProgress(message = "Processing corpus...")
    #             getTermMatrix(input$selection)
    #         })
    #     })
    # })
    # 
    # # Make the wordcloud drawing predictable during a session
    # wordcloud_rep <- repeatable(wordcloud)
    # 
    # output$plot <- renderPlot({
    #     v <- terms()
    #     wordcloud_rep(names(v), v, scale=c(4,0.5),
    #                   min.freq = input$freq, max.words=input$max,
    #                   colors=brewer.pal(8, "Dark2"))
    # })
    # 
    ##################################wordcloud#############################
    
}

# Run the application 
shinyApp(ui = ui, server = server)
