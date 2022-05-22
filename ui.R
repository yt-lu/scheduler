#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyalert)
library("readxl")
ds <- read_excel("/Users/lu_y/Downloads/CourseList.xlsx", sheet = 'in')
ds <- ds[-(1:2),]
colnames(ds) <- c('session', 'course', 'section', 'title', 'instructor',
                  'days', 'start', 'end', 'campus', 'location', 
                  'room', 'method', 'credits', 'max', 'open', 'comment')

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    
    # Application title
    titlePanel("Scheduler"),
    
    fluidRow(
        column(3, 
            # Courses
            lapply(1:6, function(i){
                useShinyalert()
                selectInput(paste0('c', i), paste0('Course ', i), c('--', unique(ds$course)))
            })
        ),
        column(2, 
           # Sections
           lapply(1:6, function(i) {
               useShinyalert()
               selectInput(paste0('s', i), 'Section', c('--'))
           })
        ),
        column(2,
            useShinyalert(),
            actionButton("run", "Run the Schedule")
        )
   ),
        # Show a plot of the generated distribution
        mainPanel(
            # fluidRow(column(12, htmlOutput("schedule"))
            # )
            htmlOutput('schedule')
        )
    )
)
