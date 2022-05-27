#
# Author: Yuanting Lu (Department of Math, CLAS, Mercer University)
#
# Last updated: 8:30 pm, May 26, 2022
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyalert)
library(readxl)
library(DT)

ds <- read_excel("CourseList.xlsx", sheet = 'in')
ds <- ds[-(1:2),]
colnames(ds) <- c('session', 'course', 'section', 'title', 'instructor',
                  'days', 'start', 'end', 'campus', 'location', 
                  'room', 'method', 'credits', 'max', 'open', 'comment')

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    
    # Application title
    titlePanel('Scheduler'),
    
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
        column(6,
            checkboxInput('OpenSeat', 
                          label = 'Only display sections with open seats in the results.',
                          value = FALSE,
                          width = '100%'),
            useShinyalert(),
            actionButton("run", "Run", width = '30%'),
            hr(),
            print(HTML("<p style = 'font-size:120%;'>How to use the app:</p>
            
            <ul style = 'font-size:120%;'><li style = 'margin-bottom:0.3em;'>Select desired courses using the drop-down menu, or 
                        delete the double dash and type in key words (e.g., UNV 101) for quicker selection.</li>
                       <li style = 'margin-bottom:0.3em;'>Select sections (if you know those for sure), or leave the section fields open
                       (to conduct a search).</li>
                       <li style = 'margin-bottom:0.3em;'>To remove a course/section, 
                       select the double dash on the top of the drop-down menu.</li>
                       <li style = 'margin-bottom:0.3em;'>Click the <em>Run</em> button to see 
                       different available schedules separated by orange bands.</li>
                       </ul>")),
            hr(),
            print(HTML("<p style='text-align: center;'>Go Bears &copy; 2022 Yuanting Lu (lu_y@mercer.edu)</p>")),
        )
   ),
        # Show a plot of the generated distribution
        mainPanel(
            # fluidRow(column(12, htmlOutput("schedule"))
            # )
            # htmlOutput('schedule')
            DT::dataTableOutput("schedule")
        )
    )
)
