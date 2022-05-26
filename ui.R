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
        column(6,
            checkboxInput('OpenSeat', 
                          label = 'Only display sections with open seats.',
                          value = FALSE),
            useShinyalert(),
            actionButton("run", "Run the Schedule"),
            hr(),
            print(HTML("<p style = 'font-size:120%;'>How to use the app to schedule up to six courses.</p>
            
            <ul style = 'font-size:120%;'><li style = 'margin-bottom:0.3em;'>Select desired courses using the drop-down menu or 
                        delete the double dash and type in key words (e.g., UNV 101).</li>
                       <li style = 'margin-bottom:0.3em;'>If you want to schedule a paticular section for a course, 
                       specify that particular section.</li>
                       <li style = 'margin-bottom:0.3em;'>If there is no preference
                       for any particular section, leave the section field open.</li>
                       <li style = 'margin-bottom:0.3em;'>To delete a course or a section, 
                       select the double dash on the top of the drop-down menu.</li>
                       <li style = 'margin-bottom:0.3em;'>Due to the capacity of the server, please do <span style='color:red;'>NOT</span> leave the section fields open
                        for more than TWO large enrollment courses (e.g., BIO 212, CHM 111, INT 101, MAT 191, REL 160, SPN 111).</li>
                       <li style = 'margin-bottom:0.3em;'>Click the Run the Schedule button to see 
                       different available schedules separated by a gray line.</li>
                       </ul>"))
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
