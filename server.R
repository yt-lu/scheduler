## Author: Yuanting Lu (Department of Math, CLAS, Mercer University)
## Last updated: 2:00 am, May 29, 2022
##
## Find out more about building applications with Shiny here:
##
##    http://shiny.rstudio.com/


library(shiny)
library(shinyalert)
library(readxl)
library(DT)


mercer <- read_excel("CourseList.xlsx", sheet = 'in')
mercer <- mercer[-(1:2),] 

colnames(mercer) <- c('session', 'course', 'section', 'title', 'instructor',
                  'days', 'start', 'end', 'campus', 'location',
                  'room', 'method', 'credits', 'max', 'open', 'comment')

mercer$days <- gsub('\\s+', '', mercer$days) # Trim 'M W F' to 'MWF'.

# The time columns in mercer are in decimals. (For calculations.)
# The time columns in mercer_copy are in time formats. (For printing.)
mercer_copy <- mercer
time0 <- as.numeric(unlist(mercer[, 'start']))
time1 <- as.numeric(unlist(mercer[, 'end']))
mercer_copy[, 'start'] <- format(as.POSIXct(time0*24*3600, origin="2001-01-01", "GMT"), "%I:%M %p")
mercer_copy[, 'end'] <- format(as.POSIXct(time1*24*3600, origin="2001-01-01", "GMT"), "%I:%M %p")

# Add a blank row at the end. It is used as a separation row between multiple schedules.
# Add an extra column to mercer_copy. It is used to color the blank row in the output. 
mercer[nrow(mercer) + 1, ] <- as.list(rep(' ', ncol(mercer))) 
mercer_copy[nrow(mercer_copy) + 1, ] <- as.list(rep(' ', ncol(mercer_copy))) 
mercer_copy['extra'] <- ifelse(mercer_copy$course == ' ', 1, 0) 


####################################################################
## Server starts here
####################################################################
shinyServer(function(input, output, session) {
    
    # Filter data if necessary
    ds <- eventReactive(input$run, {
        
        ds <- mercer[-nrow(mercer), ] # Remove the blank row
        if (input$OpenSeat) {
            ds <- mercer[which(mercer$open > 0),]
        }
        return(ds)
    })
    
    # Large enrollment courses is defined as courses with more than 5 sections.
    large_enrollment_courses <- reactive({
        df <- ds()
        t <- as.matrix(table(df$course, df$section))
        t[t>1] <- 1
        tnew <- apply(t, 1, sum)
        large_enrollment_course <- names(tnew[tnew > 5])
        return(large_enrollment_course)
    })
    
    # Update sections drop-down menu based on the courses selected
    # Display the section number directly if there is only one section offered
    # Otherwise, display '--'
    lapply(X = 1:6, FUN = function(i) {
            observeEvent(input[[paste0('c', i)]], {
                if (length(unique(mercer[which(mercer$course == input[[paste0('c', i)]]), ]$section)) == 1) {
                    updateSelectInput(session, paste0('s', i), 'Section', 
                                      unique(mercer[which(mercer$course == input[[paste0('c', i)]]), ]$section)
                    )
                } else
                    updateSelectInput(session, paste0('s', i), 'Section', 
                                      c('--', unique(mercer[which(mercer$course == input[[paste0('c', i)]]), ]$section))
                    )
                    
                # Avoid selecting a course multiple times
                lapply(X = (1:6)[-i], FUN = function(j) {
                    if (input[[paste0('c', j)]] == input[[paste0('c', i)]] & input[[paste0('c', j)]] != '--') {
                        updateSelectInput(session, paste0('c', j), 'Course', c('--', unique(mercer$course)))
                    }
                })  # End of lapply
            })      # End of observeEvent
    }) # End of lapply

    # Check if two courses are on the same day
    sameday <- function(a, b){
        if (a == 'TBD' | b == 'TBD'){
            return(FALSE)
        } else {
            aa <- strsplit(a, '')       # Split characters in a
            bb <- strsplit(b, '')       # Split characters in b
            ab <- c(aa[[1]], bb[[1]])   # Combine all characters
            if (length(ab) == length(unique(ab)))
                return(FALSE)
            else
                return(TRUE)   
        }
    }
    
    
    # Check if time conflicts. 1 for starting time and 2 for ending time.
    sametime <- function(a, b) {
        if (is.na(a[1]) | is.na(a[2]) | is.na(b[1]) | is.na(b[2])) {
            return(FALSE)
        } else {
            if (a[2] < b[1] | a[1] > b[2]) 
                return(FALSE)
            else
                return(TRUE)
        }
    }
    
    
    # Check if two courses conflict
    overlap <- function(i, j, df) {
        if (i == nrow(df) | j == nrow(df))
            return(FALSE)
        else {
            a <- c(df$start[i], df$end[i])
            b <- c(df$start[j], df$end[j])
            if (sameday(df$days[i], df$days[j]) & sametime(a, b))
                return(TRUE)
            else
                return(FALSE)
        }
    }
    
    
    # Check if a course can be added.
    addsection <- function(list, add, df) {
        out <- TRUE
        for (t in list) {
            for (a in add) {
                if (overlap(a, t, df) == TRUE) {
                    out <- FALSE
                    break
                } 
            }
        }
        return(out)
    }
    
    
    
    ####################################################
    id <- eventReactive(c(input$run), {
        
        df <- ds()
        
        # Get the input information
        x <- unlist(lapply(1:6, function(i){input[[paste0('c', i)]]}))
        y <- unlist(lapply(1:6, function(i){input[[paste0('s', i)]]}))
        m <- which(x != '--')
        n <- which(y != '--')
        mn <- intersect(m, n) # Courses with sections already been selected.
        
        if (length(m) == 0) 
            return()

        app_exit <- FALSE
        
        if (length(mn) == 0)
            open <- m        
        else
            open <- m[-which(m %in% mn)] # Courses need a search.
        
        large_enrollment <- intersect(x[open], large_enrollment_courses())
        how_many_large <- length(large_enrollment)

        if (how_many_large > 2) {
            shinyalert(title = 'Sorry',
                        text = c(sprintf('Due to the limited capacity of the server, the app can only search for at most TWO
                                    large enrollment courses with open specified sections.
                                    You have provided %d: ', how_many_large), large_enrollment),
                       type = 'info',
                       html = TRUE)
            return()
        }else {
                max_depth <- length(open)             # Number of courses that need a search.
                sched <- rep(NA, 6)                   # One possible schedule
                list <- nrow(mercer)                  # Initialize the list of schedules by the blank row
                
                # Check if the courses with sections already been selected have conflicts.
                if (length(mn) > 0) {
                    temp <- which(mercer$course == x[mn[1]] & mercer$section == y[mn[1]])
                    list <- c(list, temp)
                    sched[mn[1]] <- y[mn[1]]
                    
                    for (i in mn[-1]) {
                        temp <- which(mercer$course == x[i] & mercer$section == y[i])

                        if (addsection(list, temp, mercer)) {
                            sched[i] <- y[i]
                            list <- c(list, temp)
                        } else {
                            shinyalert('Schedule conflict',
                                       paste(x[i], 'conflicts with one of the existing course'),
                                       type = 'error')
                            app_exit = TRUE
                            break
                        }
                    }
                }

                if (app_exit) {
                    return()
                }else {
                    if (max_depth == 0) {
                        return(list)
                    }else {

                        # Conduct a search when max_depth > 0
                        # max_width records of the number of available sections for each open course
                        max_width <- rep(NA, max_depth)
                        max_width <- sapply(1:max_depth, function(i){
                            max_width[i] = length(unique(df[which(df$course == x[open[i]]),]$section))
                        })
                        
                        d <- 1
                        w <- 1
                        
                        long_list <- vector()
                        
                        # Starting from open course (depth) 1 and section (width) 1
                        # Move to the first section of next open course.
                        #       -- If a match is found, move to the next course if possible.
                        #       -- If a match is found and it is the bottom depth, move to the next section if possible.
                        #       -- If it is not a match, move to the next section if possible.
                        #       -- Otherwise, back one level (d=d-1) and move to the next section.
                        while(!(d == 1 & w > max_width[1])) {

                            if (w > max_width[d] & d > 1) {
                                # print('going back one level')
                                d <- d - 1
                                sec <- unique(df[which(df$course == x[open[d]]),]$section)
                                w <- which(sec == sched[open[d]]) + 1
                                erase <- which(mercer$course == x[open[d]] & mercer$section == sec[w - 1])
                                list <- list[! list %in% erase]
                                
                            } else {
                                sec <- unique(df[which(df$course == x[open[d]]),]$section)
                                temp <- which(mercer$course == x[open[d]] & mercer$section == sec[w])
                                if (addsection(list, temp, mercer) & d < max_depth) {
                                    # print('go deeper')
                                    sched[open[d]] <- sec[w]
                                    list <- c(list, temp)
                                    d <- d + 1
                                    w <- 1
                                }else if (addsection(list, temp, mercer) & d == max_depth) {
                                    # print('succeded. go wider')
                                    long_list <- c(long_list, list, temp)
                                    w <- w + 1
                                }else if (!addsection(list, temp, mercer)) {
                                    # print('failed. go wider')
                                    w <- w + 1
                                }
                            }
                        }
                        if (length(long_list) == 0) {
                            shinyalert('No compatible schedules for these courses.', type = 'info')
                            return()
                        }else
                            return(c(long_list, nrow(mercer)))
                    }
                }
        }
     })
    
    output$schedule <- DT::renderDT({
        
        datatable(mercer_copy[id(),
                          c('course', 'section', 'title', 'days', 'start', 'end', 'room', 'instructor', 'open', 'comment', 'extra')],
                  rownames = FALSE,
                  colnames = c('Course', 'Section', 'Title','Days', 'Start Time', 'End Time', 'Room', 'Instructor', 'Open Seats', 'Comment', 'extra'),
                  options = list(
                      pageLength = 10000,
                      ordering = FALSE,
                      dom = "p",
                      columnDefs = list(list(className ='dt-center', targets = 0:9),
                                        list(visible=FALSE, targets=10)
                      )
                  )
        ) %>%
            formatStyle("extra", backgroundColor=styleEqual(c(0, 1),c('white','orange')), target = "row")
    })
})
