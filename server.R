#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
m <- nrow(ds)
n <- ncol(ds)

colnames(ds) <- c('session', 'course', 'section', 'title', 'instructor',
                  'days', 'start', 'end', 'campus', 'location',
                  'room', 'method', 'credits', 'max', 'open', 'comment')

ds$days <- gsub('\\s+', '', ds$days) # Trim the white space

# Get courses with large number of sections
t <- as.matrix(table(ds$course, ds$section))
t[t>1] <- 1
tnew <- apply(t, 1, sum)
large_enrollment_course <- names(tnew[tnew > 5])

# ds_copy for nice display in time columns
ds_copy <- ds
time0 <- as.numeric(unlist(ds[, 'start']))
time1 <- as.numeric(unlist(ds[, 'end']))
ds_copy[, 'start'] <- format(as.POSIXct(time0*24*3600, origin="2001-01-01", "GMT"), "%I:%M %p")
ds_copy[, 'end'] <- format(as.POSIXct(time1*24*3600, origin="2001-01-01", "GMT"), "%I:%M %p")

ds[nrow(ds) + 1, ] <- as.list(rep(' ', n)) # A separation line
ds_copy[nrow(ds_copy) + 1, ] <- as.list(rep(' ', n)) # A separation line

# mark separation line as 1
ds['extra'] <- ifelse(ds$course == ' ', 1, 0)               
ds_copy['extra'] <- ifelse(ds$course == ' ', 1, 0)               


shinyServer(function(input, output, session) {
    
    ####################################################
    lapply(X = 1:6, FUN = function(i) {
            observeEvent(input[[paste0('c', i)]], {
                
                # Update the list of sections based on the course selected
                # Display the section number directly if there is only one section offered
                # Otherwise, display '--'
                if (length(unique(ds[which(ds$course == input[[paste0('c', i)]]), ]$section)) == 1) {
                    updateSelectInput(session, paste0('s', i), 'Section', 
                                      unique(ds[which(ds$course == input[[paste0('c', i)]]), ]$section)
                    )
                } else
                    updateSelectInput(session, paste0('s', i), 'Section', 
                                      c('--', unique(ds[which(ds$course == input[[paste0('c', i)]]), ]$section))
                    )
                    
                # Avoid selecting a course multiple times
                lapply(X = (1:6)[-i], FUN = function(j) {
                    if (input[[paste0('c', j)]] == input[[paste0('c', i)]] & input[[paste0('c', j)]] != '--') {
                        updateSelectInput(session, paste0('c', j), 'Course', c('--', unique(ds$course)))
                    }
                })  # End of lapply
            })      # End of observeEvent
    }) # End of lapply

    ####################################################
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
    id <- eventReactive(c(input$run, input$OpenSeat), {
        
        if (input$OpenSeat == TRUE) {
            ds <- ds[which(ds$open > 0 | ds$open == ' '),]
        }
        x <- unlist(lapply(1:6, function(i){input[[paste0('c', i)]]}))
        y <- unlist(lapply(1:6, function(i){input[[paste0('s', i)]]}))
        m <- which(x != '--')
        n <- which(y != '--')
        mn <- intersect(m, n)
        if (length(mn) == 0) 
            open <- m
        else
            open <- m[-which(m %in% mn)]

        
        large_enrollment <- intersect(x[open], large_enrollment_course)
        how_many <- length(large_enrollment)
        if (how_many > 2) {
            shinyalert(title = 'Sorry', 
                        text = c(sprintf('Due to the limited capacity of the server, the app can only search for at most TWO 
                                    large enrollment courses with open specified sections. 
                                    You have provided %d: ', how_many), large_enrollment),
                       type = 'info', 
                       html = TRUE)
            return()
        }else {
            if (length(m) == 0) 
                return()
            else {
                
                done = FALSE
                max_depth <- length(open)
                tab <- matrix(NA, nrow = 6, ncol = 1) # All possible schedules
                sched <- rep(NA, 6)                   # One possible schedule
                list <- nrow(ds) # Initialize list of schedules
                if (length(mn) > 0) {
                    temp <- which(ds$course == x[mn[1]] & ds$section == y[mn[1]])
                    list <- c(list, temp)
                    sched[mn[1]] <- y[mn[1]]
                    for (i in mn[-1]) {
                        temp <- which(ds$course == x[i] & ds$section == y[i])
                        if (addsection(list, temp, ds)) {
                            sched[i] <- y[i]
                            list <- c(list, temp)
                        } else {
                            shinyalert('Schedule conflict', 
                                       paste(x[mn[i]], 'conflicts with one of the existing course'), 
                                       type = 'error')
                            done = TRUE
                            break
                        }
                    }
                }
                
                if (done) {
                    return()
                }else {
                    if (max_depth == 0) {
                        return(list)
                    }else {
                        # Need a search when max_depth > 0
                        max_width <- rep(NA, max_depth)
                        max_width <- sapply(1:max_depth, function(i){
                            max_width[i] = length(unique(ds[which(ds$course == x[open[i]]),]$section))
                        })
                        
                        d <- 1
                        w <- 1
                        while(!(d == 1 & w > max_width[1])) {
                            
                            if (w > max_width[d] & d > 1) {
                                #print('going back one level')
                                d <- d - 1
                                w <- as.numeric(sched[open[d]]) + 1
                                sec <- unique(ds[which(ds$course == x[open[d]]),]$section)
                                erase <- which(ds$course == x[open[d]] & ds$section == sec[w - 1])
                                list <- list[! list %in% erase]
                            } else {
                                sec <- unique(ds[which(ds$course == x[open[d]]),]$section)
                                temp <- which(ds$course == x[open[d]] & ds$section == sec[w])
                                if (addsection(list, temp, ds) & d < max_depth) {
                                    #print('go deeper')
                                    sched[open[d]] <- sec[w]
                                    list <- c(list, temp)
                                    d <- d + 1
                                    w <- 1
                                }else if (addsection(list, temp, ds) & d == max_depth) {
                                    #print('s. go wider')
                                    sched[open[d]] <- sec[w]
                                    tab <- cbind(tab, sched)
                                    sched[open[d]] <- NA
                                    list <- list[! list %in% temp]
                                    w <- w + 1
                                }else if (!addsection(list, temp, ds)) {
                                    #print('f. go wider')
                                    w <- w + 1
                                }
                            }
                        }
                        if (ncol(tab) == 1) {
                            shinyalert('No compatible schedules for these courses.', type = 'info')
                            return()
                        }else {
                            tab <- as.data.frame(tab[, -1])
                            ns <- ncol(tab) # number of schedules
                            list <- nrow(ds)
                            for (i in 1:ns) {
                                # print(c('Schedule', i))
                                for (j in m) {
                                    #sections <- unique(ds[which(ds$course == x[j]),]$section)
                                    #temp <- which(ds$course == x[j] & ds$section == sections[as.numeric(tab[j, i])])
                                    temp <- which(ds$course == x[j] & ds$section == tab[j, i])
                                    list <- c(list, temp)
                                }
                                list <- c(list, nrow(ds))
                            }
                            return(list)
                        }
                    }
                }
            }
        }
        
        
        
        
    })
        
    output$schedule <- DT::renderDT({
        if (input$OpenSeat == TRUE)
            ds_copy <- ds_copy[which(ds_copy$open > 0 | ds_copy$open == ' '),]
            
        datatable(ds_copy[id(), 
            c('course', 'section', 'days', 'start', 'end', 'room', 'instructor', 'open', 'comment', 'extra')], 
            rownames = FALSE,
            colnames = c('Course', 'Section', 'Days', 'Start Time', 'End Time', 'Room', 'Instructor', 'Open Seats', 'Comment', 'extra'),
            options = list(
                pageLength = 10000,
                ordering = FALSE,
                dom = "p",
                columnDefs = list(list(className ='dt-center', targets = 0:8), 
                                  list(visible=FALSE, targets=9)
                                  )
                )
            ) %>%
            formatStyle("extra", backgroundColor=styleEqual(c(0, 1),c('white','orange')), target = "row")
    })
    
})
