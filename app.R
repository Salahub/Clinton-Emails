# load the required libraries for analysis
library(tm)
library(stringr)
library(SnowballC)
library(chron)
library(dplyr)
library(shiny)
library(grid)
library(shinyBS)

### Pre-loaded data #####################################################################
pal <- c("steelblue", "firebrick")
pal2 <- c("gray50","firebrick", "steelblue")
EmailData <- read.csv('ClintonEmailData.csv')
ThompTime <- read.csv('ThompsonTimeline.csv')
ForSched <- read.csv('ForeignSchedule.csv')
Freq <- read.csv("TermDocumentMatrix.csv")
TfIdf <- read.csv("TFIDF.csv")
# commented out daily reads, the most recent version has much greater selection power
#DailyFreq <- as.matrix(read.csv('DailyFrequency.csv'))
#DailyTfIdf <- as.matrix(read.csv('TFIDFbyDay.csv'))

### Filter and prepare the data from time as secretary of state #########################
# select all emails sent while Secretary
AsSec <- filter(EmailData, Year >= 2009 & Year <= 2013)
# extract daily counts of emails for plotting later
countbyDate <- tally(group_by(AsSec, dates(as.chron(Date))))
ASDays <- seq(from = min(countbyDate$`dates(as.chron(Date))`),
              to = max(countbyDate$`dates(as.chron(Date))`),
              by = 'days')
AScounts <- rep(0, length(ASDays))
AScounts[ASDays %in% countbyDate$`dates(as.chron(Date))`] <- countbyDate$n
# prepare the adjusted dates
AsSec <- arrange(AsSec, Date)
# now define the days with 6pm as the adjusted day definition
days6pm <- seq(from = floor(min(AsSec$Date)) + 0.75, to = max(AsSec$Date),
               by = 1)
newDays <- sapply(AsSec$Date, function(date) sum(days6pm < date) %% 7) + 1
newDays <- c("Sat-Sun","Sun-Mon","Mon-Tue","Tue-Wed","Wed-Thu","Thu-Fri",
             "Fri-Sat")[newDays]
AsSec$Weekday6pm <- newDays
# and generate new times referencing 6 pm
AsSec$Hour6pm <- (AsSec$Hour + 6) %% 24

### Spiral Network Plot Functions #######################################################
# filter the data by communications including Clinton
ClintonCom <- filter(AsSec,
                     From.name == "Hillary Clinton" | To.name == "Hillary Clinton" |
                       From.name == "H" | To.name == "H")
# extract names of all the communicated with Clinton
ClintNet <- sort(table(c(as.character(ClintonCom$To.name),
                         as.character(ClintonCom$From.name))),
                 decreasing = TRUE)[-1]
# extract state mail information, information pertaining to whether an email is .gov or not
# first get all names from above network
allnames <- levels(as.factor(c(levels(ClintonCom$To.name),
                               levels(ClintonCom$From.name))))
# generate storage in advance
stateMail <- rep(0, length(allnames))
names(stateMail) <- allnames
# for each name identify if it is associated with a .gov email anywhere in the data
for (name in allnames) {
  Mail <- filter(ClintonCom, To.name == name | From.name == name)
  state <- any(grepl(".gov", c(as.character(Mail$To.Add), as.character(Mail$To.name),
                               as.character(Mail$From.name), as.character(Mail$From.Add))))
  notState <- any(grepl("[@.]", c(as.character(Mail$To.Add), as.character(Mail$To.name),
                                   as.character(Mail$From.name), as.character(Mail$From.Add))))
  stateMail[which(allnames == name)] <- as.numeric(state) + as.numeric(notState)
}

# modify the spiral network plot function, improve it
spiralNetPlot2 <- function(centralNode = "Hillary Clinton", wgtTbl = NA,
                           levelNum = 2, nodeNum = 20, title = "Title") {
  # check the wgtTbl status
  if (!all(is.na(wgtTbl))) {
    # trim wgtTbl
    if (length(wgtTbl) > nodeNum) wgtTbl <- wgtTbl[1:nodeNum]
    # start by generating state mail colouring
    statemail <- stateMail[names(stateMail) %in% names(wgtTbl)]
    statemail <- statemail[match(names(wgtTbl), names(statemail))]
    if (length(statemail) == 0) cols <- "white"
    else cols <- pal2[statemail + 1]
    # generate a new page
    grid.newpage()
    # define radial units
    radunit <- 0.42/levelNum
    # determine inner level names
    diffs <- diff(wgtTbl)
    # select the largest
    impDiffs <- order(diffs)[1:(levelNum-1)]
    # extract names of the largest
    innerCirc <- names(wgtTbl)[1:impDiffs]
    # now sort the table alphabetically
    ordering <- order(names(wgtTbl))
    wgtTbl <- wgtTbl[ordering]
    # get the radial positions of the first level
    radPos <- seq(from = 0, to = 2*pi, length.out = length(wgtTbl) + 1)[1:length(wgtTbl)]
    # now extract coordinates
    xvals <- radunit*((!(names(wgtTbl) %in% innerCirc)) + 1)*cos(radPos) + 0.5
    yvals <- radunit*((!(names(wgtTbl) %in% innerCirc)) + 1)*sin(radPos) + 0.5
    # sort out some bookkeeping
    cols <- cols[ordering]
    # now plot everything
    for (ii in 1:length(wgtTbl)) {
      grid.lines(x = c(0.5, xvals[ii]), y = c(0.5, yvals[ii]),
                 gp = gpar(lwd = 1+wgtTbl[ii]/max(wgtTbl)*6,
                           col = adjustcolor(cols[ii]), alpha.f = 0.8))
    }
    grid.circle(x = xvals, y = yvals, r = 0.01,
                gp = gpar(col = adjustcolor(cols, alpha.f = 0.8),
                          fill = adjustcolor(cols, alpha.f = 0.8)))
    # place the central node
    grid.circle(x = 0.5, y = 0.5, r = 0.01,
                gp = gpar(col = adjustcolor(col = "firebrick", alpha.f = 1),
                          fill = adjustcolor(col = "firebrick", alpha.f = 1)))
    # label everything
    grid.text(names(wgtTbl), x = xvals, y = yvals - 0.01,
              just = "top", gp = gpar(cex = 0.75))
    # add a title
    grid.text(title, x = 0, y = 0.98, just = "left",
              gp = gpar(face = 2))
    # add a legend
    grid.text("Blue - Identifiably .gov", x = 0, y = 0.02,
              just = "left", gp = gpar(face = 2))
    grid.text("Red - Identifiably Not .gov", x = 0.98, y = 0.02,
              just = "right", gp = gpar(face = 2))
    grid.text("Grey - Unidentifiable", x = 0.5, y = 0.02,
              just = "centre", gp = gpar(face = 2))
  }
  else  {
    # generate a new page
    grid.newpage()
    # place the central node
    grid.circle(x = 0.5, y = 0.5, r = 0.01,
                gp = gpar(col = adjustcolor(col = "firebrick", alpha.f = 1),
                          fill = adjustcolor(col = "firebrick", alpha.f = 1)))
  }
}

# a column sum helper
cSum <- function(mat) {
  if (is.null(dim(mat))) mat
  else colSums(mat)
}

# a vectorized grepl function
Vgrepl <- function(patterns, strings) {
  sapply(strings, function(str) any(sapply(patterns, function(pat) grepl(pat, str))))
}

### App #################################################################################
# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Secretary Clinton's Email (Source: https://wikileaks.org/clinton-emails/)"),

   # the slider right below the title to make it as long as possible
   fluidRow(column(width = 2, offset = 0.5, h4("Date Range:"))),
   fluidRow(column(width = 6, offset = 0.5, textOutput("Dates"))),
   fluidRow(column(width = 12, sliderInput("range", "",
                                 min = floor(min(AsSec$Date)),
                                 max = floor(max(AsSec$Date)),
                                 value = c(floor(min(AsSec$Date)), floor(max(AsSec$Date))),
                                 width = "100%"))),

   # everything else
   fluidRow(
     # well panel with write up
     column(width = 4,
            wellPanel(id = "WriteUp", style = "overflow-y:scroll; max-height: 600px",
                      h3("Write up Goes Here"))),

     # central interaction panel with a slider input for number of bins
     column(width = 2,
            fluidRow(selectInput("AdjVar", "Daily Reference Point: ", c("0:00", "18:00"),
                                 selected = "0:00")),
            fluidRow(checkboxInput("Schedule", "Display Foreign Travel Schedule")),
            fluidRow(selectInput("ClassFilter", "FOIA Codes: ",
                                 c("None", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9"),
                                 selected = c("None", "B1", "B2", "B3", "B4", "B5", "B6", "B7",
                                              "B8", "B9"),
                                 multiple = TRUE)),
            fluidRow(h4("20 Highest TFIDF Terms")),
            fluidRow(textOutput("tfidf")),
            fluidRow(h4("20 Highest Frequency Terms")),
            fluidRow(textOutput("Freq"))
     ),

     # add a well panel with all the generated displays
     column(width = 6,
            wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                      fluidRow(plotOutput("Spiral")),
                      fluidRow(plotOutput("DaySum")),
                      fluidRow(plotOutput("Times")),
                      fluidRow(plotOutput("Class"))
            )
     )
   )
)

# Define server logic required to draw all displays
server <- function(input, output) {
  # select data using the input date and classification filters
  DateRange <- reactive(paste(chron(c(input$range[1], input$range[2]),
                                    format = "day mon year"),
                              collapse = " - "))
  selDat <- reactive(filter(AsSec[apply(AsSec[,input$ClassFilter],1,any),],
                            Date < input$range[2] + 1 & Date >= input$range[1]))
  selDays <- reactive(ASDays[ASDays < input$range[2] + 1 & ASDays >= input$range[1]])
  selCounts <- reactive(AScounts[ASDays < input$range[2] + 1 & ASDays >= input$range[1]])
  SpirDat <- reactive(filter(ClintonCom[apply(ClintonCom[,input$ClassFilter],1,any),],
                              Date < input$range[2] + 1 & Date >= input$range[1]))
  Sched <- reactive(filter(ForSched, (StartDate >= input$range[1] &
                                        StartDate < input$range[2]) |
                             (EndDate < input$range[2] & EndDate >= input$range[1])))
  dispSched <- reactive(input$Schedule)
  tfIdf <- reactive(filter(TfIdf[apply(TfIdf[,input$ClassFilter],1,any),],
                           Date < input$range[2] + 1 & Date >= input$range[1])[,-(1:12)])
  freq <- reactive(filter(Freq[apply(Freq[,input$ClassFilter],1,any),],
                          Date < input$range[2] + 1 & Date >= input$range[1])[,-(1:12)])
  adjVar <- reactive(input$AdjVar == "18:00")
   output$Times <- renderPlot({
     # first determine whether adjusted or unadjusted times are to be used
     if (adjVar()) {
       timevalues <- selDat()$Hour6pm*60 + selDat()$Minutes
       xlab <- "Adjusted Date (dd/mm/yy)"
       main <- "Adjusted Email Times"
       labelset <- c('22:00', '02:00', '06:00', '10:00', '14:00')
     }
     else {
       timevalues <- selDat()$Hour*60 + selDat()$Minutes
       xlab <- "Date (dd/mm/yy)"
       main <- "Email Sent/Received Times"
       labelset <- c("04:00", "08:00", "12:00", "16:00", "20:00")
     }
     # plot dates based on the dates provided from the slider
     plot(x = as.chron(floor(selDat()$Date)),
          y = timevalues, xlab = xlab,
          ylab = 'Time Sent', yaxt = 'n', xaxt = 'n', pch = 19,
          main = main,
          col = adjustcolor(pal[as.numeric(selDat()$Redacted)+1], alpha.f = 0.5),
          cex = 0.25, ylim = c(1440,0) + c(0.05,-0.05)*1440, 
          xlim = c(input$range[1], input$range[2]))
     axis(side = 2, at = c(240, 480, 720, 960, 1200),
          labels = labelset)
     axis.Date(side = 1, as.chron(c(input$range[1], input$range[2])), format = "%d/%m/%y")
     legend("topright", legend = c("Redacted", "Unedited"), pch = c(19,19),
           col = c("firebrick", "steelblue"), horiz = TRUE, cex = 0.8, inset = c(0,-0.05),
           xpd = TRUE)
   })
   # next display the time series of emails sent by day
   output$DaySum <- renderPlot({
     plot(x = selDays(), y = selCounts(), type = 'l', xlab = 'Date (dd/mm/yy)',
          xaxt = "n", ylab = 'Number of Emails', pch = 19,
          main = "Number of Emails by Date",
          ylim = extendrange(AScounts), col = adjustcolor("black", alpha.f = 0.6))
     # add the schedule if it has been selected
     if (dispSched()) {
       apply(Sched(), 1,
          function(row) {
            polygon(x = c(row[c("StartDate", "EndDate")],  rev(row[c("StartDate", "EndDate")])),
                    y = c(-50,-50,130,130), col = adjustcolor("steelblue", alpha.f = 0.4),
                    border = NA)
            })
     }
     axis.Date(side = 1, as.chron(selDays()), format = "%d/%m/%y")
     countbyDate <- tally(group_by(selDat(), dates(as.chron(Date))))
     tempDays <- seq(from = min(countbyDate$`dates(as.chron(Date))`),
                   to = max(countbyDate$`dates(as.chron(Date))`),
                   by = 'days')
     tempcounts <- rep(0, length(tempDays))
     tempcounts[tempDays %in% countbyDate$`dates(as.chron(Date))`] <- countbyDate$n
     lines(x = tempDays, y = tempcounts, col = adjustcolor("darkorchid", alpha.f = 0.4))
     legend("topright", lty = 1, col = adjustcolor(c("black", "darkorchid")),
            legend = c("All", "Selected FOIA Codes"), cex = 0.8, inset = c(0,-0.05),
            xpd = TRUE, horiz = TRUE)
   })
   # add the spiral network plot
   output$Spiral <- renderPlot({
     spiralNetPlot2(wgtTbl = sort(table(c(as.character(SpirDat()$To.name),
                                as.character(SpirDat()$From.name))),
                        decreasing = TRUE)[-1],
                   title = "Inner Circle by Volume of Communication")
   })
   # display the top twenty tfidf terms
   output$tfidf <- renderText(paste(
     colnames(tfIdf())[order(cSum(tfIdf()), decreasing = TRUE)][1:20],
     collapse = ", "
     ))
   # display the top twenty frequency terms
   output$Freq <- renderText(paste(
     colnames(freq())[order(cSum(freq()), decreasing = TRUE)][1:20],
     collapse = ", "
   ))
   # display the selected date range
   output$Dates <- renderText(DateRange())
   # finally a barplot of classification codes used in the selection
   output$Class <- renderPlot(
     barplot(table(unlist(str_split(selDat()$Classification, "-"))),
                          main = "FOIA Exemption Codes Used for Redactions")
   )
}

# Run the application
shinyApp(ui = ui, server = server)

