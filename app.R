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
pal2 <- c("darkorange","firebrick", "steelblue", "black")
EmailData <- read.csv('ClintonEmailData.csv')
ThompTime <- read.csv('ThompsonTimeline.csv')
ForSched <- read.csv('ForeignSchedule.csv')
Freq <- read.csv("TermDocumentMatrix.csv")
TfIdf <- read.csv("TFIDF.csv")

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
allnames <- levels(as.factor(c(levels(as.factor(ClintonCom$To.name)),
                               levels(as.factor(ClintonCom$From.name)))))
# generate storage in advance
stateMail <- rep(0, length(allnames))
names(stateMail) <- allnames
# for each name identify if it is associated with a .gov email anywhere in the data
for (name in allnames) {
  toMail <- filter(ClintonCom, To.name == name)
  fromMail <- filter(ClintonCom, From.name == name)
  state <- any(grepl("[\\.]gov", c(as.character(toMail$To.Add), as.character(toMail$To.name),
                               as.character(fromMail$From.name), as.character(fromMail$From.Add))))
  mil <- any(grepl("[\\.]mil$", c(as.character(toMail$To.Add), as.character(toMail$To.name),
                             as.character(fromMail$From.name), as.character(fromMail$From.Add))))
  notState <- any(grepl("[@\\.]", c(as.character(toMail$To.Add), as.character(toMail$To.name),
                                  as.character(fromMail$From.name), as.character(fromMail$From.Add))))
  stateMail[which(allnames == name)] <- as.numeric(state) + as.numeric(notState) + 
    2*as.numeric(mil)
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
    grid.text(".gov", x = 0, y = 0.02, just = "left", 
              gp = gpar(face = 2, col = "steelblue"))
    grid.text(".mil", x = 0.33, y = 0.02, just = "center",
              gp = gpar(face = 2, col = "black"))
    grid.text("Not .gov", x = 0.66, y = 0.02, just = "center", 
              gp = gpar(face = 2, col = "firebrick"))
    grid.text("Unidentifiable", x = 1, y = 0.02, just = "right", 
              gp = gpar(face = 2, col = "darkorange"))
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

# define links for important individuals
createLink <- function(val) {
  paste("<a href='https://en.wikipedia.org/wiki/",val,"'>",
        str_replace(val, "_", " "),"</a>", sep = "")
}
linknames <- c("Hillary_Clinton", "Cheryl_Mills", "Huma_Abedin", "Sidney_Blumenthal",
               "Jake_Sullivan", "Tony_Blair")
links <- createLink(linknames)

### App #################################################################################
# define some javascript code for extracting user window size data
jscode <-'
var height = 0;
$(document).on("shiny:connected", function(e) {
height = window.innerHeight;
Shiny.onInputChange("height", height);
});
$(window).resize(function(e) {
height = window.innerHeight;
Shiny.onInputChange("height", height);
});
'
# Define UI for application that draws a histogram
ui <- fluidPage(
   tags$head(tags$script(jscode)),

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
            wellPanel(id = "WriteUp", 
                      style = "overflow-y:scroll; max-height: 800px", 
                      h3("A Brief Overview"),
                      h5("Christopher D. Salahub and R. Wayne Oldford"),
                      h3("The Email Timeline"),
                      p("While the following timeline is somewhat abbreviated, it should serve
                        to raise some of the major issues and concerns related to Clinton’s
                        private email server and its contents. It also introduces some of the
                        principal characters involved. More complete and in depth timelines
                        are readily available elsewhere (e.g. ", 
                        a("Sharyl Attkisson",  
                          href = "https://sharylattkisson.com/hillary-clintons-email-the-definitive-timeline/"),
                        " or ", 
                        a("The Washington Post",
                          href = "https://www.washingtonpost.com/news/fact-checker/wp/2015/03/10/hillary-clintons-emails-a-timeline-of-actions-and-regulations/?utm_term=.dec3139a0542"),
                        ") On November 21, 2008, the New York Times reported that Hillary
                        Clinton had decided to accept the position of U.S. Secretary of State.
                        On January 13, 2009 the internet domain name clintonemail.com
                        was ",
                        a("registered", href = "https://www.whois.net/"), 
                        " and eight days later Senator Clinton was confirmed as
                        Secretary of State."),
                      p("Public knowledge that a private email server was being used by
                        Secretary Clinton and others for State Department and personal communications
                        did not surface until ",
                        a("March 2015", href = "https://www.nytimes.com/2015/03/03/us/politics/hillary-clintons-use-of-private-email-at-state-department-raises-flags.html"),
                        " during the course of a ", 
                        a("U.S. Congressional investigation", href = "http://benghazi.house.gov/"), 
                        " of the September 11, 2012 attack by militants on U.S. compounds in 
                        Benghazi Libya. The State Department had ",
                        a("difficulty fulfilling", href = "https://www.washingtonpost.com/investigations/how-clintons-email-scandal-took-root/2016/03/27/ee301168-e162-11e5-846c-10191d1fc4ec_story.html?utm_term=.95919beefdc0"),
                        " public FOIA and House Benghazi Committee requests for Secretary Clinton’s
                        government emails because she had exclusively used the private server
                        for all her email. On March 10, 2015, Clinton told reporters that she
                        turned over 30,490 emails to the State Department and deleted 31,830
                        emails deemed to be personal. Clinton had tasked ",
                        a("three lawyers", href = "http://www.dailymail.co.uk/news/article-3773393/Clinton-s-former-Chief-Staff-served-attorney-private-server-investigation.html"),
                        " Cheryl Mills (Clinton’s former chief of staff), David Kendall (Clinton’s
                        personal lawyer), and Heather Samuelson (a State Department staffer
                        during Clinton’s tenure) to make the determinations as to which emails
                        were work related and which were not."),
                      p("On March 10, 2015 the House Benghazi Committee ",
                        a("requested", href = "https://www.nytimes.com/politics/first-draft/2015/03/20/house-benghazi-committee-requests-hillary-clinton-email-server/"),
                        " that the private email server be turned over to a neutral third party
                        to determine which emails are personal and which are government
                        records, but was informed March 27 by David Kendall that ",
                        a("no emails remained", href = "https://www.nytimes.com/2015/03/28/us/politics/no-copies-of-hillary-clinton-emails-on-server-lawyer-says.html"),
                        " on the private server for any kind of review.
                        Between March 25 and 31, 2015, Paul Combetta (then the server’s
                        system administrator), erased all backup copies using BleachBit (see
                        www.bleachbit.org)."),
                      p(a("Combetta,", href = "https://www.nytimes.com/2016/09/09/us/politics/hillary-clinton-emails-investigation.html"),
                        a("Mills,", href = "http://www.politico.com/story/2016/09/mills-immunity-228580"),
                        " and ",
                        a("Samuelson", href = "http://dailycaller.com/2016/09/23/the-immunized-five-meet-the-people-covering-for-hillary/"),
                        " were later granted partial immunity by the Justice Department during
                        the FBI investigations into the private email server, as were two others: ",
                        a("Bryan Pagliano", href = "http://dailycaller.com/2016/09/23/the-immunized-five-meet-the-people-covering-for-hillary/"),
                        " (original server manager) and ",
                        a("John Bentel", href = "http://dailycaller.com/2016/09/23/the-immunized-five-meet-the-people-covering-for-hillary/"),
                        " (former director of Information Resources
                        Management for the State Department’s Executive Secretariat)"),
                      p("On April 12, 2015 Clinton announces that she is running for the
                        U.S. Presidency. On July 24, 2015, inspectors general for the State
                        Department and the national intelligence agencies announce that they ",
                        a("found classified information", href = "https://www.nytimes.com/2015/07/25/us/politics/hillary-clinton-email-classified-information-inspector-general-intelligence-community.html"),
                        "in the emails and that the information was classified at the time sent,
                       though her campaign declared that they must have been classified after
                       the fact. On August 19, 2015, Clinton calls the allegation of mishandling
                       classified information a ",
                        a("'disagreement between agencies'", href = "https://www.theguardian.com/us-news/2015/aug/19/hillary-clinton-dismisses-misconduct-private-email")),
                      p("Nearly one year later, July 5, 2016, FBI Director James Comey
                       recommended that ",
                        a("no charges be laid", href = "https://www.nytimes.com/2016/07/06/us/politics/hillary-clinton-fbi-email-comey.html"),
                        "against Clinton for her use of a private email server. On 
                       October 28, 2016, Comey revealed that
                       in a separate investigation into former Congressman Anthony Weiner,
                       that emails belonging to his wife Huma Abedin had been found on
                       his laptop. Since Abedin was a close aid to former Secretary Clinton,
                       FBI investigations were reopened into the private server usage but
                       closed again by Comey on November 6, 2016 ",
                        a("without charges being laid.", href = "https://www.theguardian.com/us-news/2016/nov/06/fbi-director-hillary-clinton-email-investigation-criminal-james-comey"),
                        "In both cases, Comey and the FBI are criticized by pundits
                       from different political parties."),
                      h3("Wikipedia Articles of Key Players"),
                      dataTableOutput("links"))),

     # central interaction panel with a slider input for number of bins
     column(width = 2,
            fluidRow(selectInput("AdjVar", "Daily Reference Point: ", c("0:00", "18:00"),
                                 selected = "0:00")),
            fluidRow(checkboxInput("Schedule", "Display Foreign Travel Schedule")),
            fluidRow(selectInput("ToFromFilter", "Show Emails: ", 
                                 c("From Clinton", "To Clinton", "All Emails"),
                                 selected = "All Emails", multiple = FALSE)),
            fluidRow(selectInput("ClassFilter", "Emails Including Any of the FOIA Codes: ",
                                 c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "None"),
                                 selected = c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", 
                                              "B9", "None"),
                                 multiple = TRUE)),
            fluidRow(h4("20 Highest TFIDF Terms")),
            fluidRow(textOutput("tfidf")),
            fluidRow(h4("20 Highest Frequency Terms")),
            fluidRow(textOutput("Freq"))
     ),

     # add a well panel with all the generated displays
     column(width = 6,
            wellPanel(id = "tPanel",
                      style = paste('overflow-y:scroll; max-height: 800px'),
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
  # insert a caching function
  inputProc <- function(input) reactive({
    # generate a storage item
    intermed <- list()
    # select data using the input date and classification filters
    intermed$DateRange <- paste(chron(c(input$range[1], input$range[2]),
                                      format = "day mon year"),
                                collapse = " - ")
    intermed$selIDs <- 
      switch(input$ToFromFilter,
             "All Emails" = filter(AsSec[apply(AsSec[,input$ClassFilter, drop = FALSE],1,any),],
                                   Date < input$range[2] + 1 & Date >= input$range[1])$ID,
             "From Clinton" = filter(AsSec[apply(AsSec[,input$ClassFilter, drop = FALSE],1,any),],
                                     Date < input$range[2] + 1 & Date >= input$range[1] &
                                       (From.name == "Hillary Clinton" | From.name == "H"))$ID,
             "To Clinton" = filter(AsSec[apply(AsSec[,input$ClassFilter, drop = FALSE],1,any),],
                                   Date < input$range[2] + 1 & Date >= input$range[1] &
                                     (To.name == "Hillary Clinton" | To.name == "H"))$ID)
    intermed$toFromLab <- switch(input$ToFromFilter,
                                 "All Emails" = "Sent/Received",
                                 "From Clinton" = "Sent",
                                 "To Clinton" = "Received")
    intermed$selDays <- ASDays[ASDays < input$range[2] + 1 & ASDays >= input$range[1]]
    intermed$selCounts <- AScounts[ASDays < input$range[2] + 1 & ASDays >= input$range[1]]
    intermed$Sched <- filter(ForSched, (StartDate >= input$range[1] &
                                          StartDate < input$range[2]) |
                               (EndDate < input$range[2] & EndDate >= input$range[1]))
    intermed$dispSched <- input$Schedule
    intermed$adjVar <- input$AdjVar == "18:00"
    return(intermed)
  })
  # memoise the above function for caching
  inter <- inputProc(input)
  # now define all outputs
   output$Times <- renderPlot({
     # first determine whether adjusted or unadjusted times are to be used
     if (inter()$adjVar) {
       timevalues <- AsSec$Hour6pm[AsSec$ID %in% inter()$selIDs]*60 + 
         AsSec$Minutes[AsSec$ID %in% inter()$selIDs,]
       xlab <- "Adjusted Date (dd/mm/yy)"
       main <- paste("Adjusted Email", inter()$toFromLab, "Times")
       labelset <- c('22:00', '02:00', '06:00', '10:00', '14:00')
     } else {
       timevalues <- AsSec$Hour[AsSec$ID %in% inter()$selIDs]*60 + 
         AsSec$Minutes[AsSec$ID %in% inter()$selIDs]
       xlab <- "Date (dd/mm/yy)"
       main <- paste("Email", inter()$toFromLab, "Times")
       labelset <- c("04:00", "08:00", "12:00", "16:00", "20:00")
     }
     # plot dates based on the dates provided from the slider
     plot(x = as.chron(floor(AsSec$Date[AsSec$ID %in% inter()$selIDs])),
          y = timevalues, xlab = xlab,
          ylab = paste("Time", inter()$toFromLab), yaxt = 'n', xaxt = 'n', pch = 19,
          main = paste(main, " (", inter()$DateRange, ")", sep = ""),
          col = adjustcolor(pal[as.numeric(AsSec$Redacted[AsSec$ID %in% inter()$selIDs])+1], 
                            alpha.f = 0.5),
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
     plot(x = inter()$selDays, y = inter()$selCounts, type = 'l', xlab = 'Date (dd/mm/yy)',
          xaxt = "n", ylab = 'Number of Emails', pch = 19,
          main = paste("Number of Emails by Date (", inter()$DateRange, ")", sep = ""),
          ylim = extendrange(AScounts), col = adjustcolor("black", alpha.f = 0.6))
     # add the schedule if it has been selected
     if (inter()$dispSched) {
       apply(inter()$Sched, 1,
          function(row) {
            polygon(x = c(row[c("StartDate", "EndDate")],  rev(row[c("StartDate", "EndDate")])),
                    y = c(-50,-50,130,130), col = adjustcolor("steelblue", alpha.f = 0.4),
                    border = NA)
            })
     }
     axis.Date(side = 1, as.chron(inter()$selDays), format = "%d/%m/%y")
     countbyDate <- tally(group_by(AsSec[AsSec$ID %in% inter()$selIDs,], dates(as.chron(Date))))
     tempDays <- seq(from = min(countbyDate$`dates(as.chron(Date))`),
                   to = max(countbyDate$`dates(as.chron(Date))`),
                   by = 'days')
     tempcounts <- rep(0, length(tempDays))
     tempcounts[tempDays %in% countbyDate$`dates(as.chron(Date))`] <- countbyDate$n
     lines(x = tempDays, y = tempcounts, col = adjustcolor("darkorchid", alpha.f = 0.4))
     legend("topright", lty = 1, col = adjustcolor(c("black", "darkorchid")),
            legend = c("All", "Selected Emails"), cex = 0.8, inset = c(0,-0.05),
            xpd = TRUE, horiz = TRUE)
   })
   # add the spiral network plot
   output$Spiral <- renderPlot({
     spiralNetPlot2(wgtTbl = sort(table(c(as.character(ClintonCom$To.name[ClintonCom$ID %in% inter()$selIDs]),
                                as.character(ClintonCom$From.name[ClintonCom$ID %in% inter()$selIDs]))),
                        decreasing = TRUE)[-1],
                   title = paste("Inner Circle by Volume of Communication (", 
                                 inter()$DateRange, ")", sep = ""))
   })
   # display the top twenty tfidf terms
   output$tfidf <- renderText(paste(
     colnames(TfIdf[TfIdf$ID %in% inter()$selIDs,-(1:12)])[order(cSum(TfIdf[TfIdf$ID %in% inter()$selIDs,-(1:12)]), decreasing = TRUE)][1:20],
     collapse = ", "
     ))
   # display the top twenty frequency terms
   output$Freq <- renderText(paste(
     colnames(Freq[Freq$ID %in% inter()$selIDs,-(1:12)])[order(cSum(Freq[Freq$ID %in% inter()$selIDs,-(1:12)]), decreasing = TRUE)][1:20],
     collapse = ", "
   ))
   # display the selected date range
   output$Dates <- renderText(inter()$DateRange)
   # finally a barplot of classification codes used in the selection
   output$Class <- renderPlot(
     barplot(table(unlist(str_split(AsSec$Classification[AsSec$ID %in% inter()$selIDs], "-"))),
                          main = paste("FOIA Exemption Codes Used for Redactions (",
                                       inter()$DateRange, ")", sep = ""))
   )
   # record the user window size (still cannot use in ui)
   # output$winHeight <- reactive(input$height)
   # add server link processing
   output$links <- renderDataTable({inter <- as.matrix(links, ncol = 1)
   colnames(inter) <- "Individual"
   return(inter)}, escape = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)

