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
spiralNetPlot2 <- function(centralNode = "Hillary Clinton", wgtTbl = integer(0),
                           levelNum = 2, nodeNum = 20, title = "Title") {
  # check the wgtTbl status
  if (!(length(wgtTbl) == 0)) {
    # trim wgtTbl
    if (length(wgtTbl) > nodeNum) wgtTbl <- wgtTbl[1:nodeNum]
    # start by generating state mail colouring
    statemail <- stateMail[names(stateMail) %in% names(wgtTbl)]
    statemail <- statemail[match(names(wgtTbl), names(statemail))]
    if (length(statemail) == 0) cols <- "white"
    else cols <- pal2[statemail + 1]
    # generate a new page
    grid.newpage()
    # now sort the table alphabetically
    ordering <- order(names(wgtTbl))
    wgtTbl <- wgtTbl[ordering]
    # get the radial positions of the points
    radPos <- seq(from = 0, to = 2*pi, length.out = length(wgtTbl) + 1)[1:length(wgtTbl)]
    ### old code which determined the position radially by the largest difference ####
    # define radial units
    #radunit <- 0.42/levelNum
    # determine inner level names
    #diffs <- diff(wgtTbl)
    # select the largest
    #impDiffs <- order(diffs)[1:(levelNum-1)]
    # extract names of the largest
    #innerCirc <- names(wgtTbl)[1:impDiffs]
    # now generate coordinates
    #xvals <- radunit*((!(names(wgtTbl) %in% innerCirc)) + 1)*cos(radPos) + 0.5
    #yvals <- radunit*((!(names(wgtTbl) %in% innerCirc)) + 1)*sin(radPos) + 0.5
    # generate coordinates ####
    # first define range
    radunit <- 0.37
    xvals <- (radunit*(1-(sqrt(wgtTbl)-sqrt(min(wgtTbl)))/(sqrt(max(wgtTbl))))+0.05)*cos(radPos)+0.5
    yvals <- (radunit*(1-(sqrt(wgtTbl)-sqrt(min(wgtTbl)))/(sqrt(max(wgtTbl))))+0.05)*sin(radPos)+0.5
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
               "Jake_Sullivan", "Philippe_Reines")
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
   titlePanel(HTML("Secretary Clinton's Email (Source: <a href = 'https://wikileaks.org/clinton-emails/'>Wikileaks</a>)")),
   fluidRow(column(h4("Christopher D. Salahub and R. Wayne Oldford"), offset = 0.2, width = 12)),

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
                      fluidRow(id = "Contents", h3("Table of Contents"),
                               p(HTML("<a href='#KeyPlayers'>Wikipedia Articles of Key Players</a>")),
                               p(HTML("<a href='#Timeline'>Timeline Links</a>")),
                               p(HTML("<a href='#Context'>Context Links</a>")),
                               p(HTML("<a href='#App'>Service Description</a>")),
                               p(HTML("<a href='#Data'>Data</a>")),
                               p(HTML("<a href='#Analysis'>Some Interesting Analyses</a>"))),
                      fluidRow(id = "KeyPlayers", 
                               h3("Wikipedia Articles of Key Players"),
                               p(HTML("<a href='#Contents'>Back to table of contents</a>")),
                               p(HTML(paste(paste(links, collapse = " | "),
                                            "<a href='https://en.wikipedia.org/wiki/Madeleine_Albright'>Madeleine Albright ('pathfinder')</a>",
                                            "<a href='https://en.wikipedia.org/wiki/Tony_Blair'>Tony Blair ('aclb')</a>",
                                            sep = " | ")))),
                      fluidRow(id = "Timeline", h3("The Email Timeline"),
                               p(HTML("<a href='#Contents'>Back to table of contents</a>")),
                               a("Sharyl Attkisson Timeline",  
                                 href = "https://sharylattkisson.com/hillary-clintons-email-the-definitive-timeline/"),
                               " | ",
                               a("The Washington Post", href = "https://www.washingtonpost.com/news/fact-checker/wp/2015/03/10/hillary-clintons-emails-a-timeline-of-actions-and-regulations/?utm_term=.dec3139a0542"),
                               " | ",
                               a("Wikipedia", href = "https://en.wikipedia.org/wiki/Hillary_Clinton_email_controversy")),
                      fluidRow(id = "Context", h3("Useful Context"),
                               p(HTML("<a href='#Contents'>Back to table of contents</a>")),
                               a("Clinton's tenure as Secretary of State",
                                 href = "https://en.wikipedia.org/wiki/Hillary_Clinton%27s_tenure_as_Secretary_of_State"),
                               " | ",
                               a("2012 Benghazi attack",
                                 href = "https://en.wikipedia.org/wiki/Timeline_of_the_investigation_into_the_2012_Benghazi_attack#October_2012")),
                      fluidRow(id = "App", h3("The Service"),
                               p(HTML("<a href='#Contents'>Back to table of contents</a>")),
                               p("This application provides the ability to interactively filter 32,795 emails
                                 sent during Hillary Clinton's tenure as the United States Secretary of 
                                 State and display features of the selected subset. The data is extracted
                                 from HTML representations of the ",
                                 a("official State Department release", href = "https://foia.state.gov/Search/Results.aspx?collection=Clinton_Email"),
                                 "provided in a ",
                                 a("Wikileaks data base.", href = "https://wikileaks.org/clinton-emails/")),
                               p("This service is not meant to provide stand-alone means of analyzing this
                                 controversial data set. It is most powerful when used simultaneously with
                                 both internet searches and the Wikileaks data base or official State 
                                 Department site. The latter two services provide indispensable context
                                 and precision; two services which the primarily 
                                 metadata-driven displays cannot provide. Rather, the intended use of this
                                 application is the exploration of patterns present in the data to
                                 generate and explore different hypotheses."),
                               p("It is hoped that this service will not simply provide you
                                 with a means of exploring this particular data set, but will
                                 demonstrate how much can be discovered about an individual
                                 using visual analytic tools of uninformative metadata to
                                 motivate searches of public data. The dissemination of data in
                                 the modern world is a topic of heated discussion, and hopefully
                                 experience firsthand exploring the way data can be leveraged
                                 will prove informative to you and help you to inform your own
                                 opinion on the subject.")),
                      fluidRow(id = "Data", h3("The Data"),
                               p(HTML("<a href='#Contents'>Back to table of contents</a>")),
                               p("This application allows the user to view the senders, recipients, times, ",
                                 a("Freedom of Information Act exemption codes",
                                   href = "https://vault.fbi.gov/explanation-of-exemptions"),
                                 "(applied by the State Department censors in the official release), the
                                 most frequent terms, and the most important terms as measured by the ", 
                                 a("tf-idf", href = "https://en.wikipedia.org/wiki/Tf%E2%80%93idf"),
                                 "over the filtered region selected.")),
                      fluidRow(id = "Analysis", 
                               h3("Some Interesting Analyses"),
                               p(HTML("<a href='#Contents'>Back to table of contents</a>")),
                               p(HTML("<a href='#Peak'>Peak Email</a>")),
                               p(HTML("<a href='#Gaps'>Email Gaps</a>")),
                               p(HTML("<a href='#EmTimes'>Email Times</a>"))),
                      fluidRow(id = "Peak",
                               h4("Peak Email"),
                               p(HTML("<a href='#Contents'>Back to table of contents</a>")),
                               p(HTML("<a href='#Analysis'>Back to analysis links</a>")),
                               p("Focusing on the email volume plot, an obvious peak can be seen
                                 near the centre of the time series. Using the date selection
                                 slider, the day of highest email volume can be identified as
                                 August 21, 2011, the beginning of the ",
                                 a("Battle of Tripoli", href = "https://en.wikipedia.org/wiki/Battle_of_Tripoli_(2011)"),
                                 "in the Libyan Civil War. Inspecting the term frequency and tf-idf
                                 for this day reveals a host of terms related to this conflict. The ",
                                 a("United States", href = "https://en.wikipedia.org/wiki/American_involvement_in_the_2011_Libyan_Civil_War"),
                                 "and ",
                                 a("NATO", href = "http://www.nato.int/cps/en/natohq/topics_71652.htm"),
                                 "were both heavily involved in this conflict, so this peak makes
                                 perfect sense. In fact, many other local maxima correspond to 
                                 events related to the ",
                                 a("Arab Spring", href = "https://en.wikipedia.org/wiki/Arab_Spring"),
                                 "and the countries affected by this revolutionary wave.")),
                      fluidRow(id = "Gaps",
                               h4("Email Gaps"),
                               p(HTML("<a href='#Contents'>Back to table of contents</a>")),
                               p(HTML("<a href='#Analysis'>Back to analysis links</a>")),
                               p("There are a number of conspicuous time periods where no emails
                                 are recorded in this data set. The most obvious of these occurs
                                 in early November 2012. This time period marks the beginning of 
                                 much of the ",
                                 a("increased controversy", href = "https://en.wikipedia.org/wiki/Timeline_of_the_investigation_into_the_2012_Benghazi_attack"),
                                 "surrounding the ",
                                 a("2012 attack", href = "https://en.wikipedia.org/wiki/2012_Benghazi_attack"),
                                 "on the US Diplomatic compound in Benghazi, and also includes the 
                                 2012 US Presidential Election."),
                               p("The time slider can be used to select a period surrounding this 
                                 gap which includes the Benghazi attack, take September 11 to 
                                 November 23. In this selection mentions of terms related to 
                                 this attack, such as Benghazi and Ansar al-Sharia, can be seen.
                                 The network plot also reveals one of the contentious points of 
                                 interest in Clinton's emails, the nature and frequency of her
                                 contact with ",
                                 a("Sidney Blumenthal", href = "https://en.wikipedia.org/wiki/Sidney_Blumenthal#Relationship_to_Hillary_Clinton_and_post.E2.80.932007_employment"),
                                 "during the Benghazi attack and shortly thereafter. We can also
                                 see contact with an account of unidentifiable domain with the
                                 label 'aclb.' Utilizing internet searches and inspecting emails,
                                 this account can be identified as that of Tony Blair, with the 
                                 four letter string likely standing for his ",
                                 a("full initials", href = "https://en.wikipedia.org/wiki/Tony_Blair"), 
                                 "Finally, many of the emails surrounding this gap contain some
                                 FOIA  redaction, as is clearly visible in the barplot of FOIA 
                                 redaction codes."),
                               p("Other gaps in the data can be found by narrowing the slider 
                                 range, selecting the centre bar, and dragging this small window
                                 across the whole time range with the 'Show Emails' filter set to
                                 show only mail from Clinton. Doing this, a number of periods of
                                 no email can be discovered. By selecting the foreign travel 
                                 tickbox, some of these can be identified as corresponding to
                                 official state visits. Other gaps occur near less typical events,
                                 such as a gap in mid June 2009, likely due to Clinton ",
                                 a("fracturing her elbow.", href = "http://www.nytimes.com/2009/06/19/us/politics/19clinton.html"),
                                 "Another gap in December 2012 corresponds with the ",
                                 a("resignation", href = "https://www.theguardian.com/world/2012/dec/19/benghazi-state-department-officials-resign"),
                                 "of four State Department officials due to the results of the
                                 Benghazi investigation."),
                               p("A number of other gaps of possible interest are not discussed
                                 here, and you are encouraged to investigate any period of
                                 interest you notice for yourself. However, you should
                                 always be mindful of the tendency for all of us to seek
                                 information which confirms preconceptions, and attempt as
                                 much as possible to be honest and unbiased in your 
                                 investigations.")),
                      fluidRow(id = "EmTimes",
                               h4("Email Times"),
                               p(HTML("<a href='#Contents'>Back to table of contents</a>")),
                               p(HTML("<a href='#Analysis'>Back to analysis links</a>")),
                               p("Filtering by the emails sent by Clinton during her tenure, we
                                 can glimpse her email sending patterns by looking at the email
                                 time plot. These patterns are fairly regular, with most activity
                                 occurring at night and a gap between roughly 6 and 11 pm. There 
                                 are also some strikingly consistent sending times visible in the 
                                 data, alternatingly at 2 and 3 am. The switching between 2 and
                                 3 am in these sending times matches exactly with the switching of
                                 daylight savings time in North America, suggesting these
                                 represent some server function performed every 24 hours.")))),

     # central interaction panel with a slider input for number of bins
     column(width = 2,
            fluidRow(selectInput("AdjVar", "Daily Reference Point: ", c("0:00", "18:00"),
                                 selected = "0:00")),
            fluidRow(checkboxInput("Schedule", "Display Foreign Travel Schedule")),
            fluidRow(selectInput("ToFromFilter", "Show Emails: ", 
                                 c("From Clinton", "To Clinton", "All Emails"),
                                 selected = "All Emails", multiple = FALSE)),
            fluidRow(selectInput("ClassFilter", 
                                 HTML("Contains any of the <a href='https://vault.fbi.gov/explanation-of-exemptions'>FOIA codes</a>:"),
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
    intermed$ZeroSel <- length(intermed$selIDs) == 0
    return(intermed)
  })
  # memoise the above function for caching
  inter <- inputProc(input)
  # now define all outputs
   output$Times <- renderPlot({
     # first determine whether adjusted or unadjusted times are to be used
     if (inter()$adjVar) {
       timevalues <- AsSec$Hour6pm[AsSec$ID %in% inter()$selIDs]*60 + 
         AsSec$Minutes[AsSec$ID %in% inter()$selIDs]
       xlab <- "Adjusted Date (dd/mm/yy)"
       main <- paste("Adjusted Email", inter()$toFromLab, "Times")
       labelset <- c('18:00', '24:00', '06:00', '12:00', '18:00')
     } else {
       timevalues <- AsSec$Hour[AsSec$ID %in% inter()$selIDs]*60 + 
         AsSec$Minutes[AsSec$ID %in% inter()$selIDs]
       xlab <- "Date (dd/mm/yy)"
       main <- paste("Email", inter()$toFromLab, "Times")
       labelset <- c("00:00", "06:00", "12:00", "18:00", "24:00")
     }
     # plot dates based on the dates provided from the slider
     plot(x = as.chron(floor(AsSec$Date[AsSec$ID %in% inter()$selIDs])),
          y = timevalues, xlab = xlab,
          ylab = paste("Time", inter()$toFromLab), yaxt = 'n', xaxt = 'n', pch = 19,
          main = main, sub = inter()$DateRange,
          col = adjustcolor(pal[as.numeric(AsSec$Redacted[AsSec$ID %in% inter()$selIDs])+1], 
                            alpha.f = 0.5),
          cex = 0.25, ylim = c(1440,0) + c(0.05,-0.05)*1440, 
          xlim = c(input$range[1], input$range[2]))
     axis(side = 2, at = c(0, 360, 720, 1080, 1440),
          labels = labelset, las = 1)
     axis.Date(side = 1, as.chron(c(input$range[1], input$range[2])), format = "%d/%m/%y")
     legend("topright", legend = c("Redacted", "Unedited"), pch = c(19,19),
           col = c("firebrick", "steelblue"), horiz = TRUE, cex = 0.8, inset = c(0,-0.05),
           xpd = TRUE)
   })
   # next display the time series of emails sent by day
   output$DaySum <- renderPlot({
     plot(x = inter()$selDays, y = inter()$selCounts, type = 'l', xlab = 'Date (dd/mm/yy)',
          xaxt = "n", ylab = 'Number of Emails', pch = 19,
          main = "Number of Emails by Date", sub = inter()$DateRange,
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
     tempDays <- seq(from = min(inter()$selDays),
                   to = max(inter()$selDays),
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
     if (!inter()$ZeroSel) {
       barplot(table(unlist(str_split(AsSec$Classification[AsSec$ID %in% inter()$selIDs], "-"))),
               main = "FOIA Redaction Codes Appearing in Selected Emails",
               sub = inter()$DateRange)
       } else {
         plot(NA, xlim = 0:1, ylim = 0:1, xaxt = 'n', yaxt = 'n', ylab = "", xlab = "")
         text("No Emails", x = 0.5, y = 0.5)
    })
   
   # record the user window size (still cannot use in ui)
   # output$winHeight <- reactive(input$height)
   # add server link processing
}

# Run the application
shinyApp(ui = ui, server = server)

