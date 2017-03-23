# load required packages
library(RCurl)
library(XML)
library(tm)
library(stringr)
library(SnowballC)
library(chron)
library(dplyr)
library(loon)

# load the raw HTML data if desired
fullmail <- readRDS('ClintonsEmails')

### Email Extraction Functions #########################################################
# generate a function to clean up emails using the library chron
infoExtractor <- function(emails, ids, includeRaw = FALSE) {
  # first identify whether the email is at all redacted
  redacted <- grepl(x = emails, pattern = "RELEASE\\s*IN\\s*PART")
  # extract the information on sender and recipient
  # extract the recipient information from the Wikileaks header
  to <- str_extract(emails, "(?<=(To\\: <span title=\"Original: )).+(?=</span>)")
  nameTo <- str_extract(to, "(?<=(\">)).+")
  addressTo <- str_extract(to, "[\\.a-zA-Z0-9\\_]+@[\\.a-zA-Z0-9]+")
  # extract the sender information from the Wikileaks header
  from <- str_extract(emails, "(?<=(From\\: <span title=\"Original: )).+(?=</span>)")
  nameFrom <- str_extract(from, "(?<=(\">)).+")
  addressFrom <- str_extract(from, "[\\.a-zA-Z0-9\\_]+@[\\.a-zA-Z0-9]+")
  # also extract the forwarding contact chain using generic email matching on "@"
  forwards <- str_extract_all(emails, "[a-zA-Z0-9]+\\@[a-zA-Z0-9\\.]+|To:|From:")
  forwards <- lapply(forwards, 
                     function(el) c(rbind(el[which(grepl(x = el, pattern = "@"))-1],
                                          el[which(grepl(x = el, pattern = "@"))]
                                          )))
  forwards <- unlist(lapply(forwards, function(el) paste(el, collapse = " ")))
  # extract the subject line
  subj <- str_extract(emails, "(?<=(Subject\\:)).+")
  # extract the time and date information
  date <- str_extract(emails, "(?<=(Date\\:\\s)).+")
  # clean the extracted times and dates up
  date <- str_replace_all(date, "[^\\s0-9\\-\\:]", "")
  # convert these into time structures
  # apply to the times from above a function which gets date and time information
  chr_tm <- lapply(date, function (tm) {
    chron(dates. = str_split(tm, " ")[[1]][1], 
          times. = paste(str_split(tm, " ")[[1]][2], ':00', sep = ''),
          format = c(dates = "y-m-d", times = "h:m:s"))})
  chr_tm <- unlist(chr_tm)
  # now extract the email content using Wikileaks content tab
  content <- str_extract(emails, "(?<=(<div class=\"email-content\" id=\"uniquer\">))(?s).+(?-s)(?=(<div class=\"tab-pane fade in\" id=\"source\">))")
  # also extract all possible classifications used in the email
  Clevel <- str_extract_all(content, "B[1-9](?![0-9a-zA-Z])")
  Clevel <- unlist(lapply(Clevel, function(el) {
    if(length(el)==0) "None"
    else paste(sort(unique(el)), collapse = "-")}))
  # next clean the email content
  cln_txt <- str_replace_all(content, pattern = "\\s+|\"", " ")
  cln_txt <- str_replace_all(cln_txt, 
                             pattern = "<span(?s).*?(?s)</span>|</div>|B[0-9]+", "")
  cln_txt <- str_replace_all(cln_txt, pattern = "\\s+", " ")
  # collect all this data into a structure
  emailData <- data.frame(ID = ids, To.name = nameTo, To.Add = addressTo,
                          From.name = nameFrom, From.Add = addressFrom,
                          Subject = subj, Date = chr_tm,
                          Day = days(chr_tm), Month = months(chr_tm), 
                          Year = years(chr_tm), Weekday = weekdays(chr_tm),
                          Hour = hours(chr_tm), Minutes = minutes(chr_tm),
                          Redacted = redacted, Forwards = forwards,
                          Content = cln_txt, Classification = Clevel)
  # now if we want raw output alongside the processed output, include the raw header
  # and the raw email content
  if (includeRaw) {
    header <- str_extract(emails, "<header id=\"header\">(?s).+(?-s)<div class=\"email-content\" id=\"uniquer\">")
    # add this raw header and the raw content to the data
    emailData <- data.frame(emailData, rawHead = header, rawCont = content)
  }
  # return the data structure
  return(emailData)
}

# define a function which accepts some basic user input and pulls the
# corresponding emails, the max was 30322, but addition of more increased it to 32795
get_Clin_emails <- function(ids = 1:32795, # give possible emails for selection
                            size = length(ids), # size of selection
                            random = FALSE, # determine if selection is random
                            replaceOutput = TRUE, # determine if old output is kept
                            contAtErr = NULL, # provide the option of specifying default
                            # error handling procedure
                            includeRaw = FALSE # pass along argument to specify if
                            # raw html data is to be included
) 
{
  # check if output has already been generated
  output.exist <- file.exists("ClintonEmails.csv")
  # if so extract the ids which are included
  if (output.exist) {
    prevPull <- read.csv("ClintonEmails.csv")
    # now, if past output is to be rewritten, do not select emails with ID values
    if (replaceOutput) prevPull <- prevPull[!(prevPull$ID %in% ids),]
    else {
      ids <- ids[!(ids %in% prevPull$ID)]
      # it is possible that we have already extracted all IDs provided
      if (length(ids) == 0) return("Given IDs already extracted")
    }
  } else prevPull <- NULL
  # check if all IDs provided exist
  if (!all(ids %in% 1:32795)) {
    # give a warning
    warning("IDs must be integers between 1 and 32795: continuing only with valid IDs")
    ids <- ids[ids %in% 1:32795] # select the valid ids
  }
  # check the size of the selection and the range
  if (size <= length(ids)) N <- size
  # if the size given is too big give a warning and simply take the length of the
  # list given
  else {
    warning("Size provided was too large")
    N <- length(ids)
  }
  # check for randomness
  if (random) ids <- sample(ids, size = N)
  # if there is none take the first elements
  else ids <- ids[1:N]
  # define the base url to make access easier later
  baseURL <- 'https://wikileaks.org/clinton-emails/emailid/'
  # generate storage for future use
  emails <- rep("", N)
  failures <- rep(0, N)
  idx <- 1
  continue <- TRUE
  while (idx <= N & continue) {
    # generate the specific URL
    specURL <- paste(baseURL, ids[idx], sep = '')
    # extract the web page
    tryURL <- tryCatch(getURL(specURL),
                       error = function(err) {
                         return(c("Failed", err))})
    # prompt the user if this is an error
    if (length(tryURL) > 1) {
      # present the error and prompt for input if the default action is unset
      if (is.null(contAtErr)) {
        continue <- as.logical(readline(prompt = paste("Error:", tryURL[2],
                                                       "     Continue? (T/F)")))
      }
      # if default is set use the default
      else continue <- contAtErr
      # save the error index
      failures[idx] <- 1
    }
    # if there is no error continue as normal
    else {
      # now save the data in a vector
      emails[idx] <- tryURL
      # provide a message on progress
      if(idx %% 10 == 0) message(paste(idx/N*100, '% completed at', Sys.time()))
    }
    # update the index value
    idx <- idx + 1
    # set a random pause length
    pause.length <- runif(min = 0.05, max = 0.1, n = 1)
    # pause to ensure requests are not sent too quickly
    Sys.sleep(pause.length)
  }
  # remove the entries which failed
  emails <- emails[emails != ""]
  # provide a report of the failed emails
  if (sum(failures) > 0) {
    message(paste("Failed:", paste(ids[which(failures == 1)], collapse = ",")))
    assign("Failtures", ids[which(failures == 1)])
  }
  # now clean this data using a regex helper
  newMailData <- infoExtractor(emails, ids = ids[!failures], includeRaw)
  # combine this with the previous email data, making sure dimension matches
  if ((is.null(prevPull$rawHead)|is.null(prevPull$rawCont)) & includeRaw & output.exist) {
    # if the dimension doesn't, add columns appropriately
    prevPull <- data.frame(prevPull, rawHead = rep("", dim(prevPull)[1]),
                           rawCont = rep("", dim(prevPull)[1]))
  }
  FullData <- rbind(newMailData, prevPull)
  # save this
  write.csv(FullData, "ClintonEmails.csv", row.names = FALSE)
  # return this
  return(FullData)
}

### Pre-loaded data #####################################################################
EmailData <- read.csv('ClintonEmailData2.csv')

### Thompson Timeline ###################################################################
# a function to pull the timeline compiled on the Thompson timeline, not used in the final
# analysis as it did not seem a reliable source
get_ThompTime <- function() {
  # define the base url to make access easier later
  baseURL <- "http://www.thompsontimeline.com/category/clinton-email-server/timeline-long/page/"
  # generate storage for future use
  timelineData <- rep("", 24)
  # pull first page (slightly different than the others)
  specURL <- "http://www.thompsontimeline.com/category/clinton-email-server/timeline-long/"
  # extract the web page
  timelineData[1] <- getURL(specURL)
  # now for the general case
  for (pg in 2:24) {
    # generate the specific URL
    specURL <- paste(baseURL, pg, "/", sep = '')
    # extract the web page
    timelineData[pg] <- getURL(specURL)
    # set a random pause length
    pause.length <- runif(min = 0.05, max = 0.1, n = 1)
    # pause to ensure requests are not sent too quickly
    Sys.sleep(pause.length)
  }
  # use regex to extract the useful parts
  pgHeaders <- unlist(str_extract_all(timelineData, "(?<=(<h2 class=\"entry-title\">))(?s).*?(?-s)(?=(</h2>))"))
  # now extract the more specific information from this
  pgDates <- str_extract_all(pgHeaders, "(?<=(<span class=\"date-range\">)).*?(?=(: </span>))")
  pgHeadLine <- str_extract_all(pgHeaders, "(?<=(\\&quot;)).+?(?=(\\&quot;))")
  # output the information
  dates <- matrix(c(unlist(pgDates), unlist(pgHeadLine)), ncol = 2)
  # replace peculiarities
  dates[,2] <- str_replace_all(dates[,2], pattern = "&#8217;|&#8220;|&#8221;", 
                               replacement = "'")
  return(dates)
}

# extract the times
ThompsonTimeline <- get_ThompTime()
# perform some replacements as identified later, these are specific and based on searching
# the document for issues
ThompsonTimeline[c(11,139,179,188,215,236,256,264,306,315,447,462,530,742,775,323),1] <-
  c("January 1, 2006", "June 1, 2010", "April 1, 2011-May 31, 2011", 
    "May 13, 2011-May 14, 2011", "August 18, 2011-August 19, 2011", 
    "December 23, 2011-December 27, 2011", "June 19, 2012-June 20, 2012", 
    "August 1, 2012-December 31, 2012", "March 31, 2013", "March 31, 2013", 
    "March 10, 2015", "March 25, 2015-March 31, 2015", "August 8, 2015",
    "March 2, 2016-March 3, 2016", "April 1, 2016-May 31, 2016",
    "June 1, 2013")

# define a helper for processing the dates given
DateConverter <- function(date) {
  # have a month vector for reference
  months <- c("January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December")
  # split the date for rearrangement
  splt_date <- unlist(str_split(date, pattern = " |, "))
  # extract year
  yr <- splt_date[grepl(splt_date, pattern = "[0-9]{4}")]
  # extract month
  mnth <- splt_date[splt_date %in% months]
  # extract day
  dy <- splt_date[!(splt_date %in% c(mnth, yr)) & grepl(x = splt_date,
                                                        pattern = "[0-9]+")]
  # define month numerically
  mnth <- which(months == mnth)
  # if no day is specified simply choose the first of the month, no month choose January
  if (length(dy) == 0) dy <- 1
  if (length(mnth) == 0) mnth <- 1
  # put it all together
  finDate <- tryCatch(dates(paste(dy, mnth, yr, sep = "/"), format = "d/m/y"),
                      error = function(err) message(date))
  finDate
}

# a wrapper to process all the dates
DateCleaner <- function(dates, headlines) {
  # identify inexact dates
  inexact <- grepl(x = dates, pattern = "Between|Around|Late|Early|Mid|Thereafter|Before|Later|Shortly")
  # remove problem words related to the above inexact dates
  dates <- str_replace_all(dates, 
                           "Mid\\p{Pd}|:|Around|Earlier| or Shortly Thereafter|Earlier",
                           "")
  # first split the dates
  split_dates <- str_split(dates, pattern = "\\p{Pd}| and | or | OR")
  # regularize by duplicating singular elements
  split_dates <- lapply(split_dates, function(splt) {
    if (length(splt) == 1) c(splt, splt)
    else splt
  })
  # now unlist and apply the processing helper
  proc_dates <- sapply(unlist(split_dates), DateConverter, USE.NAMES = FALSE)
  # return the processed values as a matrix
  dateMat <- matrix(NA, nrow = 1192, ncol = 2)
  for (ii in 1:1192) dateMat[ii,] <- proc_dates[[ii]]
  data.frame(Start = dateMat[,1], End = dateMat[,2], Inexact = inexact,
             Headline = headlines)
}

ThompsonTimeline <- DateCleaner(ThompsonTimeline[,1], ThompsonTimeline[,2])


### Official Schedule ###################################################################
# create a helper to process official foreign schedule dates from the state department
schedProc <- function(date) {
  # have a month vector for reference
  months <- c("January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December")
  # split the years
  spltDates <- unlist(str_split(date, pattern = ", "))
  yr <- spltDates[2]
  # next split the days and possibly months
  days <- unlist(str_split(spltDates[1], pattern = "\\p{Pd}"))
  # check if it is a one day trip, in this case repeat the date twice
  if (length(days) == 1) {
    mnth1 <- unlist(str_split(days, pattern = " "))[1]
    day1 <- unlist(str_split(days, pattern = " "))[2]
    mnth2 <- mnth1
    day2 <- day1
  }
  # otherwise get both dates
  else{
    # check the case (should the month apply to both or not?)
    InclMnth <- grepl(x = days, pattern = "[a-zA-Z]+")
    # deal with the different cases
    if (InclMnth[1] & InclMnth[2]) {
      # if both have a month we must separate the dates
      mnth1 <- unlist(str_split(days[1], pattern = " "))[1]
      day1 <- unlist(str_split(days[1], pattern = " "))[2]
      mnth2 <- unlist(str_split(days[2], pattern = " "))[1]
      day2 <- unlist(str_split(days[2], pattern = " "))[2]
    }
    # if both don't have a month then the first will and the second won't
    else {
      mnth1 <- unlist(str_split(days[1], pattern = " "))[1]
      day1 <- unlist(str_split(days[1], pattern = " "))[2]
      mnth2 <- mnth1
      day2 <- days[2]
    }
  }
  # now to generate the two dates to return
  startdate <- dates(paste(day1, which(months == mnth1), yr, sep = "/"), 
                     format = "d/m/y")
  enddate <- dates(paste(day2, which(months == mnth2), yr, sep = "/"), 
                   format = "d/m/y")
  # return these
  c(startdate, enddate)
}

# create a function to pull Clinton's foreign trip schedule
ClinForeignSched <- function() {
  # define the URL of interest
  schedURL <- "https://history.state.gov/departmenthistory/travels/secretary/clinton-hillary-rodham"
  # pull the URL
  schedDat <- getURL(schedURL)
  # extract all schedule entries
  schedEntries <- unlist(str_extract_all(schedDat, 
                                         pattern = "(?<=(<tr>))(?s).*?(?-s)(?=(</tr>))"))
  # now extract more specific information
  schedEntries <- str_replace_all(schedEntries[-1], 
                                  pattern = "(?<=(</td>))\\s+(?=(<td>))",
                                  replacement = "")
  # trim whitespace
  schedEntries <- trimws(schedEntries, "both")
  # perform some final cleanup
  schedEntries <- str_replace_all(schedEntries, pattern = "\\s+", replacement = " ")
  # now split and separate
  schedEntries <- str_split(schedEntries, pattern = "<td>|</td>")
  # finally remove empty splits
  schedEntries <- lapply(schedEntries, function(vec) vec[vec != ""])
  # now extract information
  country <- sapply(schedEntries, function(vec) vec[1])
  city <- sapply(schedEntries, function(vec) vec[2])
  reason <- sapply(schedEntries, function(vec) vec[3])
  dates <- dates(t(sapply(schedEntries, function(vec) schedProc(vec[4]))))
  # save and return this data
  sched <- data.frame(Country = country, City = city, Reason = reason,
                      StartDate = as.numeric(dates[,1]), 
                      EndDate = as.numeric(dates[,2]))
  sched
}

Schedule <- ClinForeignSched()

### Hard-coded Favourability Ratings ####################################################
Favourability <- data.frame(Date = dates(c('01/05/2008', '01/11/2009', '01/12/2010',
                                            '01/01/2012', '01/06/2012', '01/12/2012',
                                            '01/10/2013'),
                                          format = 'd/m/y'),
                            Favourability = c(0.48, 0.66, 0.56, 0.62, 0.63, 0.65,
                                               0.53))


### Putting it all together #############################################################

# start by splitting the data by Clinton's secretary of state tenure
NotSec <- filter(EmailData, Year < 2009 | Year > 2013)
AsSec <- filter(EmailData, Year >= 2009 & Year <= 2013)

# let's see what times of day have a lot of email activity before and after
# Clinton became the secretary of state, use some alpha blending and colouring
# to visualize it
plot(y = AsSec$Hour*60 + AsSec$Minutes, 
     x = rnorm(nrow(AsSec), sd = 0.025, mean = 0.25),
     xlim = extendrange(c(0,1)), xlab = '',
     ylab = 'Time (Minutes After Midnight)', main = 'Emails During the Day', 
     col = adjustcolor('red', alpha.f = 1/100), pch = 19,
     xaxt = 'n')
points(y = NotSec$Hour*60 + NotSec$Minutes,
       x = rnorm(nrow(NotSec), sd = 0.025, mean = 0.75),
       col = adjustcolor('blue', alpha.f = 1/100),
       pch = 19)
legend(x = 'topright', 
       legend = c('As Secretary of State', 'Before Secretary of State'),
       col = c('red','blue'), pch = 19)
# how about overplotting to see what the patterns are day by day
# first see if any weekday is favoured
barplot(table(factor(as.character(AsSec$Weekday), 
                     levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))))
# create a simple two-colour palette
pal <- c("firebrick", "steelblue")
# looks pretty even, now overplot
plot(NA, xlim = c(0,7), ylim = c(0,1440), xaxt = 'n', yaxt = 'n', xlab = "Weekday",
     ylab = "Time Sent", main = "Time Sent by Day of the Week")
axis(side = 1, at = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5),
     labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"),
     tick = FALSE)
axis(side = 1, at = c(0,1,2,3,4,5,6,7), labels = NA)
axis(side = 2, at = c(240, 480, 720, 960, 1200), 
     labels = c("04:00", "08:00", "12:00", "16:00", "20:00"))
for (day in 1:7) {
  dylst <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  mail <- filter(AsSec, Weekday == dylst[day])
  points(x = (mail$Date - min(AsSec$Date))/(max(AsSec$Date) - min(AsSec$Date)) + day - 1,
       y = mail$Hour*60 + mail$Minutes, pch = 19, cex = 0.3,
       col = adjustcolor(pal[as.numeric(mail$Redacted)+1], alpha.f = 0.5))
}
legend("topright", legend = c("Redacted", "Unedited"), pch = c(19,19), 
       col = c("steelblue", "firebrick"), horiz = TRUE, cex = 0.5, inset = c(0,-0.1),
       xpd = TRUE)
# we can break this down by time for the emails sent as the secretary of state
plot(x = as.chron(AsSec$Date),
     y = AsSec$Hour*60 + AsSec$Minutes, xlab = 'Date', ylab = 'Time Sent',
     yaxt = 'n', pch = 19, main = "Email Sending Times Over Clinton's Tenure",
     col = adjustcolor(pal[as.numeric(AsSec$Redacted)+1], alpha.f = 0.5), cex = 0.25)
axis(side = 2, at = c(240, 480, 720, 960, 1200), 
     labels = c('04:00', '08:00', '12:00', '16:00', '20:00'))
legend("topright", legend = c("Redacted", "Unedited"), pch = c(19,19), 
       col = c("steelblue", "firebrick"), horiz = TRUE, cex = 0.5, inset = c(0,-0.1),
       xpd = TRUE)
# both of these suggest that a different definition of 'day' might be useful,
# where the new days are defined by 24 hour intervals defined by 6 pm rather than
# defined by midnight
# first arrange the emails sequentially
AsSec <- arrange(AsSec, Date)
# now define the days
days6pm <- seq(from = floor(min(AsSec$Date)) + 0.75, to = max(AsSec$Date),
               by = 1)
newDays <- sapply(AsSec$Date, function(date) sum(days6pm < date) %% 7) + 1
newDays <- c("Sat-Sun","Sun-Mon","Mon-Tue","Tue-Wed","Wed-Thu","Thu-Fri",
             "Fri-Sat")[newDays]
AsSec$Weekday6pm <- newDays
# and generate new times referencing 6 pm
AsSec$Hour6pm <- (AsSec$Hour + 6) %% 24
# let's plot this now
plot(x = as.chron(AsSec$Date),
     y = AsSec$Hour6pm*60 + AsSec$Minutes, xlab = 'Adjusted Date', ylab = 'Time Sent',
     yaxt = 'n', pch = 19, main = "Adjusted Email Sending Times",
     col = adjustcolor(pal[as.numeric(AsSec$Redacted)+1], alpha.f = 0.5), cex = 0.25)
axis(side = 2, at = c(240, 480, 720, 960, 1200), 
     labels = c('22:00', '02:00', '06:00', '10:00', '14:00'))
legend("topright", legend = c("Redacted", "Unedited"), pch = c(19,19), 
       col = c("steelblue", "firebrick"), horiz = TRUE, cex = 0.5, inset = c(0,-0.1),
       xpd = TRUE)
# we can also look at the emails by day of the year and calendar date
# first use 'tally' and 'group_by' in dplyr to extract counts
countbyDate <- tally(group_by(AsSec, dates(as.chron(Date))))
ASDays <- seq(from = min(countbyDate$`dates(as.chron(Date))`),
              to = max(countbyDate$`dates(as.chron(Date))`),
              by = 'days')
AScounts <- rep(0, length(ASDays))
AScounts[ASDays %in% countbyDate$`dates(as.chron(Date))`] <- countbyDate$n

defmar <- par()$mar
par(mar = c(c(5.1, 4.1, 4.1, 4.1)))
plot(x = ASDays, y = AScounts, type = 'l', xlab = 'Date',
     ylab = 'Number of Emails Sent', main = "Number of Emails Sent by Date")
axis(side = 4, at = seq(0, 122, length.out = 11), labels = seq(0,1, length.out = 11))
mtext("Favourability", side = 4, line = 2.5)
lines(x = Favourability$Date, y = Favourability$Favourability*122, col = "red")
# now add the trip schedule
for (ii in 1:239) {
  polygon(x = c(Schedule[ii, c("StartDate", "EndDate")], 
                rev(Schedule[ii, c("StartDate", "EndDate")])),
          y = c(-50,-50,130,130), col = adjustcolor("steelblue", alpha.f = 0.4),
          border = NA)
}
# add the Thompson Timeline
for (ii in 1:1192) {
  start <- ThompsonTimeline[ii, c("Start")]
  end <- ThompsonTimeline[ii, c("End")]
  if (start == end) {
    abline(v = ThompsonTimeline[ii, c("Start")], 
           col = adjustcolor("firebrick", alpha.f = 0.4))
  }
  else {
    polygon(x = c(ThompsonTimeline[ii, c("Start", "End")],
                  rev(ThompsonTimeline[ii, c("Start", "End")])),
            y = c(-50,-50,130,130), col = adjustcolor("firebrick", alpha.f = 0.2),
            border = NA)
  }
}
# re-sort by ID
AsSec <- arrange(AsSec, ID)
# we can use loon to interact with this plot much more easily
mail <- l_plot(x = ASDays, y = AScounts, xlabel = 'Date',
               ylabel = 'Number of Emails Sent', size = 0,
               itemlabel = as.character(ASDays), color = "black")
l_layer_line(mail, x = ASDays, y = AScounts)
l_layer_line(mail, x =rep(as.numeric(ASDays[ASDays == "09/11/12"]), 2),
             y = c(0, 125), col = 'red')
# alternatively, we can use a double-sided slider and basic R, as shown below

### Keyword Analysis ####################################################################
# define a function which can be used to clean and process email content
get_keywords <- function(Content, stem = TRUE, table = TRUE) {
  # first combine all the emails into one string
  corpus <- paste(Content, collapse = " ")
  # make everything lowercase for reference
  corpus <- tolower(corpus)
  # start by removing a number of problematic terms
  corpus <- str_replace_all(corpus, "u.s.|u.s.a.", "usa")
  # remove all non-alphanumeric characters
  # first remove the contractions and commas used in number display
  corpus <- str_replace_all(corpus, "[\\'\\.]", "")
  corpus <- str_replace_all(corpus, "(?<=[0-9])\\,(?=[0-9])", "")
  # now safely remove everything else
  corpus <- str_replace_all(corpus, "[^0-9a-zA-Z\\s]", " ")
  # replace all duplicate spaces
  corpus <- str_replace_all(corpus, "\\s+", " ")
  # split by singular spaces
  corpus <- unlist(str_split(corpus, " "))
  # remove common stopwords
  corpus <- removeWords(corpus, c(stopwords(), "s"))
  # remove empty strings which may have occurred
  corpus <- corpus[corpus != ""]
  # stem the remaining terms
  if (stem) corpus <- stemDocument(corpus)
  # now to extract the unique words used
  if (table) corpus <- sort(table(corpus), decreasing = TRUE)
  corpus
}

# run this on the main data
keywords <- get_keywords(AsSec$Content)
# set the cutoff
cutoff <- 200
# select arbitrary relevant keywords
relKey <- names(keywords)[keywords > cutoff]
# generate the term document matrix
ASmat <- lapply(lapply(AsSec$Content, function(e) get_keywords(e, table = FALSE)), 
                function(email) sapply(relKey, function(kw) sum(kw == email)))
ASmat <- matrix(unlist(ASmat), ncol = length(relKey), byrow = TRUE)
# load the term-document matrix
ASmat <- read.csv('TermDocumentMatrix.csv')
# generate a tfidf matrix
tfIdf <- apply(ASmat, 2, function(col) col/max(col) * log(length(col)/sum(col != 0)))
colnames(tfIdf) <- relKey

### Frequency and Email Network Plots with Sliders ######################################
# let's do some processing by days and extract relevant information
Days <- unique(floor(AsSec$Date))
# now extract information for each day
# first define a simple helper function to extract at most the top N
# elements of a sorted list
topN <- function(tble, n = 10) {
  tble <- sort(tble, decreasing = TRUE)
  tble[tble != 0][1:min(length(tble), n)]
}
# a row sum helper
cSum <- function(mat) {
  if (is.null(dim(mat))) mat
  else colSums(mat)
}
# a rescaling helper
rescale <- function(vec) {
  (vec - min(vec))/max(vec)
}
# a na replacement helper
narep <- function(vec) {
  vec[is.na(vec)] <- "Unknown"
  vec
}
# a helper to remove repeated columns
repRem <- function(mat) {
  if (is.null(dim(mat))) t(as.matrix(mat))
  else mat[mat[,1] != mat[,2],]
}
# now extract
DayFrom <- lapply(Days, 
                  function(dy) AsSec$From.name[floor(AsSec$Date) == dy])
names(DayFrom) <- Days
DayTo <- lapply(Days,
                function(dy) AsSec$To.name[floor(AsSec$Date) == dy])
names(DayTo) <- Days
DayKW <- lapply(Days,
                function(dy) topN(get_keywords(AsSec$Content[floor(AsSec$Date) == dy],
                                               stem = TRUE),
                                  n = 20))
names(DayKW) <- Days
DayKW_tfidf <- lapply(Days,
                      function(dy) topN(cSum(tfIdf[which(floor(AsSec$Date) == dy),]),
                                        n = 20))
names(DayKW_tfidf) <- Days
# create a slider to display this information interactively throughout time
tt <- tktoplevel()
tt2 <- tktoplevel()
tktitle(tt) <- "Network"
tktitle(tt2) <- "Keywords"
MailRng <- range(Days)
sel <- min(Days)
p <- l_plot(x = rep(c(0.33, 0.66), times = 10), 
            y = rep(seq(from = 0.95, to = 0.05, length.out = 10), each = 2),
            parent = tt2,
            size = 6 + 10*rescale(DayKW_tfidf[[as.character(sel)]]),
            title = paste('TF-IDF', as.character(dates(as.chron(sel)))),
            xlabel = "", ylabel = "", color = 'black')
names <- l_glyph_add_text(p, text = names(DayKW_tfidf[[as.character(sel)]]))
p['glyph'] <- names
p2 <- l_plot(x = rep(c(0.33, 0.66), times = 10), 
             y = rep(seq(from = 0.95, to = 0.05, length.out = 10), each = 2),
             parent = tt2,
             size = 6 + 10*rescale(DayKW[[as.character(sel)]]),
             title = paste('Frequency', as.character(dates(as.chron(sel)))),
             xlabel = "", ylabel = "", color = 'black')
names2 <- l_glyph_add_text(p2, text = names(DayKW[[as.character(sel)]]))
p2['glyph'] <- names2
node_names <- narep(unique(c(as.character(DayFrom[[as.character(sel)]]),
                             as.character(DayTo[[as.character(sel)]]))))
FromTo <- unique(cbind(narep(as.character(DayFrom[[as.character(sel)]])),
                       narep(as.character(DayTo[[as.character(sel)]]))),
                 MARGIN = 1)
grph <- loongraph(nodes = node_names,
                  from = FromTo[,1],
                  to = FromTo[,2],
                  isDirected = TRUE)
g <- l_graph(grph, parent = tt,
             title = paste('Communication Graph', sel))
# define an update function
update_plot <- function() {
  Dt <- as.character(dates(as.chron(as.numeric(tclvalue(SliderValue)))))
  sel <- as.numeric(tclvalue(SliderValue))
  l_configure(p, title = paste('TF-IDF', Dt))
  l_configure(p2, title = paste('Frequency', Dt))
  if (sel %in% Days) {
    node_names <- sort(unique(narep(c(as.character(DayFrom[[as.character(sel)]]),
                                      as.character(DayTo[[as.character(sel)]])))))
    FromTo <- unique(cbind(narep(as.character(DayFrom[[as.character(sel)]])),
                           narep(as.character(DayTo[[as.character(sel)]]))),
                     MARGIN = 1)
    l_configure(g, nodes = node_names, from = FromTo[,1], to = FromTo[,2],
                title = paste('Communication Graph', sel))
    names <- l_glyph_add_text(p, text = names(DayKW_tfidf[[as.character(sel)]]))
    names2 <- l_glyph_add_text(p2, text = names(DayKW[[as.character(sel)]]))
    l_configure(p, glyph = names,
                size = 6 + 10*rescale(DayKW_tfidf[[as.character(sel)]]))
    l_configure(p2, glyph = names2,
                size = 6 + 10*rescale(DayKW[[as.character(sel)]]))
    l_scaleto_world(g)
    l_scaleto_world(p)
    l_scaleto_world(p2)
    tcl("update", "idletasks")
  }
  else {
    names <- l_glyph_add_text(p, text = rep('No Mail', 20))
    names2 <- l_glyph_add_text(p2, text = rep('No Mail', 20))
    l_configure(p, glyph = names)
    l_configure(p2, glyph = names2)
    tcl("update", "idletasks")
  }
}
# add a slider
slider <- tkscale(tt, from = min(Days), to = max(Days), resolution = 1, 
                  showvalue = TRUE, orient = "horizontal")
SliderValue <- tclVar(min(Days))
tkconfigure(slider, variable = SliderValue, command = function(...) update_plot())
tkpack(slider, fill = "x", side = "bottom")
tkpack(p, side = "left", fill = "both", expand = 1)
tkpack(p2, side = "right", fill = "both", expand = 1)
tkpack(g, side = "top", fill = "both", expand = 1)

### Spiral Network Plot Function ########################################################
library(grid)
# first filter out those communications which involve Clinton
ClintonCom <- filter(EmailData, 
                     From.name == "Hillary Clinton" | To.name == "Hillary Clinton" |
                       From.name == "H" | To.name == "H")
# extract the names of all addresses that communicated with Clinton
ClintNet <- sort(table(c(as.character(ClintonCom$To.name), 
                         as.character(ClintonCom$From.name))),
                 decreasing = TRUE)[-1]
# write the function to generate the spiral network plot
spiralNetPlot <- function(centralNode = "Hillary Clinton", wgtTbl = NA,
                           levelNum = 2, nodeNum = 10, title = "Title") {
  # check the wgtTbl status
  if (!all(is.na(wgtTbl))) {
    # start by generating state mail colouring
    statemail <- stateMail[names(stateMail) %in% names(wgtTbl)]
    statemail <- statemail[match(names(wgtTbl), names(statemail))]
    if (length(statemail) == 0) cols <- "white"
    else cols <- c("firebrick", "steelblue")[statemail + 1]
    # generate a new page
    grid.newpage()
    # define radial units
    radunit <- 0.45/levelNum
    # determine inner level names
    diffs <- diff(wgtTbl)
    # select the largest
    impDiffs <- order(diffs)[1:(levelNum-1)]
    # get the radial positions of the first level
    radPoslvl1 <- seq(from = 0, to = 2*pi, length.out = impDiffs[1] + 1)[1:impDiffs[1]]
    # add the second level, see how much space has to be left in between axes
    numRot <- ceiling((length(wgtTbl) - impDiffs[1])/impDiffs[1])
    numFull <- floor((length(wgtTbl) - impDiffs[1])/impDiffs[1])
    # determine the major positions
    shift <- (2*pi)/((numRot+1)*impDiffs[1])
    radPoslvl2 <- c(outer(radPoslvl1, shift*(1:numFull), FUN = function(x,y) x + y))
    # add in the rest
    extraPos <- if(numRot-numFull == 0) NULL else(radPoslvl2 +
                                                    numFull*shift)[1:(length(wgtTbl) - 
                                                                        impDiffs[1]*(numFull+1))]
    # combine
    radPoslvl2 <- c(radPoslvl2, extraPos)
    # now extract coordinates
    lvl1x <- radunit*cos(radPoslvl1) + 0.5
    lvl1y <- radunit*sin(radPoslvl1) + 0.5
    lvl2x <- 2*radunit*cos(radPoslvl2) + 0.5
    lvl2y <- 2*radunit*sin(radPoslvl2) + 0.5
    # now plot everything
    for (ii in 1:length(wgtTbl)) {
      grid.lines(x = c(0.5, c(lvl1x,lvl2x)[ii]), y = c(0.5, c(lvl1y,lvl2y)[ii]),
                 gp = gpar(lwd = 1+wgtTbl[ii]/max(wgtTbl)*6,
                           col = adjustcolor(cols[ii]), alpha.f = 0.8))
    }
    grid.circle(x = c(lvl1x, lvl2x), y = c(lvl1y, lvl2y), r = 0.01, 
                gp = gpar(col = adjustcolor(cols, alpha.f = 0.8),
                          fill = adjustcolor(cols, alpha.f = 0.8)))
    # place the central node
    grid.circle(x = 0.5, y = 0.5, r = 0.01, 
                gp = gpar(col = adjustcolor(col = "firebrick", alpha.f = 1),
                          fill = adjustcolor(col = "firebrick", alpha.f = 1)))
    # label everything
    grid.text(names(wgtTbl), x = c(lvl1x, lvl2x), y = c(lvl1y, lvl2y) - 0.01,
              just = "top", gp = gpar(cex = 0.5))
    # add a title
    grid.text(title, x = 0, y = 0.02, just = "left",
              gp = gpar(face = 2))
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

# modify the spiral network plot function
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
    else cols <- c("firebrick", "steelblue")[statemail + 1]
    # generate a new page
    grid.newpage()
    # define radial units
    radunit <- 0.45/levelNum
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
    grid.text("Red - Not Identifiably .gov", x = 0.98, y = 0.02,
              just = "right", gp = gpar(face = 2))
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

### Spiral Network Plot Double-Sided Slider #############################################
# extract state mail information
allnames <- levels(as.factor(c(levels(ClintonCom$To.name),
                               levels(ClintonCom$From.name))))
stateMail <- rep(0, length(allnames))
names(stateMail) <- allnames
for (name in allnames) {
  Mail <- filter(ClintonCom, To.name == name | From.name == name)
  state <- any(grepl(".gov", c(as.character(Mail$To.Add), as.character(Mail$To.name),
                               as.character(Mail$From.name), as.character(Mail$From.Add))))
  stateMail[which(allnames == name)] <- as.numeric(state)
}
# this is my double sided slider hack
nshown <- 20
dev.new()
t2 <- tktoplevel()
sel1 <- min(Days)
sel2 <- max(Days)
SliderValue1 <- tclVar(sel1)
SliderValue2 <- tclVar(sel2)
slider <- tkscale(t2, from = min(Days), to = sel2, resolution = 1, 
                  showvalue = TRUE, orient = "horizontal")
slider2 <- tkscale(t2, from = sel1, to = max(Days), resolution = 1,
                   showvalue = TRUE, orient = "horizontal")
RelEmDat <- filter(ClintonCom, Date >= sel1 & Date <= sel2)
RelEmNodes <- sort(table(c(as.character(RelEmDat$To.name), 
                           as.character(RelEmDat$From.name))),
                   decreasing = TRUE)[-1]
spiralNetPlot(wgtTbl = RelEmNodes[1:min(nshown, length(RelEmNodes))])
update_plot1 <- function(slider1) {
  sVal2 <- as.numeric(tclvalue(SliderValue2))
  sVal1 <- as.numeric(tclvalue(SliderValue1))
  tkconfigure(slider1, to = sVal2)
  dat <- filter(ClintonCom, Date >= sVal1 & Date <= sVal2)
  tab <- sort(table(c(as.character(dat$To.name), 
                      as.character(dat$From.name))),
              decreasing = TRUE)[-1]
  spiralNetPlot(wgtTbl = tab[1:nshown], 
                title = paste(dates(sVal1), dates(sVal2), sep = " - "))
}
update_plot2 <- function(slider2) {
  sVal1 <- as.numeric(tclvalue(SliderValue1))
  sVal2 <- as.numeric(tclvalue(SliderValue2))
  tkconfigure(slider2, from = sVal1)
  dat <- filter(ClintonCom, Date >= sVal1 & Date <= sVal2)
  tab <- sort(table(c(as.character(dat$To.name), 
                      as.character(dat$From.name))),
              decreasing = TRUE)[-1]
  spiralNetPlot(wgtTbl = tab[1:nshown],
                title = paste(dates(sVal1), dates(sVal2), sep = " - "))
}
tkconfigure(slider, variable = SliderValue1, 
            command = function(...) update_plot2(slider2))
tkconfigure(slider2, variable = SliderValue2,
            command = function(...) update_plot1(slider))
tkpack(slider, fill = "x", side = "bottom")
tkpack(slider2, fill = "x", side = "bottom")

### Next Step: Double Sided Slider with Foreign Trips and Thompson Timeline #############
# convert the foreign trip schedule into more useful forms
awaydates <- unlist(lapply(1:239, 
                           function(n) seq(Schedule$StartDate[n],
                                           Schedule$EndDate[n],
                                           by = "day")))
AsSec$Away <- floor(AsSec$Date) %in% awaydates

# process the days to get keywords
AsSecKeyword <- floor(AsSec$Date)
AsSecKeyword <- sapply(AsSecKeyword, 
                       function(dy) paste(AsSec[floor(AsSec$Date) == dy, c("Content")],
                                          collapse = " "))
AsSecKeyword <- lapply(AsSecKeyword, get_keywords)

# this is my double sided slider hack
nshown <- 20
dev.new()
dev.new()
dev.new()
spiralPlot <- dev.list()[3]
tfisf <- dev.list()[4]
countPlot <- dev.list()[5]
t2 <- tktoplevel()
sel1 <- min(Days)
sel2 <- max(Days)
SliderValue1 <- tclVar(sel1)
SliderValue2 <- tclVar(sel2)
slider <- tkscale(t2, from = min(Days), to = sel2, resolution = 1, 
                  showvalue = TRUE, orient = "horizontal")
slider2 <- tkscale(t2, from = sel1, to = max(Days), resolution = 1,
                   showvalue = TRUE, orient = "horizontal")
RelEmDat <- filter(ClintonCom, Date >= sel1 & Date <= sel2)
RelEmNodes <- sort(table(c(as.character(RelEmDat$To.name), 
                           as.character(RelEmDat$From.name))),
                   decreasing = TRUE)[-1]
dev.set(which = spiralPlot)
spiralNetPlot(wgtTbl = RelEmNodes[1:nshown])
dev.set(which = countPlot)
plot(x = ASDays[ASDays >= sel1 & ASDats <= sel2], 
     y = AScounts[ASDays >= sel1 & ASDats <= sel2])
dev.set(which = tfisf)
tsisf <- get_keywords(RelEmDat$content)
update_plot1 <- function(slider1) {
  sVal2 <- as.numeric(tclvalue(SliderValue2))
  sVal1 <- as.numeric(tclvalue(SliderValue1))
  tkconfigure(slider1, to = sVal2)
  dat <- filter(ClintonCom, Date >= sVal1 & Date <= sVal2)
  tab <- sort(table(c(as.character(dat$To.name), 
                      as.character(dat$From.name))),
              decreasing = TRUE)[-1]
  spiralNetPlot(wgtTbl = tab[1:nshown], 
                title = paste(dates(sVal1), dates(sVal2), sep = " - "))
}
update_plot2 <- function(slider2) {
  sVal1 <- as.numeric(tclvalue(SliderValue1))
  sVal2 <- as.numeric(tclvalue(SliderValue2))
  tkconfigure(slider2, from = sVal1)
  dat <- filter(ClintonCom, Date >= sVal1 & Date <= sVal2)
  tab <- sort(table(c(as.character(dat$To.name), 
                      as.character(dat$From.name))),
              decreasing = TRUE)[-1]
  spiralNetPlot(wgtTbl = tab[1:nshown],
                title = paste(dates(sVal1), dates(sVal2), sep = " - "))
}
tkconfigure(slider, variable = SliderValue1, 
            command = function(...) update_plot2(slider2))
tkconfigure(slider2, variable = SliderValue2,
            command = function(...) update_plot1(slider))
tkpack(slider, fill = "x", side = "bottom")
tkpack(slider2, fill = "x", side = "bottom")