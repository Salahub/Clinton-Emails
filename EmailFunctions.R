# load required packages
library(RCurl)
library(XML)
library(tm)
library(stringr)
library(SnowballC)
library(chron)
library(dplyr)

# define the current email totals
mailMax <- 33727

# load the raw HTML data if desired
# fullmail <- readRDS('fullRawMail.Rds')

### Email Extraction Functions #########################################################
# generate a function to clean up emails
infoExtractor <- function(emails, ids, includeRaw = FALSE) {
    ## first identify the interesting information
    Cleaned1 <- str_extract(emails, '(?<=(div class=\"tab-pane fade in active\" id=\"content\">))(?s).+(?-s)(?=(<div class=\"tab-pane fade in\" id=\"source\">))')
    ## now clean up some strange text formatting
    Cleaned1 <- str_replace_all(Cleaned1, "&lt.", "\\(")
    Cleaned1 <- str_replace_all(Cleaned1, "&gt.", "\\)")
    Cleaned1 <- str_replace_all(Cleaned1, "&quot.", "")
    ## replace HTML tags
    Cleaned <- str_replace_all(Cleaned1, "<[^><]+>", "|")
    ## clean up extra spaces and anything left after the above replacements
    Cleaned <- str_replace_all(Cleaned, "[\\s]+", " ")
    Cleaned <- str_replace_all(Cleaned, "\\| \\|", "|")
    ## now get to actually extracting data
    ## start with the dates supplied by Wikileaks
    WikileaksDate <- str_extract(Cleaned, "(?<=(Date\\: ))[\\-0-9]+ [\\:0-9]+")
    WikileaksDate <- as.POSIXlt(WikileaksDate, format = "%Y-%m-%d %H:%M",
                                tz = "GMT")
    ## next try to extract the dates as recorded in the PDF documents
    PDFDate <- str_extract(Cleaned, "(?<=([A-Za-z]{1,9}[,]?[\\s]?))[A-Za-z]+[\\s]?[0-9]+[,]?[\\s]?[0-9]+[\\s]?[0-9]{1,2}\\:[0-9]{1,2}[\\s]?[APM]{2}")
    ## regularize formatting
    PDFDate <- str_replace(PDFDate, "(?<=([0-9]{4}))(?=([0-9]{1,2}\\:))", " ")
    PDFDate <- str_replace_all(PDFDate, "(?<=([a-z\\,]))(?=([0-9]))", " ")
    PDFDate <- str_replace_all(PDFDate, "(?<=(\\:[0-9]{2}))(?=[PA])", " ")
    PDFDate <- str_replace_all(PDFDate, "(?<=([0-9]{4}))(?=[\\:])", " 0")
    PDFDate <- str_replace_all(PDFDate, "(?<=([A-Za-z]{1,9} [0-9]{1,2}))[^,](?=([0-9]+))", ", ")
    ## create a new object for the date objects
    PDFDateObj <- as.POSIXlt(PDFDate, format = "%B %d, %Y %I:%M %p",
                             tz = "GMT")
    ## use this to create a flag
    PDFDateFlag <- is.na(PDFDateObj)|is.na(PDFDate)
    ## try to extract more date information from these based on other formats
    PDFDate2 <- str_extract(Cleaned[is.na(PDFDateObj)], "[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}[\\s]?[0-9]{1,2}:[0-9]{2}:[0-9]{2} [APMl/]{2,5}")
    PDFDate2 <- str_replace(PDFDate2, "ll/l", "M")
    PDFDate2 <- as.POSIXlt(PDFDate2, format = "%m/%e/%Y %I:%M:%S %p")
    ## put these two together
    PDFDateObj[is.na(PDFDateObj)] <- PDFDate2
    PDFDateObj[as.numeric(format(PDFDateObj, "%Y")) < 2009 | as.numeric(format(PDFDateObj, "%Y")) > 2014] <- NA
    ## extract the subject as provided by Wikileaks
    WikiSubject <- str_extract(Cleaned,"(?<=(Subject:[\\s]?))[^\\|]+(?=\\|)")
    ## try to extract the subjects from the emails directly
    EmailSubjects <- str_extract_all(Cleaned,"(?<=(Subject:[\\s]?))[^\\|]+(?=\\|)")
    EmailSubjects <- lapply(EmailSubjects, function(vec) paste(vec, collapse = "|"))
    ## extract the wikileaks to and from information
    WikiFrom <- str_extract(Cleaned, "(?<=(From: \\|))[^\\|]+(?=\\|)")
    WikiAddFrom <- str_extract(Cleaned1, "(?<=(From\\: <span title=\"Original: ))[^\"]+(?=\")")
    WikiTo <- str_extract(Cleaned, "(?<=(To: \\|))[^\\|]+(?=\\|)")
    WikiAddTo <- str_extract(Cleaned1, "(?<=(To\\: <span title=\"Original: ))[^\"]+(?=\")")
    ## extract all the to from information
    AllToFrom <- str_extract_all(Cleaned, "(?<=(From:[\\s]?[\\|]?))[^\\|]+(?=\\|)|(?<=(To:[\\s]?[\\|]?))[^\\|]+(?=\\|)")
    AllToFrom <- lapply(AllToFrom, function(vec) paste(vec, collapse = "|"))
    ## identify whether the email is redacted
    Redacted <- grepl("RELEASE\\s*IN\\s*PART", Cleaned)
    ## identify the email redaction codes used
    Clevel <- str_extract_all(Cleaned, "B[1-9](?![0-9a-zA-Z])")
    Clevel <- unlist(lapply(Clevel, function(el) {
        if(length(el)==0) "None"
        else paste(sort(unique(el)), collapse = "-")}))
    ## extract the email content
    Content <- str_split(Cleaned, "\\|")
    Content <- lapply(Content, function(vec) {
        temp <- vec[9:length(vec)]
        paste(temp[!grepl("To:|From:|Subject:|Date:|UNCLASSIFIED U.S.|RELEASE|MSG_|Original Message|Attachments:", temp)], collapse = " ")
    })
    ## return all the data after placing it into a storage structure
    Data <- data.frame(ID = ids, To.name = WikiTo, From.name = WikiFrom,
                       To.Add = WikiAddTo, From.Add = WikiAddFrom,
                       Subject = WikiSubject,
                       Date = as.numeric(as.chron(WikileaksDate)),
                       Day = days(WikileaksDate),
                       Month = months(WikileaksDate),
                       Year = years(WikileaksDate),
                       Weekday = weekdays(WikileaksDate),
                       Hour = hours(WikileaksDate),
                       Minutes = minutes(WikileaksDate),
                       PDFDate = as.numeric(as.chron(PDFDateObj)),
                       PDFDay = days(PDFDateObj),
                       PDFMonth = months(PDFDateObj),
                       PDFYear = years(PDFDateObj),
                       PDFWeekday = weekdays(PDFDateObj),
                       PDFHour = hours(PDFDateObj),
                       PDFMinutes = minutes(PDFDateObj),
                       Content = unlist(Content),
                       AllToFrom = unlist(AllToFrom),
                       Classification = Clevel,
                       Redacted = Redacted, B1 = grepl("B1", Clevel),
                       B2 = grepl("B2", Clevel), B3 = grepl("B3", Clevel),
                       B4 = grepl("B4", Clevel), B5 = grepl("B5", Clevel),
                       B6 = grepl("B6", Clevel), B7 = grepl("B7", Clevel),
                       B8 = grepl("B8", Clevel), B9 = grepl("B9", Clevel),
                       None = grepl("None", Clevel),
                       PDFSubjects = as.character(unlist(EmailSubjects)),
                       CleanedEmails = as.character(Cleaned))
}

# define a function which accepts some basic user input and pulls the
# corresponding emails, the max was 30322, but addition of more increased it to 32795, then 33727
get_Clin_emails <- function(ids = 1:mailMax, # give possible emails for selection
                            size = length(ids), # size of selection
                            random = FALSE, # determine if selection is random
                            replaceOutput = TRUE, # determine if old output is kept
                            contAtErr = NULL, # provide the option of specifying default
                                              # error handling procedure
                            includeRaw = FALSE, # pass along argument to specify if
                                                # raw html data is to be included
                            output = "ClintonEmailData.csv"
)
{
  # check if output has already been generated
  output.exist <- file.exists(output)
  # if so extract the ids which are included
  if (output.exist) {
    prevPull <- read.csv(output)
    # now, if past output is to be rewritten, do not select emails with ID values
    if (replaceOutput) prevPull <- prevPull[!(prevPull$ID %in% ids),]
    else {
      ids <- ids[!(ids %in% prevPull$ID)]
      # it is possible that we have already extracted all IDs provided
      if (length(ids) == 0) return("Given IDs already extracted")
    }
  } else prevPull <- NULL
  # check if all IDs provided exist
  if (!all(ids %in% 1:mailMax)) {
    # give a warning
    warning("IDs must be integers between 1 and 32795: continuing only with valid IDs")
    ids <- ids[ids %in% 1:mailMax] # select the valid ids
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
      if(idx %% 10 == 0) message(paste(round(idx/N*100, digits = 3), '% completed at', Sys.time()))
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
  # now clean this data using a regex helper with error handling
  newMailData <- tryCatch(infoExtractor(emails, ids = ids[!failures], includeRaw),
                            error = function(err) return(paste("Email Processing Error:", err)))
  # combine this with the previous email data, making sure dimension matches
  if ((is.null(prevPull$rawHead)|is.null(prevPull$rawCont)) & includeRaw & output.exist) {
    # if the dimension doesn't, add columns appropriately
    prevPull <- data.frame(prevPull, rawHead = rep("", dim(prevPull)[1]),
                           rawCont = rep("", dim(prevPull)[1]))
  }
  # incude more error handling conditionals
  if (length(newMailData) != 1) {
      # if ther are no errors save and return the data
      FullData <- rbind(newMailData, prevPull)
      # save this
      write.csv(FullData, output, row.names = FALSE)
      # return this
      return(FullData)
  }
  else {
      # if an error has occurred, display it and return the raw emails
      message(newMailData)
      return(emails)
      }
}

# the function call
get_Clin_emails(contAtErr = TRUE, output = "Data_19052019.csv")

### Pre-loaded data #####################################################################
#EmailData <- read.csv('ClintonEmailData.csv')
EmailData <- read.csv("Data_19052019.csv")

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

# first arrange the emails sequentially
AsSec <- arrange(AsSec, Date)
# we can also look at the emails by day of the year and calendar date
# first use 'tally' and 'group_by' in dplyr to extract counts
countbyDate <- tally(group_by(AsSec, dates(as.chron(Date))))
ASDays <- seq(from = min(countbyDate$`dates(as.chron(Date))`),
              to = max(countbyDate$`dates(as.chron(Date))`),
              by = 'days')
AScounts <- rep(0, length(ASDays))
AScounts[ASDays %in% countbyDate$`dates(as.chron(Date))`] <- countbyDate$n

# re-sort by ID
AsSec <- arrange(AsSec, ID)

### Keyword Analysis ####################################################################
ExtraWords <- c("will", "state", "pm", "said", "secretari", "can", "call",
                "depart", "time", "presid", "govern", "offic", "work", "usa", "also", "meet",
                "new", tolower(LETTERS))
# define a function which can be used to clean and process email content
get_keywords <- function(Content, stem = TRUE, table = TRUE) {
  # first combine all the emails into one string
  corpus <- paste(Content, collapse = " ")
  # make everything lowercase for reference
  corpus <- tolower(corpus)
  # start by removing a number of problematic terms
  corpus <- str_replace_all(corpus, "\\&lt;|\\&gt;|</span>|</div>", " ")
  corpus <- str_replace_all(corpus, "unclassified [a-z0-9][.]*[a-z0-9][.]* department of state case no[.]* [a-z0-9]+ doc no[.]* [a-z0-9]+ date: [0-9/]+ | unclassified [a-z0-9][.]*[a-z0-9][.]* department of state case no[.]* [a-z0-9\\-]+ doc no[.]* [a-z0-9]+",
                            " ")
  corpus <- str_replace_all(corpus, "release\\s*in\\s*part|release\\s*in\\s*full", " ")
  corpus <- str_replace_all(corpus, "u.s.|u.s.a.", "usa")
  # remove all non-alphanumeric characters
  # first remove the contractions and commas used in number display
  corpus <- str_replace_all(corpus, "[\\'\\.]", "")
  corpus <- str_replace_all(corpus, "(?<=[0-9])\\,(?=[0-9])", "")
  # now safely remove everything else
  corpus <- str_replace_all(corpus, "[^0-9a-zA-Z\\s]", " ")
  corpus <- str_replace_all(corpus, "[0-9]", "")
  # replace all duplicate spaces
  corpus <- str_replace_all(corpus, "\\s+", " ")
  # split by singular spaces
  corpus <- unlist(str_split(corpus, " "))
  # remove common stopwords
  Cust_stop <- c(stopwords(), stopwords(kind = "SMART"), ExtraWords)
  corpus <- removeWords(corpus, Cust_stop)
  # remove empty strings which may have occurred
  corpus <- corpus[corpus != ""]
  # stem the remaining terms
  if (stem) {
    corpus <- removeWords(stemDocument(corpus), Cust_stop)
    corpus <- corpus[corpus != ""]
  }
  # now to extract the unique words used
  if (table) corpus <- sort(table(corpus), decreasing = TRUE)
  corpus
}

# run this on the main data
keywords <- get_keywords(AsSec$Content)
# set the cutoff
cutoff <- 150
# select arbitrary relevant keywords
relKey <- names(keywords)[keywords > cutoff]
# generate the term document matrix
ASmat <- lapply(lapply(AsSec$Content, function(e) get_keywords(e, table = FALSE)),
                function(email) sapply(relKey, function(kw) sum(kw == email)))
ASmat <- matrix(unlist(ASmat), ncol = length(relKey), byrow = TRUE)
# load the term-document matrix
# ASmat <- read.csv('TermDocumentMatrix.csv')
# generate a tfidf matrix
tfIdf <- apply(ASmat, 2, function(col) col/max(col) * log(length(col)/sum(col != 0)))
ASmat <- cbind(AsSec$ID, AsSec$Date,
               AsSec[,c("B1","B2","B3","B4","B5","B6","B7","B8","B9","None")],
               ASmat)
tfIdf <- cbind(AsSec$ID, AsSec$Date,
               AsSec[,c("B1","B2","B3","B4","B5","B6","B7","B8","B9","None")],
               tfIdf)
colnames(tfIdf) <- c("ID","Date","B1","B2","B3","B4","B5","B6","B7","B8","B9","None",
                     relKey)
colnames(ASmat) <- c("ID","Date","B1","B2","B3","B4","B5","B6","B7","B8","B9","None",
                     relKey)

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

### Spiral Network Data ############################################################################
# first filter out those communications which involve Clinton
ClintonCom <- filter(EmailData,
                     From.name == "Hillary Clinton" | To.name == "Hillary Clinton" |
                       From.name == "H" | To.name == "H")
# extract the names of all addresses that communicated with Clinton
ClintNet <- sort(table(c(as.character(ClintonCom$To.name),
                         as.character(ClintonCom$From.name))),
                 decreasing = TRUE)[-1]

# extract state mail information
allnames <- levels(as.factor(c(levels(ClintonCom$To.name),
                               levels(ClintonCom$From.name))))
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

### App Data List Definition ############################################################
AppData <- list(AsSec = arrange(AsSec, Date), Freq = ASmat, TfIdf = tfIdf,
                ForSched = Schedule, countbyDate = countbyDate,
                ASDays = ASDays, AScounts = AScounts, ClintonCom = ClintonCom,
                ClintNet = ClintNet, stateMail = stateMail)
saveRDS(AppData, "FullAppData.Rds")
