library(RCurl)
library(XML)
library(tm)
library(stringr)
library(SnowballC)
library(chron)
library(dplyr)

newRaw <- readRDS('fullRawMail.rds')
NewPull <- readRDS('WorkEmailPull.rds')
EmailData <- read.csv('ClintonEmailData.csv')

infoExtractor2 <- function(emails, ids, includeRaw = FALSE) {
    ## first identify the interesting information
    Cleaned1 <- str_extract(emails, '(?<=(div class=\"tab-pane fade in active\" id=\"content\">))(?s).+(?-s)(?=(<div class=\"tab-pane fade in\" id=\"source\">))')
    ## replace HTML tags
    Cleaned <- str_replace_all(Cleaned1, "<[^><]+>", "|")
    ## now clean up some strange text formatting
    Cleaned <- str_replace_all(Cleaned, "&lt.", "\\(")
    Cleaned <- str_replace_all(Cleaned, "&gt.", "\\)")
    Cleaned <- str_replace_all(Cleaned, "&quot.", "")
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
    PDFDate2[as.numeric(format(PDFDate2, "%Y%")) < 2009] <- NA
    ## put these two together
    PDFDateObj[is.na(PDFDateObj)] <- PDFDate2
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
                       Subject = WikiSubject, Date = WikileaksDate,
                       Day = days(WikileaksDate),
                       Month = months(WikileaksDate),
                       Year = years(WikileaksDate),
                       Weekday = weekdays(WikileaksDate),
                       Hour = hours(WikileaksDate),
                       Minutes = minutes(WikileaksDate),
                       PDFDate = PDFDateObj, PDFDay = days(PDFDateObj),
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
