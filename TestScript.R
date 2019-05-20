## simple script to run the desired test

## function to calculate the analytical test
prun <- function(n, N = 1489, prob = 71/1489) (1 - pbinom(n, N, prob))*(factorial(n)/prod(seq(N-n+2, N, by = 1)))

## a small zero run helper
zerohelp <- function(sequ) {
    ## establish longest current run, and the current run length
    longest <- 0
    currun <- 0
    ## start iterating through the sequence
    for (val in sequ) {
        ## check value
        if (val == 0) currun <- currun + 1
        ## the value is otherwise not zero
        else if (currun > longest) {
            longest <- currun
            currun <- 0
        }
        ## finally set zero if nothing else is true
        else currun <- 0
    }
    ## return the longest
    longest
}

## now a way to simulate it
prunSim <- function(nruns, N = 1489, pr = 71/1489) {
    ## use this to sample and determine the longest run nrun times
    rundist <- sapply(1:nruns, function(ind) zerohelp(sample(c(0,1), size = N, replace = TRUE, prob = c(pr, 1-pr))))
    ## return the distribution
    rundist
}

## this is a somewhat parametric solution to the problem, instead focus on the emails themselves and try a
## pseudo-sampling approach
emailDate <- read.csv("ClintonEmailData.csv")[,c("ID","To.Add","From.Add","Date","PDFDate")]
## truncate the dates to the day level
emailDate$Date <- floor(emailDate$Date)
emailDate$PDFDate <- floor(emailDate$PDFDate)
## remove the NA emails
emailDate <- emailDate[!is.na(emailDate$PDFDate),]

## get the unique dates
unqDates <- unique(emailDate$PDFDate)
## get the date ranks
dateRank <- match(emailDate$PDFDate, sort(unqDates))
## now apply consecutive dates to these ranks
emailDate$AdjDate <- seq(min(emailDate$PDFDate), by = 1, length.out = length(unqDates))[dateRank]

## duplicate the data
emailDate <- rbind(emailDate, emailDate)

## write a function to perform n half samples of the emails and look for gaps
halfSample <- function(n) {
    ## get the dates
    dts <- emailDate$AdjDate
    ndts <- ceiling(length(dts)/2)
    ## get the date range
    dtrng <- seq(min(dts), max(dts))
    ## simple sapply function call to perform this
    zeroruns <- sapply(1:n, function(n) zerohelp(as.numeric(dtrng %in% sample(dts, size = ndts))))
    ## return this
    zeroruns
}

## use both of these to generate estimates of likelihood
set.seed(19052019)
HalfSamp <- halfSample(1e6)
simpSim <- prunSim(1e6)


## make a dual histogram
sampHist <- hist(HalfSamp, breaks = seq(-0.5,6.5, by = 1))
simHist <- hist(simpSim, breaks = seq(-0.5,6.5, by = 1))
plot(NA, xlim = max(c(sampHist$counts, simHist$counts))*c(-1,1), ylim = range(sampHist$breaks),
     xlab = expression(paste("Frequency (", x, 10^5, ")")), xaxt = "n", ylab = "Longest Gap")
axis(1, at = seq(-7.5e5, 7.5e5, by = 2.5e5), labels = abs(seq(-7.5, 7.5, by = 2.5)))
with(sampHist, rect(-counts, breaks[1:7], 0, breaks[2:8], col = adjustcolor("firebrick", alpha.f = 0.7)))
with(simHist, rect(0, breaks[1:7], counts, breaks[2:8], col = adjustcolor("steelblue", alpha.f = 0.7)))
abline(h = 6, lty = 2)
legend(x = "topright", legend = c("Non-parametric", "Parametric", "Observed"),
       col = c(adjustcolor("firebrick", alpha.f = 0.7), adjustcolor("steelblue", alpha.f = 0.7), "black"),
       pch = c(15,15,NA), lty = c(NA,NA,2), bg = "white")
