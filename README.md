# The Interactive Filter and Display of Hillary Clinton's Emails
This repository contains all of the code files required to extract,
process, and display the emails sent over Hillary Clinton's private
email server during her tenure as Secretary of State of the United
States. An analysis facilitated by these code files written by Chris
Salahub and Wayne Oldford can be found in the article "About 'her
emails'" in the June 2018 issue of Significance and an online version
of the shiny application developed for public use can be found at
[https://shiny.math.uwaterloo.ca/sas/clinton/](https://shiny.math.uwaterloo.ca/sas/clinton/).

A brief explanation of the files found in this repository follows.

## VAST_submission
This folder contains the LaTeX files used to generate a working paper
used as the template for later journal submissions. A compiled PDF
version of this folder can be seen on
[ResearchGate](https://www.researchgate.net/publication/315876309_Interactive_Filter_and_Display_of_Hillary_Clinton's_Emails_A_Cautionary_Tale_of_Metadata).

## EmailFunctions.R
This file contains the code used to extract and process the email data
set. All of the functions are defined here as well as the script which
calls those functions.

## FullAppData.Rds
The Rds version of the data, processed into a list structure which
contains all necessary data sources for the interactive shiny
application. This file is an output of the EmailFunctions.R script.

## TestScript.R
Code for performing a randomization test of the gap found in the data.

## app.R
As it sounds, this contains the code to display the data in a shiny app in R.
