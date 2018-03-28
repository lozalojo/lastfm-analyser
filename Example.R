### Load required libraries
library(extrafont)
loadfonts(device="win")
library(XML)
library(dplyr)
library(urltools)
library(jsonlite)
library(memoise)
library(ggplot2)
library(zoo)
library(tidyr)
# devtools::install_github("hrbrmstr/hrbrmisc")
library(hrbrmisc)
library(viridis)
library(ggrepel)
library(doSNOW)
library(doParallel)
library(lubridate)
library(stringi)

### Set parameters
# List of profiles, a list of lists with names and usernames (can be more than one)
profile.names.list<-list(
  list(name="Jose E. Lozano", lastfm.users="jelozano")
)
# Should the program restart from the begining or continue if the program failed due to timeouts?
restart.tasks=F
# Working directory
wd.dir <- "C:/lastfm-analyser-data"

### Set the working directory
if (!dir.exists(wd.dir)) dir.create(wd.dir)
setwd(wd.dir)

### Sep API key for lastfm, you have to create one at https://www.last.fm/es/api 
api_key="fa88980a5ebb1dc8d3375a0451c1b5fb"

### Download required functions from github
if (!dir.exists("programs")) dir.create("programs")
# https://github.com/lozalojo/lastfm-to-R, forged from: https://github.com/ppatrzyk/lastfm-to-R
if (!file.exists("programs/lastfm.R")) download.file("https://raw.githubusercontent.com/lozalojo/lastfm-to-R/master/lastfm.R", "programs/lastfm.R")
# https://github.com/lozalojo/lastfm-analyser
if (!file.exists("programs/lastfm-analyser.R")) download.file("https://raw.githubusercontent.com/lozalojo/lastfm-analyser/master/lastfm-analyser.R", "programs/lastfm-analyser.R")

### Source functions
source("programs/lastfm.R")
source("programs/lastfm-analyser.R")

lastfm.results<-list()
for (i in 1:length(profile.names.list)) lastfm.results[[i]]<-lastfm.analiser(i.profile.name=profile.names.list[[i]], i.api_key=api_key, i.restart=restart.tasks)
