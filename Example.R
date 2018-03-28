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

### Set the working directory
setwd("C:/JLozano/lastfm-analyser-data")

### Sep API key for lastfm, you have to create one at https://www.last.fm/es/api 
api_key="fa88980a5ebb1dc8d3375a0451c1b5fb"

### Download required functions from github

if (!dir.exists("programs")) dir.create("programs")
# https://github.com/ppatrzyk/lastfm-to-R
if (!file.exists("programs/lastfm.R")) download.file("https://raw.githubusercontent.com/lozalojo/lastfm-to-R/master/lastfm.R", "programs/lastfm.R")
source("programs/lastfm.R")
# https://github.com/ppatrzyk/lastfm-to-R
if (!file.exists("programs/lastfm-analyser.R")) download.file("https://raw.githubusercontent.com/lozalojo/lastfm-to-R/master/lastfm-analyser.R", "programs/lastfm-analyser.R")
source("programs/lastfm-analyser.R")



profile.names.list<-list(
  list(name="@corpcd", lastfm.users="jelozano"),
  list(name="@Kivenkantaja", lastfm.users="l_mou_l"),
  list(name="@Rider of storm", lastfm.users=c("rider_of_Storm","seitan_mb")),
  list(name="Neutravo", lastfm.users="Neutravo"),
  list(name="truenoacero", lastfm.users="truenoacero"),
  list(name="reyox", lastfm.users="reyox"),
  list(name="@Aristóteles", lastfm.users="WoodenDors"),
  list(name="@mattogrosso", lastfm.users="mattogrosso"),
  list(name="@metalgod", lastfm.users="tuviejapelleja"),
  list(name="@El_Tolo", lastfm.users="el_tolo")
)

restart.tasks=F

source("programs/analyser - functions.R")

# i<-1
# temp1<-lastfm.analiser(i.profile.name=profile.names.list[[i]], i.api_key=api_key, i.restart=restart.tasks)

lastfm.results<-list()
for (i in 1:length(profile.names.list)) lastfm.results[[i]]<-lastfm.analiser(i.profile.name=profile.names.list[[i]], i.api_key=api_key, i.restart=restart.tasks)
