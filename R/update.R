try(setwd("/home/leoniedu/reps/Brazilian-Roll-Calls/R"))
source("functionsRC.R")

update.all <- TRUE

Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
i <- 3
j <- 3

year.now <- substr((1999-51*4)+sessions[i]*4+j-1,3,4)
session.now <- paste(sessions[i],rounds[j],sep="")

rdir <- paste("www.camara.gov.br/internet/votacaodbf/",session.now,"/",sep="")
fname <- paste(rdir,"votcam",year.now,".dbf",sep="")



## current dbf
dbf.old <- read.dbf(paste("../data/",fname,sep=""))[-c(3:9),]
## wget new file
system(paste("wget ",fname," -Nr -P ../data",sep=""))
## new pdf
dbf.new <- read.dbf(paste("../data/",fname,sep=""))
if (!update.all)   dbf.new <- dbf.new[!with(dbf.new,(NUMVOT%in%dbf.old$NUMVOT)&(NUMVOT%in%dbf.old$NUMVOT)),]

download.now <- FALSE
##file updated?
file.updated <- (nrow(dbf.new)>0)|update.all
if (file.updated) {
  dbf <- dbf.new
  download.now <- TRUE
  source("matchRollLegis.R")
  source("download.R")
  library(R2HTML)
  Sweave("report.Rnw",driver=RweaveHTML())
}
