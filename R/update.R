totest <- TRUE
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
dbfname <- paste("../data/",fname,sep="")
if (file.exists(dbfname)) {
  dbf.old <- read.dbf(dbfname)
}
## wget new file
system(paste("wget ",fname," -Nr -P ../data",sep=""))
## new pdf
dbf <- read.dbf(paste("../data/",fname,sep=""))
if (file.exists(dbfname)) {
  if ((!update.all))   dbf <- dbf[!with(dbf,(NUMVOT%in%dbf.old$NUMVOT)&(NUMVOT%in%dbf.old$NUMVOT)),]
}
download.now <- TRUE
##file updated?
file.updated <- (nrow(dbf)>0)|update.all


if (totest) {
  download.now <- FALSE
  dbf <- dbf[1:min(5,nrow(dbf)),]
  file.update <- TRUE
}

if (file.updated) {
  source("matchRollLegis.R")
  source("download.R")
  library(R2HTML)
  Sweave("report.Rnw",driver=RweaveHTML())
}
