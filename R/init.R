totest <- FALSE

try(setwd("/home/leoniedu/reps/Brazilian-Roll-Calls/R"))
source("functionsRC.R")

library(RMySQL)
## create  db if it does not exist
driver<-dbDriver("MySQL")
if (exists("connect")) dbDisconnect(connect)
connect<-dbConnect(driver,  username="monte",password="e123456",dbname="congressoaberto",host="mysql.cluelessresearch.com")

## begin by creating legis db
##remove db
## create db

try(dbRemoveTable(connect,"votos"))
try(dbRemoveTable(connect,"votacoes"))


## try(dbRemoveTable(connect,"legisladores"))
## source("createLegis.R",echo=TRUE)


Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

##legislaturas
for (i in 1:3) {
  ##sessoes
  for (j in 1:4) {
    print("where are we?")
    print(i);print(j)
    year.now <- substr((1999-51*4)+sessions[i]*4+j-1,3,4)
    session.now <- paste(sessions[i],rounds[j],sep="")
    rdir <- paste("www.camara.gov.br/internet/votacaodbf/",session.now,"/",sep="")
    fname <- paste(rdir,"votcam",year.now,".dbf",sep="")
    ## current dbf
    dbfname <- paste("../data/",fname,sep="")
    ## wget new file
    system(paste("wget ",fname," -Nr -P ../data",sep=""))
    if (file.exists(dbfname)) {
      dbf <- read.dbf(dbfname)
      download.now <- TRUE
      if (totest) {
        download.now <- TRUE
        dbf <- dbf[1:min(15,nrow(dbf)),]
        file.update <- TRUE
      }
      source("download.R",echo=FALSE)
    }
  }
}

    
      
