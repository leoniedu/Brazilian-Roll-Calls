i <- 3
j <- 3
sessions <- 51:53
rounds <- c("Primeira","Segunda","Terceira","Quarta")

year.now <- substr((1999-51*4)+sessions[i]*4+j-1,3,4)
session.now <- paste(sessions[i],rounds[j],sep="")

rdir <- paste("www.camara.gov.br/internet/votacaodbf/",session.now,"/",sep="")
fname <- paste(rdir,"votcam",year.now,".dbf",sep="")

dbf.old <- file.info(paste("../data/",fname,sep=""))
system(paste("wget ",fname," -Nr -P ../data",sep=""))
##file updated?
dbf.new <- file.info(paste("../data/",fname,sep=""))
file.updated <- all.equal(dbf.old[,c("size","mtime")],dbf.new[,c("size","mtime")])

if (!file.updated) {
  download.now <- TRUE
  source("download.R")
  library(R2HTML)
  Sweave("report.Rnw")
}
