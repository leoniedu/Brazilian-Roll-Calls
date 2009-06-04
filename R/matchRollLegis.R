library(foreign)
library(RColorBrewer)
library(ggplot2)
library(GDD)
library(reshape)
source("utils.R")

##file with the current legislators
##http://www.camara.gov.br/internet/deputado/deputado.xls
## may be a send an email link?


try(source("mergeApprox.R"))
try(source("spatial.R"))

rounds <- c("Primeira","Segunda","Terceira","Quarta")
## 51:53
sessions <- 51:53
session.now <- paste(sessions[i],rounds[j],sep="")
year.now <- substr((1999-51*4)+sessions[i]*4+j-1,3,4)

## figure out how to automatically download this file (currently the 53rd)
f <- paste("../data/dep_lista.asp",sessions[i],".html",sep="")
time.download <- Sys.time() ## will have to change this if downloading manually

ll <- readLines(f,encoding="latin1")
t0 <- grep("Mais Detalhes",ll)
ids <- gsub(".*id=(.*)\".*","\\1",ll[t0])
names <- gsub("\t| *$","",ll[t0-5])
uf <- gsub(".*/([A-Z]+).*","\\1",ll[grep("Partido/UF",ll)])
out <- substr(names,1,1)=="<"
names[out] <- gsub(" +$","",gsub(".*<b>(.*)</b>.*","\\1",ll[t0[out]-4]))
data.legis <- data.frame(idcamara=ids,name=names,inoffice=!out,time=time.download,uf=uf)
save(data.legis,file=paste("../data/data.legis",session.now,".RData",sep=""))
