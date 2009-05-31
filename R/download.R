library(foreign)
source("utils.R")
try(source("mergeApprox.R"))

## figure out how to automatically download this file
f <- "dep_lista.asp.html"
time.download <- Sys.time() ## will have to change this if downloading manually

ll <- readLines(f,encoding="latin1")
t0 <- grep("Mais Detalhes",ll)
ids <- gsub(".*id=(.*)\".*","\\1",ll[t0])
names <- gsub("\t| *$","",ll[t0-5])
uf <- gsub(".*/([A-Z]+).*","\\1",ll[grep("Partido/UF",ll)])
out <- substr(names,1,1)=="<"
names[out] <- gsub(" +$","",gsub(".*<b>(.*)</b>.*","\\1",ll[t0[out]-4]))
data.legis <- data.frame(idcamara=ids,name=names,inoffice=!out,time=time.download,uf=uf)
##can collect more data from the individual webpages
head(data.legis)

download.file("http://www.camara.gov.br/internet/votacaodbf/53Terceira/votcam09.dbf","../tmp/here.dbf",mode="wb")

txt <- read.dbf("../tmp/here.dbf")
decode <- function(x) data.frame(lapply(x,
                                        function(z) {
                                          if (is.character(z)|is.factor(z)){
                                            iconv(z,from="cp850",to="")
                                          } else {
                                            z
                                          }
                                        }))

txtd <- decode(txt)
txtd$NUMVOT <- pad0(as.numeric(as.character(txt$NUMVOT)),4)

getx <- function(x) {
  fname <- paste("CD09",x,".dbf",sep="")
  download.file(paste("http://www.camara.gov.br/internet/votacaodbf/53Terceira/",fname,sep=""),"../tmp/file.dbf",mode="wb")
  dnow <- read.dbf("../tmp/file.dbf")
  dnow$file <- fname
  dnow$file.name <- x
  dnow
}

d.all <- lapply(txt$NUMVOT,getx)

dnow <- do.call(rbind,d.all)
names(dnow) <- tolower(names(dnow))
dnow$name <- normalize(dnow$nome_par)
dnow$uf <- dnow$estado
levels(dnow$uf) <- state.l2a(levels(dnow$uf))

d.rolls <- with(dnow,unique(data.frame(name=name,uf=uf)))
d.rolls$id <- 1:nrow(d.rolls)
d.legis <- subset(data.legis,select=c(uf,name,idcamara))

res <- merge.approx(states,d.rolls,d.legis,"uf","name")
##match the remaining (duplicates)
d.rolls.tmp <- subset(d.rolls,!id%in%res$x.ind)
res2 <- data.frame()
while (nrow(d.rolls.tmp)>0) {
  d.rolls.tmp$id2 <- 1:nrow(d.rolls.tmp)
  print("************************")
  res2 <- rbind(res2,merge.approx(states,d.rolls.tmp,d.legis,"uf","name"))
  d.rolls.tmp <- subset(d.rolls.tmp,!id2%in%res2$x.ind)
}

rm(d.rolls.tmp)
res <- subset(res,select=-c(x.ind,y.ind))
res2 <- subset(res2,select=-c(x.ind,y.ind,id2))
res <- rbind(res,res2)

tmp1 <- merge(dnow,subset(res,select=-c(name.1,id)),by.x=c("name","uf"),by.y=c("name","uf"))






       


       
