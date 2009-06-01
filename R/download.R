library(foreign)
library(RColorBrewer)
library(GDD)
library(reshape)
source("utils.R")


download.now <- FALSE

try(source("mergeApprox.R"))
try(source("spatial.R"))

rounds <- c("Primeira","Segunda","Terceira","Quarta")
## 51:53
sessions <- 51:53
i <- 3
j <- 3
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
##can collect more data from the individual webpages
head(data.legis)

rdir <- paste("www.camara.gov.br/internet/votacaodbf/",session.now,"/",sep="")

fname <- paste(rdir,"votcam",year.now,".dbf",sep="")
system(paste("wget ",fname," -Nr -P ../data",sep=""))

dbf <- read.dbf(paste("../data/",fname,sep=""))
names(dbf) <- tolower(names(dbf))

decode <- function(x) data.frame(lapply(x,
                                        function(z) {
                                          if (is.character(z)|is.factor(z)){
                                            iconv(z,from="cp850",to="")
                                          } else {
                                            z
                                          }
                                        }))

dbfd <- decode(dbf)
dbfd$numvot <- pad0(as.numeric(as.character(dbf$numvot)),4)

getx <- function(x) {
  fname <- paste(rdir,"CD",year.now,x,".dbf",sep="")
  system(paste("wget ",fname," -Nr -P ../data",sep=""))
}

readx <- function(x) {
  fname <- paste(rdir,"CD",year.now,x,".dbf",sep="")
  dnow <- try(read.dbf(paste("../data/",fname,sep="")))
  if (!"try-error"%in%class(dnow)) {
    dnow$file <- fname
    dnow$file.name <- x
    dnow
  } else {
    NULL
  }
}

##download
if (download.now) download.results <- lapply(dbf$numvot,getx)

d.all <- lapply(dbf$numvot,readx)

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

res$name.1 <- NULL
tmp1 <- merge(dnow,subset(res,select=-c(id)),by.x=c("name","uf"),by.y=c("name","uf"))
tmp2 <- merge(data.legis,subset(tmp1,select=-c(name,uf)),by="idcamara")
tmp3 <- subset(tmp2,!voto%in%c("PRESENTE","<------>"))
tmp3$n <- with(tmp3,ave(file,file,FUN=length))
tmp3$voto <- factor(tmp3$voto)
tmp3$u <- with(tmp3,ave(voto=="<------->",file,FUN=sum))
##exclude votes with all missing
tmp3 <- subset(tmp3,u!=n)


## create final format

data.votos <- subset(tmp3,select=-c(n,u,threshold,file,nome_par,inoffice,time))
data.votos$voto <- tolower(car::recode(data.votos$voto,"'<------->'='AUSENTE'"))
data.votos$voto <- factor(data.votos$voto,levels=c("sim","nao", "obstrucao","abstencao", "art. 17", "ausente"))
data.votos$numvot <- factor(data.votos$file.name)
data.votos$file.name <- NULL
data.votos <- reshape::rename(data.votos,c(`name`="nome"))


data.votacoes <- merge(subset(dbfd,select=c(numvot,datavot,texordia)),recast(data.votos,numvot~voto,measure.var="idcamara",fun.aggregate=length,margins="grand_col"))
data.votacoes <- reshape::rename(data.votacoes,c(`(all)`="total"))


conc.pt <- recast(subset(data.votos,partido=="PT"),numvot~variable,measure.var="voto",fun.aggregate=rattle::modalvalue)
conc.pt <- reshape::rename(conc.pt, c(voto="votopt"))
data.votos <-merge(data.votos, conc.pt)
data.votos$concpt <- with(data.votos, as.character(votopt)==as.character(voto))

save(data.votos,data.votacoes,data.legis,file=paste('../data/',session.now,'.RData',sep=''))




m1 <- readShape.cent("../data/maps/BRASIL.shp","UF")
## plot maps
nvotnow <- "0045"
for (nvotnow in data.votacoes$numvot) {
  tmp <- recast(subset(data.votos,numvot==nvotnow),uf~variable,measure.var="concpt",fun.aggregate=function(x) c(n=length(x),p=sum(x)/length(x)))
  tmp$UF <- tmp$uf
  m2 <- merge.sp(m1,tmp,by="UF")
  par(bg="grey")
  n1 <- 4
  seqx <- c(0,.15,.3,.45,.55,.70,.85,1)
  ##seqx <- seq(0,1,length=length(col.vec)+1)
  col.vec <- c(rev(brewer.pal(n1,"Blues")[-1]),"white",brewer.pal(n1,"Reds")[-1])
  dn <- subset(data.votacoes,numvot==nvotnow)
  ##png(file=with(dn,paste("../images/",datavot,".",numvot,"small.png",sep="")))
  GDD(file = with(dn,paste("../images/",datavot,".",numvot,"small.png",sep="")), type = "png", width = 480, height = 480, ps = 12, bg = "white")
  par(mai=c(0,0,0,0))
  plot.heat(m2,NULL,"concpt_p",title="Proporção votando\njunto com o PT",breaks=seqx,reverse=FALSE,cex.legend=1,bw=1,col.vec=col.vec,plot.legend=FALSE)
  dev.off()  
  ##png(file=with(dn,paste("../images/",datavot,".",numvot,".png",sep="")))
  GDD(file = with(dn,paste("../images/",datavot,".",numvot,".png",sep="")), type = "png", width = 480, height = 480, ps = 12, bg = "white")
  par(mai=c(0,0,0.6,0))
  plot.heat(m2,NULL,"concpt_p",title="Proporção votando\njunto com o PT",breaks=seqx,reverse=FALSE,cex.legend=1,bw=1,col.vec=col.vec,main=nvotnow)
  with(m2@data,text(x,y,UF,cex=0.8),col="grey30")
  mtext(tolower(paste(dn$texordia,"\n",dn$data)),3, cex=1)
  dev.off()  
}




library(ggplot2)

tmp <- subset(data.votos,numvot=="0099")
tmp$simnao <- car::recode(tmp$voto,"'sim'='sim';else='nao'")
lp <- names(sort(-table(data.votos$partido)))[1:10]
tmp <- subset(tmp,partido%in%lp)
tmp$partido <- reorder(tmp$partido,tmp$partido,FUN=function(x) length(x))
##FIX calculate votopt again
qplot(partido, data=tmp,geom="bar",fill=voto)+ scale_fill_manual(values = c(brewer.pal(9,"Reds")[5],rev(brewer.pal(6,"Blues")[-1])))








## tmp <- recast(subset(data.votos,numvot=="0099"),measure.var=c("concpt"),partido~variable,fun.aggregate=function(x) c(prop=sum(x)/length(x),total=length(x)))
## partido.voto <- recast(data.votos,partido~variable,measure.var="ausente",fun.aggregate=function(x) c(prop=sum(x)/length(x),total=length(x)))
