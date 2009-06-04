library(foreign)
library(RColorBrewer)
library(ggplot2)
library(GDD)
library(reshape)
source("utils.R")
source("functionsRC.R")
try(source("mergeApprox.R"))
try(source("spatial.R"))

##file with the current legislators
##http://www.camara.gov.br/internet/deputado/deputado.xls
## may be a send an email link?


session.now <- paste(sessions[i],rounds[j],sep="")
year.now <- substr((1999-51*4)+sessions[i]*4+j-1,3,4)

load(file=paste("../data/data.legis",session.now,".RData",sep=""))
##can collect more data from the individual webpages
head(data.legis)

names(dbf) <- tolower(names(dbf))
dbfd <- decode(dbf)
dbfd$file.name <- paste("www.camara.gov.br/internet/votacaodbf/",session.now,"/","CD",year.now,pad0(as.numeric(as.character(dbf$numvot)),4),".dbf",sep="")


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
if (nrow(res2)>0) {
  try(res <- subset(res,select=-c(x.ind,y.ind)))
  try(res2 <- subset(res2,select=-c(x.ind,y.ind,id2)))
  res <- rbind(res,res2)
}

res$name.1 <- NULL
tmp1 <- merge(dnow,subset(res,select=-c(id)),by.x=c("name","uf"),by.y=c("name","uf"))
tmp2 <- merge(data.legis,subset(tmp1,select=-c(name,uf)),by="idcamara")
tmp3 <- subset(tmp2,!voto%in%c("PRESENTE","<------>"))
tmp3$n <- with(tmp3,ave(file,file,FUN=length))
tmp3$voto <- factor(tmp3$voto)
tmp3$u <- with(tmp3,ave(voto=="<------->",file,FUN=sum))
##exclude votes with all missing
tmp3 <- subset(tmp3,u!=n)

## are there new votes (instead of "lista de presenca")
if (nrow(tmp3)==0) stop("No new votes")

## create final format
data.votos <- subset(tmp3,select=-c(n,u,threshold,nome_par,inoffice,time))
data.votos$voto <- tolower(car::recode(data.votos$voto,"'<------->'='AUSENTE'"))
data.votos$voto <- factor(data.votos$voto,levels=c("sim","nao", "obstrucao","abstencao", "art. 17", "ausente"))
data.votos$file.name <- factor(data.votos$file)
data.votos$file <- NULL
data.votos <- reshape::rename(data.votos,c(`name`="nome"))

tmp4 <- recast(data.votos,file.name~voto,measure.var="idcamara",fun.aggregate=length,margins="grand_col")
data.votacoes <- merge(subset(dbfd,select=c(numvot,file.name,datavot,texordia)),tmp4,by="file.name")
data.votacoes <- reshape::rename(data.votacoes,c(`(all)`="total"))
data.votos$voto2 <- car::recode(data.votos$voto,"'sim'='A Favor';else='Contra'")

conc.pt <- recast(subset(data.votos,partido=="PT"),file.name~variable,measure.var="voto2",fun.aggregate=rattle::modalvalue)
conc.pt <- reshape::rename(conc.pt, c(voto2="votopt"))
data.votos <-merge(data.votos, conc.pt)
data.votos$concpt <- with(data.votos, as.character(votopt)==as.character(voto2))
ss <- strsplit(gsub(" +"," ",as.character(data.votacoes$texordia)),c(" |/"))
data.votacoes$tipo <- factor(sapply(ss,function(x) x[1]))
data.votacoes$numero <- as.numeric(sapply(ss,function(x) x[3]))
data.votacoes$ano <- as.numeric(sapply(ss,function(x) x[4]))
data.votacoes$proposicao <- with(data.votacoes,paste(tipo," ",numero,"/",ano,sep=""))
data.votacoes$descricao <- sapply(ss,function(x) paste(x[6:length(x)], collapse=" "))


data.votacoes$data <- as.character(as.Date(data.votacoes$datavot,format="%y%m%d"))
data.votacoes$wpdate <- as.character(paste(data.votacoes$data,"T12:00:00"))
data.votacoes$wpid <- NA
data.votacoes <- data.votacoes[order(data.votacoes$data,decreasing=TRUE),]
rownames(data.votacoes) <- NULL
data.votos$ausente <- data.votos$voto=="ausente"



fname <- with(data.votacoes,paste(datavot,".",substr(file.name,nchar(file.name)-11,nchar(file.name)-4),"",sep=""))  

data.votacoes$wptitle <- paste(data.votacoes[,"proposicao"],data.votacoes[,"descricao"],data.votacoes[,"data"])
data.votacoes$wpimage <- with(data.votacoes,paste('images/',fname,'.png',sep=''))
data.votacoes$resultado <- with(data.votacoes, paste('<img src="http://cluelessresearch.com/images/',fname,'barsmall.png','" height=45 width=45 />',sep=''))
data.votacoes$resultado <- with(data.votacoes, paste('<a href="http://cluelessresearch.com/images/',fname,'bar.png','">',resultado,'</a>',sep=''))
data.votacoes$mapa <- with(data.votacoes, paste('<img src="http://cluelessresearch.com/images/',fname,'small.png','" height=45 width=45 />',sep=''))
data.votacoes$mapa <- with(data.votacoes, paste('<a href="http://cluelessresearch.com/images/',fname,'.png','">',mapa,'</a>',sep=''))
data.votacoes$wpdescricao <- sapply(data.votacoes$descricao,function(x) wordwrap(x,40,collapse="<br>",prefix=""))
data.votacoes$wpdescricao <- with(data.votacoes,
paste('<a href="http://www.camara.gov.br/sileg/Prop_Lista.asp?Sigla=',
tipo,
                                 '&Numero=',
                                 numero,
                                 '&Ano=',ano,'">',wpdescricao,'</a>',sep=''))

data.votacoes$wpcontent <- paste("Link para a proposicao na Camara: ",data.votacoes[,"descricao"],data.votacoes[,"mapa"],data.votacoes[,"resultado"])


##take this out data.votacoes <- subset(data.votacoes,select=c(data,proposicao,descricao,resultado,mapa))

## should add this through a database
##FIX!
data.votos.new <- data.votos
data.votacoes.new <- data.votacoes



##load(file=paste('../data/',session.now,'.RData',sep=''))
m1 <- readShape.cent("../data/maps/BRASIL.shp","UF")
updateall <- FALSE
for (i in 1:nrow(data.votacoes)) {
  print(i)
  filenow <- as.character(data.votacoes$file.name[i])
  ## create graphs
  ##graphs(filenow)
  ## create blog post
  data.votacoes$wpid[i] <- with(data.votacoes[i,], {
    res <- wpid[i]
    if(is.na(res)|updateall) {
      cat(wptitle,file="title.txt")
      cat(wpimage,file="imagelink.txt")
      cat(wpcontent,file="post.txt")
      ##put this earlier in the code
      cat(gsub(" |-","",as.character(wpdate)),file='date.txt')
      cat(as.character(tipo),file='category.txt')
      ##res <- system("python post.py",intern=TRUE)
    } else {
      ##modify the python script so that it just edits the post when there is already an id number
    }
    res
  })
  ##   file.show("title.txt")
  ##   file.show("imagelink.txt")
  ##   file.show("post.txt")
}

## if (file.exists(paste('../data/',session.now,'.RData',sep=''))) {
##   load(file=paste('../data/',session.now,'.RData',sep=''))
##   data.votos <- merge(data.votos,data.votos.new,all=TRUE)
##   data.votacoes <- merge(data.votacoes,data.votacoes.new,all=TRUE)
## }

save(data.votos,data.votacoes,file=paste('../data/',session.now,'.RData',sep=''))

