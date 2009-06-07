## to do

## needed to win line in the bar graphs
## bar graphs or dot plots with the vote by party and roll call
## create tags for the posts
## store data in mysql
## scrape data from the 

library(foreign)
library(reshape)
source("utils.R")
source("functionsRC.R")
try(source("mergeApprox.R"))
try(source("spatial.R"))
library(RMySQL)
## create  db if it does not exist
driver<-dbDriver("MySQL")

if (exists("connect")) dbDisconnect(connect)
connect<-dbConnect(driver,  username="monte",password="e123456",dbname="congressoaberto",host="mysql.cluelessresearch.com")

##file with the current legislators
##http://www.camara.gov.br/internet/deputado/deputado.xls
## may be a send an email link?


session.now <- paste(sessions[i],rounds[j],sep="")
year.now <- substr((1999-51*4)+sessions[i]*4+j-1,3,4)

##load(file=paste("../data/data.legis",session.now,".RData",sep=""))
##load(file=paste("../data/datalegis.RData",sep=""))
##can collect more data from the individual webpages
##data.legis <- subset(data.legis.all,legislatura==sessions[i])

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

##manual fixes
dnow[with(dnow,tolower(name)=="carlos cur" & is.na(uf)),"uf"] <- "ro"
dnow[with(dnow,tolower(name)=="clovis volpi" & is.na(uf)),"uf"] <- "sp"
dnow[with(dnow,tolower(name)=="gessivaldo isaias" & is.na(uf)),"uf"] <- "pi"
dnow[with(dnow,tolower(name)=="marcelo teixeira" & is.na(uf)),"uf"] <- "ce"
dnow[with(dnow,tolower(name)=="nelson otoch" & is.na(uf)),"uf"] <- "ce"
dnow[with(dnow,tolower(name)=="silvio torres" & is.na(uf)),"uf"] <- "sp"
       
d.rolls <- with(dnow,unique(data.frame(nameroll=toupper(name),uf=toupper(uf))))
d.rolls$id <- 1:nrow(d.rolls)
d.rolls$legislatura <- sessions[i]
## read names from db
d.legis <- dbGetQuery(connect,
                      paste("select nameroll,uf,indb,idcamara from legisladores where legislatura=",sessions[i])
                      )
## have to convert fro latin1
d.legis$nameroll <- iconv(d.legis$nameroll,from="latin1")

d.rolls$nameroll <- tolower(d.rolls$nameroll)
d.legis$nameroll <- tolower(d.legis$nameroll)
d.rolls$uf <- tolower(d.rolls$uf)
d.legis$uf <- tolower(d.legis$uf)
d.rolls <- subset(merge(d.rolls,d.legis,by=c("nameroll","uf"),all.x=TRUE),select=-c(idcamara))




if (sum(is.na(d.rolls$indb))>0) {
  d.rolls.tmp <- subset(d.rolls,is.na(indb))
  d.rolls.tmp$indb <- NULL
  res <- merge.approx(states,d.rolls.tmp,d.legis,"uf","nameroll")
  ##match the remaining (duplicates)
  d.rolls.tmp <- subset(d.rolls.tmp,!id%in%res$x.ind)
  res2 <- data.frame()
  while (nrow(d.rolls.tmp)>0) {
    d.rolls.tmp$id2 <- 1:nrow(d.rolls.tmp)
    print("************************")
    res2 <- rbind(res2,merge.approx(states,d.rolls.tmp,d.legis,"uf","nameroll"))
    d.rolls.tmp <- subset(d.rolls.tmp,!id2%in%res2$x.ind)
  }
  rm(d.rolls.tmp)
  if (nrow(res2)>0) {
    try(res <- subset(res,select=-c(x.ind,y.ind)))
    try(res2 <- subset(res2,select=-c(x.ind,y.ind,id2)))
    res <- rbind(res,res2)
  }
  res$name.1 <- NULL
  res <- unique(with(res,data.frame(nameroll,uf=toupper(uf),legislatura,idcamara)))
  d.legis <- dbGetQuery(connect,
                      paste("select * from legisladores where legislatura=",sessions[i])
                      )
  d.legis <- unique(subset(d.legis,select=-nameroll))
  d.legis <- merge(d.legis,res)
  d.legis$indb <- TRUE
  (dbWriteTable(connect, "legisladores", d.legis, append=TRUE,
                row.names = FALSE, eol = "\r\n" ))
  ## remove duplicates
  try(tmp <- dbSendQuery(connect, "drop table tmp"))
  tmp <- dbSendQuery(connect, "create table tmp as select distinct * from legisladores")
  tmp <- dbSendQuery(connect, "drop table legisladores")
  tmp <- dbSendQuery(connect, "create table legisladores as select * from tmp")  
}


d.legis <- dbGetQuery(connect,
                      paste("select nameroll,uf,idcamara from legisladores where legislatura=",sessions[i])
                      )

d.legis$nameroll <- tolower(iconv(d.legis$nameroll,from="latin1"))
d.legis$uf <- tolower(d.legis$uf)

tmp1 <- merge(dnow,d.legis,by.y=c("nameroll","uf"),by.x=c("name","uf"), all.x=TRUE)
if (sum(is.na(tmp1$idcamara))>0) {
  tmp1 <- subset(tmp1,is.na(idcamara))
  save(tmp1,file=paste("errorLegis",i,"Sess",j,".RData",sep=""))
  stop()
}
##tmp2 <- merge(data.legis,subset(tmp1,select=-c(name,uf)),by="idcamara")
tmp3 <- subset(tmp1,!voto%in%c("PRESENTE","<------>"))
tmp3$n <- with(tmp3,ave(file,file,FUN=length))
tmp3$voto <- factor(tmp3$voto)
tmp3$u <- with(tmp3,ave(voto=="<------->",file,FUN=sum))
##exclude votes with all missing
tmp3 <- subset(tmp3,u!=n)

## are there new votes (instead of "lista de presenca")
if (nrow(tmp3)==0) stop("No new votes")

## create final format
data.votos <- subset(tmp3,select=-c(n,u,nome_par))

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

data.votacoes$wptitle <- paste(data.votacoes[,"proposicao"],data.votacoes[,"data"])
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

data.votacoes$wpcontent <- paste("<p>Link para a proposicao na Camara: ",data.votacoes[,"wpdescricao"],"</p> <p>Gráficos: ",data.votacoes[,"mapa"],data.votacoes[,"resultado"])

data.votacoes$sessao <- session.now
data.votos$sessao <- session.now
data.votos$indb <- TRUE
data.votacoes$indb <- TRUE







###########################################
if (dbExistsTable(connect,"votos")) {
  ## write new rows
  cat('votos exist')

  dbWriteTable(connect, "votos", data.votos, append=TRUE, row.names = F, eol = "\r\n" )
  
} else {
  ## create new table
  (dbWriteTable(connect, "votos", data.votos, overwrite=TRUE,
                row.names = F, eol = "\r\n" ))
}


if (dbExistsTable(connect,"votacoes")) {
  ## write new rows
  dbWriteTable(connect, "votacoes", data.votacoes, append=TRUE, row.names = F, eol = "\r\n" )
  cat('votacoes exist')
} else {
  ## create new table
  (dbWriteTable(connect, "votacoes", data.votacoes, overwrite=TRUE,
                row.names = F, eol = "\r\n" ))
}

rm(data.votacoes,data.votos,tmp1,tmp3,dnow,dbf,dbfd)
gc()
