library(foreign)
rounds <- c("Primeira","Segunda","Terceira","Quarta")
## 51:53
sessions <- 51:53

data.legis.all <- data.frame()
## figure out how to automatically download these files
for (i in 1:length(sessions)) {
  f <- paste("../data/dep_lista.asp",sessions[i],".html",sep="")
  time.download <- Sys.time() #f# will have to change this if downloading manually
  ll <- readLines(f,encoding="latin1")
  pe <- ll[grep("Partido/UF",ll)]
  if (length(pe)==0) {
    ll <- readLines(f,encoding="latin1")
    ## here if it is a list of previous sessions
    punloc <- grep(".*-.*/[A-Z]{2}",ll,perl=TRUE)    
    pun <- ll[punloc]
    ll <- ll[-punloc]
    pun <- gsub("[\t ]+"," ",pun)
    id1 <- gsub(".*id=([0-9]*).*","\\1", pun)
    pun <- gsub(" (.*) - (.*)/([A-Z]{2})<.*","\\1;\\2;\\3",pun)
    pun <- data.frame(do.call(rbind,strsplit(pun,";")))
    id2 <- gsub(".*id=([0-9]*).*","\\1", ll[grep("id=",ll)])    
    names(pun) <- c('name','partido','uf')
    data.legis <- data.frame(idcamara=id2,
                             name=pun$name,uf=pun$uf,partido.current=pun$partido)
  } else {
    pe <- gsub(" +"," ",pe)
    pe <- gsub("\t","",pe)
    pe <- gsub(".*UF: ([A-Za-z]*) /([A-Z]*).*","\\1;\\2",pe)
    pe <- strsplit(pe,";")
    partido.current <- sapply(pe,function(x) x[[1]])
    uf <- sapply(pe,function(x) x[[2]])
    t0 <- grep("Mais Detalhes",ll)
    ids <- gsub(".*id=(.*)\".*","\\1",ll[t0])
    names <- gsub("\t| *$","",ll[t0-5])
    out <- substr(names,1,1)=="<"
    names[out] <- gsub(" +$","",gsub(".*<b>(.*)</b>.*","\\1",ll[t0[out]-4]))
    data.legis <- data.frame(idcamara=ids,name=names,uf=uf,partido.current=partido.current)
  }
  data.legis$legislatura <- sessions[i]
  data.legis.all <- rbind(data.legis.all,data.legis)
}

data.legis.all$nameroll <- data.legis.all$name


library(RMySQL)
## create  db if it does not exist
driver<-dbDriver("MySQL")
if (exists("connect")) dbDisconnect(connect)
connect<-dbConnect(driver,  username="monte",password="e123456",dbname="congressoaberto",host="mysql.cluelessresearch.com")

r.now <- data.legis.all
if (dbExistsTable(connect,"legisladores")) {
  ## load table
  old <- (dbGetQuery(connect, "SELECT idcamara,legislatura,indb from legisladores"))
  ##old$name <- iconv(old$name,from="latin1")
  ##look for ids not in db
  tmp <- merge(r.now,old,all.x=TRUE)
  r.now <- tmp[is.na(tmp$indb),]    
  if (nrow(r.now)>0) {
    r.now$indb <- TRUE
    ## write new rows
    (dbWriteTable(connect, "legisladores", r.now, append=TRUE, row.names = F, eol = "\r\n" ))
    cat('here')
  }
}  else {
  ## create new table
  r.now$indb <- TRUE
  (dbWriteTable(connect, "legisladores", r.now, overwrite=TRUE,
                   row.names = F, eol = "\r\n" ))
}

## manual adds
## Pastor Jorge -> Jorge Pinheiro id 100606
## http://blogs.maiscomunidade.com/blogdocallado/2009/01/07/pastor-jorge-e-brunelli-em-goiania/
r.now <- dbGetQuery(connect,"select * from legisladores where idcamara=100606  AND legislatura=51 limit 1")
r.now$nameroll <- "Pastor Jorge"
(dbWriteTable(connect, "legisladores", r.now, append=TRUE, row.names = F, eol = "\r\n" ))

r.now <- dbGetQuery(connect,"select * from legisladores where idcamara=100606 AND legislatura=52 limit 1")
r.now$nameroll <- "Pastor Jorge"
(dbWriteTable(connect, "legisladores", r.now, append=TRUE, row.names = F, eol = "\r\n" ))

