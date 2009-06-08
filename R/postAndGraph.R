library(RColorBrewer)
library(ggplot2)
library(GDD)
source("spatial.R")
##load(file=paste('../data/',session.now,'.RData',sep=''))
m1 <- readShape.cent("../data/maps/BRASIL.shp","UF")
updateall <- FALSE

##FIX this should be set earlier
dbSendQuery(connect,"alter table votacoes drop column wpid")
dbSendQuery(connect,"alter table votacoes add column wpid varchar(20)")


data.votos <- dbGetQuery(connect,'select * from votos where sessao="51Primeira"')
for ( i in which(sapply(data.votos,is.character))) {
  data.votos[,i] <- iconv(data.votos[,i],from="latin1")
}


## get category table
## if (exists("connect")) dbDisconnect(connect)
## connect<-dbConnect(driver,  username="monte",password="e123456",dbname="congressoaberto",host="mysql.cluelessresearch.com")
## cats <-  dbGetQuery(connect,"select * from wp_hufib7_terms where term_group=0")

##TO DO: CREATE in DB
##FIX: there are NAs in vote.
##FIX: Year=2019 in one of the 51Primeira votes
##FIX: Year is wrong in the links to the proposicao (should have 4 digits)
##FIX: dots (.) appearing in "tipo"
##FIX: links are not live when posts are displayed in the first page
##FIX: check
data.votacoes <- dbGetQuery(connect,'select * from votacoes') ##  where sessao="51Primeira"
for ( i in which(sapply(data.votacoes,is.character))) {
  data.votacoes[,i] <- iconv(data.votacoes[,i],from="latin1")
}
data.votacoes$tipo <- gsub("\\.","",data.votacoes$tipo)
data.votacoes$wpidold <- data.votacoes$wpid
data.votacoes$wpcontent <- gsub("Link para a proposicao na Camara: ","",data.votacoes$wpcontent)
data.votacoes$tipo <- car::recode(data.votacoes$tipo,"'MENSAGEM'='MSG';'PROCESSO'='PRC';'PROPOSICAO'='PRP';'RECURSO'='REC';'REQUERIMENTO'='REQ'")

for (i in nrow(data.votacoes):1) {
##for (i in 1:5) {
  filenow <- as.character(data.votacoes$file_name[i])
  ## create graphs
  graphs(filenow)
  ## create blog post
  data.votacoes$wpid[i] <- with(data.votacoes[i,], {
    res <- wpid
    cat(wptitle,file="title.txt")
    cat(wpimage,file="imagelink.txt")
    cat(wpcontent,file="post.txt")
    cat(res,file="postid.txt")
    cat(paste(gsub(" +","",proposicao),tipo, format.Date(data,"%Y"),sep=","),file="tags.txt")
    ##put this earlier in the code
    cat(gsub(" |-","",as.character(wpdate)),file='date.txt')
    cat("Votações",file='category.txt')
    res <- system("python post.py",intern=TRUE)
    print(res)
    res
  })
  ##file.show("tags.txt")
  ##   file.show("imagelink.txt")
  ##file.show("postid.txt")
}
##write tmp table with wpids and file_name
tmp <- subset(data.votacoes,(wpid!=wpidold),select=c(wpid,file_name))
if (nrow(tmp)>0) {
  try(dbRemoveTable(connect,"tmp"))
  dbWriteTable(connect,"tmp",tmp)
  dbSendQuery(connect, "update votacoes, tmp set votacoes.wpid = tmp.wpid where votacoes.file_name = tmp.file_name")
}
