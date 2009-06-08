library(foreign)

## global vars
sessions <- 51:53
rounds <- c("Primeira","Segunda","Terceira","Quarta")


decode <- function(x) data.frame(lapply(x,
                                        function(z) {
                                          if (is.character(z)|is.factor(z)){
                                            iconv(z,from="cp850",to="")
                                          } else {
                                            z
                                          }
                                        }))

getx <- function(x) {
  fname <- paste(rdir,"CD",year.now,x,".dbf",sep="")
  system(paste("wget ",fname," -Nr -P ../data",sep=""))
}

readx <- function(x) {
  fname <- paste(rdir,"CD",year.now,x,".dbf",sep="")
  dnow <- try(read.dbf(paste("../data/",fname,sep="")))
  if (!"try-error"%in%class(dnow)) {
    dnow$file <- fname
    dnow$file_name <- x
    dnow
  } else {
    NULL
  }
}


## table(data.votos$partido)

## ggplot(tmp, aes(x = voto2))+geom_bar(width = wd,aes(fill = voto))+geom_bar(data=tmp,colour=colvec,width=wd,size=6,fill="transparent")+facet_wrap(~partido)
## ##+scale_y_continuous(name="",limits=c(0,513),expand=c(0,0))

## tmp$voto <- relevel(factor(tmp$voto),"sim")

## ggplot(tmp, aes(x = partido))+geom_bar(aes(fill = voto),position="fill")





barplot.rc <- function(filenow,fname,title) {
  tmp <- subset(data.votos,file_name==filenow)
  colvec <- c("transparent","blue")[order(table(tmp$votopt))]
  ## Stacked barchart
  wd <- 1
  theme_set(theme_grey(base_size = 10))
  p <- ggplot(tmp, aes(x = voto2))+geom_bar(width = wd,aes(fill = voto))+geom_bar(data=tmp,colour=colvec,width=wd,size=6,fill="transparent")+scale_y_continuous(name="",limits=c(0,513),expand=c(0,0))
  p <- p+theme_bw()+opts(axis.title.x = theme_blank(),
                         axis.title.y = theme_blank(),
                         panel.grid.minor = theme_blank(),
                         panel.grid.major=theme_blank(),
                         panel.background=theme_rect(fill = NA, colour = NA),
                         plot.background = theme_rect(colour = NA,fill=NA)
                         ,plot.title = theme_text(size = 10))
  psmall <- p+opts(legend.position="none",
                   axis.text.y = theme_blank(),
                   axis.text.x = theme_blank()
                   ,axis.ticks = theme_blank()
                   ,panel.border = theme_blank()
                   )
  p <- p+opts(title=title)
  pdf(file=paste(fname,"bar.pdf",sep=""),height=6,width=6)
  print(p)
  dev.off()
  pdf(file=paste(fname,"barsmall.pdf",sep=""),height=6,width=4)
  print(psmall)
  dev.off()  
}

map.rc <- function(filenow,fname,title) {
  tmp <- recast(subset(data.votos,file_name==filenow),uf~variable,measure.var="concpt",fun.aggregate=function(x) c(n=length(x),p=sum(x)/length(x)))
  tmp$UF <- tmp$uf
  m2 <- merge.sp(m1,tmp,by="UF")
  par(bg="grey")
  n1 <- 4
  seqx <- c(0,.15,.3,.45,.55,.70,.85,1)
  col.vec <- c(rev(brewer.pal(n1,"Blues")[-1]),"grey95",brewer.pal(n1,"Reds")[-1])
  pdf(file=paste(fname,"small.pdf",sep=""),height=6,width=6)
  par(mai=c(0,0,0,0))
  plot.heat(m2,NULL,"concpt_p",title="Proporção votando\njunto com o PT",breaks=seqx,reverse=FALSE,cex.legend=1,bw=1,col.vec=col.vec,plot.legend=FALSE)
  dev.off()
  pdf(file=paste(fname,".pdf",sep=""),height=6,width=6)
  par(mai=c(0,0,0.6,0))
  plot.heat(m2,NULL,"concpt_p",title="Proporção votando\njunto com o PT",breaks=seqx,reverse=FALSE,cex.legend=1,bw=1,col.vec=col.vec,main=filenow)
  with(m2@data,text(x,y,UF,cex=0.8),col="grey30")
  mtext(title,3, cex=.9)
  dev.off()
}

graphs <- function(filenow) {  
  dn <- subset(data.votacoes,file_name==filenow)
  fname <- with(dn,paste("../images/",datavot,".",substr(filenow,nchar(filenow)-11,nchar(filenow)-4),"",sep=""))  
  title <- wordwrap(subset(data.votacoes,file_name==filenow)$texordia,40)
  cat(".")
  barplot.rc(filenow,fname,title)
  map.rc(filenow,fname,title)
  system(paste("convert -density 400x400 -resize 45x45 -quality 90 ", paste(fname,"small.pdf",sep="")," ",fname,"small.png",sep=""),wait=TRUE)
  system(paste("convert -density 400x400 -resize 45x45 -quality 90 ", paste(fname,"barsmall.pdf",sep="")," ",fname,"barsmall.png",sep=""),wait=TRUE)
  system(paste("convert -density 400x400 -resize 800x800 -quality 90 ", paste(fname,"bar.pdf",sep="")," ",fname,"bar.png",sep=""),wait=TRUE)
  system(paste("convert -density 400x400 -resize 800x800 -quality 90 ", paste(fname,".pdf",sep="")," ",fname,".png",sep=""),wait=TRUE)
}
