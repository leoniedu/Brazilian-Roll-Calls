theme_blue <- function (base_size = 12) 
{
    structure(list(axis.line = theme_blank(), axis.text.x = theme_text(size = base_size * 0.8, lineheight = 0.9, vjust = 1), axis.text.y = theme_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1), axis.ticks = theme_segment(colour = "black", size = 0.2), axis.title.x = theme_text(size = base_size , vjust = 1
 ), axis.title.y = theme_text(size = base_size, angle = 90, vjust = .5), axis.ticks.length = unit(0.3, "lines"), axis.ticks.margin = unit(0.5, "lines"), legend.background = theme_rect(colour = NA), legend.key = theme_rect(colour = "grey80"), legend.key.size = unit(1.2, "lines"), legend.text = theme_text(size = base_size * 0.8), legend.title = theme_text(size = base_size * 0.8, face = "bold", hjust = 0), legend.position = "right", panel.background = theme_rect(fill = rgb(0,.25,.5,.3), colour = NA), panel.border = theme_rect(fill = NA, colour = "grey50"), panel.grid.major = theme_line(colour = "white", size = 0.2), panel.grid.minor = theme_line(colour = "white", size = 0.1), panel.margin = unit(0.25, "lines"), strip.background = theme_rect(fill = "grey80", colour = "grey50"), strip.label = function(variable, value) value, strip.text.x = theme_text(size = base_size * 0.8), strip.text.y = theme_text(size = base_size * 0.8, angle = -90), plot.background = theme_rect(colour = NA), plot.title = theme_text(size = base_size * 1.2), plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")), class = "options")
}
data.votos$ausente <- data.votos$voto=="ausente"

m1 <- glmer(ausente~(1|estado)+(1|partido)+(1|idcamara)+(1|numvot),data=data.votos,family=binomial)

m1.sim <- sim(m1,n.sims=1000)
m1.sim.1 <- data.frame(melt(m1.sim$partido[,,1]))
m1.sim.1$p <- invlogit(m1.sim$fixef[,1]+m1.sim.1$value) 
m1.sim.1 <- recast(m1.sim.1,X2~variable,measure.var=c("value","p"),fun.aggregate=quantile,probs=c(.1,.5,.8))
names(m1.sim.1) <- c("Partido",paste("value",c("lo","mid","hi"),sep="."),paste("p",c("lo","mid","hi"),sep="."))
m1.sim.1$Partido <- with(m1.sim.1,reorder(Partido,p.mid))
m1.sim.1 <- merge(m1.sim.1,reshape::rename(recast(data.votos,partido~variable,measure.var="voto",fun.aggregate=length),c(partido="Partido")))

qplot(Partido,p.mid,data=m1.sim.1,ymin=p.lo,ymax=p.hi,geom="linerange")+geom_point(data=m1.sim.1,mapping=aes(size=voto),colour=rgb(0,.25,.5))+coord_flip()

x.high <- setx(m1, partido = "PT") 
x.low <- setx(m1, partido = "DEM")
##Generate ﬁrst diﬀerences for the eﬀect of high versus low education on voting: 
s.out1 <- sim(m1, x = x.high, x1 = x.low) 
summary(s.out1) 

##qplot(rnorm(100),runif(100))+theme_bw2()
qplot(Partido,p.mid,data=m1.sim.1,ymin=p.lo,ymax=p.hi,geom="pointrange")+coord_flip()+theme_blue()
