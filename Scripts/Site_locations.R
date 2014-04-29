#script for locations of sites used for trait analysis

library(ggplot2)
library(cshapes)

setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Data")
#import WQ data
AGC<-read.csv("AGC.csv")
AGC<-subset(AGC,H_inv!="#N/A")
BGC<-read.csv("BGC.csv")
BGC<-subset(BGC,!is.na(I_H))
WP<-read.csv("WP.csv")
WP<-subset(WP,!is.na(I_Root))
WQ<-read.csv("WQ.csv")
WQ<-subset(WQ,!is.na(I_Height))

AGC2<-data.frame(Lat=AGC$Y,Long=AGC$X,Type="Aboveground carbon")
BGC2<-data.frame(Lat=BGC$Y,Long=BGC$X,Type="Belowground carbon")
WP2<-data.frame(Lat=WP$Y,Long=WP$X,Type="Water provision")
WQ2<-data.frame(Lat=WQ$Y,Long=WQ$X,Type="Water quality")

Locs<-rbind(AGC2,BGC2,WP2,WQ2)


#plot locations
world <- cshp(date=as.Date("2008-1-1"))
world.points <- fortify(world, region='COWCODE')
p <- ggplot(world.points, aes(long,lat,group=group)) + geom_polygon(colour="light grey",fill="light grey")+theme(panel.grid.major = element_line(colour =NA),panel.grid.minor = element_line(colour =NA),panel.border = element_rect(size=1.5,colour="black",fill=NA))
locs_p<-p+geom_point(data=Locs,aes(x=Long,y=Lat,group=NULL,colour=Type),alpha=0.5)+facet_wrap(~Type,ncol=1)+scale_colour_brewer(palette="Set1")+coord_equal()
locs_p2<-locs_p+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA),axis.title=element_blank(),title=element_blank())
locs_p2+theme(axis.line=theme_blank(),axis.text.x=theme_blank(),
        axis.text.y=theme_blank(),axis.ticks=theme_blank(),
        axis.title.x=theme_blank(),
        axis.title.y=theme_blank())
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Figures")
ggsave("Trait_locations.png",height=8,width=8,units="in",dpi=400)
