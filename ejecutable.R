rm(list = ls())
#instalacion de librerias necesarias 
require(ggplot2)
require(forecast)
require(dplyr)
require(shinyBS)
require(data.table)
require(sp)
require(shiny)
require(zoo)
require(rgeos)
require(rgdal)
require(raster)
require(leaflet)
require(tcltk)
#-------------Run-------------
path0="C:/Ero1.0/bin/"
setwd(path0)
load("dataf.Rdata")
ero<-brick("C:/Ero1.0/bin/archivos/erobruta.gri")
erobruta<-brick("C:/Ero1.0/bin/archivos/eroacum.gri")
eroneta<-brick("C:/Ero1.0/bin/archivos/eroneta.gri")
if(length(list.files("C:/Ero1.0",all.files = T,full.names = T,pattern = "\\.shp$"))>0){ 
  s<-shapefile(list.files("C:/Ero1.0",all.files = T,full.names = T,pattern = "\\.shp$"))
  locs<-data.frame(loc=s$Name,lon=coordinates(s)[,1],lat=coordinates(s)[,2],stringsAsFactors = F)  
} else { locs<-read.csv("C:/Ero1.0/station.csv",stringsAsFactors = F)}
decades <- seq(1981, 2015, by=1)
lon <- -73
lat <- -10
tb<-table(point = locs[1,])
for(i in 2:nrow(locs)) tb<-rbind(tb,table(point = locs[i,]))
colnames(tb)<-  c(names(tb)[1],"Location",names(tb)[3:ncol(tb)])
tb$Eroacum<-tb$Eroacum*7741.206/100
tb$Eronta<-tb$Eronta*7741.206/100
d<-tb
x<-  eroneta

runApp(appDir = path0,launch.browser = T)
#runApp(appDir = "C:/Users/uSER/Desktop/EntregarANA/Ero1.0/bin",launch.browser = T)

save(ero,erobruta,eroneta,pallete,shp,stk,plotERO,table,
     file = "C:/Users/uSER/Desktop/EntregarANA/Ero1.0/bin/dataf.Rdata")

plotERO<- function(data=prub,pronost=20,type="MMC",perio=19812015,sediment=1800,traini=3){ 
  #prub<-d[d$Location=="RIO SANTA",]
  set.seed(10)
  if(type=="Erobrt"){
    ero1<-data$Erobruta
    ero1[which(!ero1<median(ero1)*10)]<-rep(median(ero1),length(which(!ero1<median(ero1)*10)))}
  if(type== "Eroacum") ero1<-data$Eroacum
  if(type== "Eronta") ero1<-data$Eronta
  if(type== "MMC") ero1<-data$Eronta*1000/(10^6*sediment)
  ero.zoo<-zoo(ero1,seq(as.Date("1981-01-01"),as.Date("2015-01-01"),"year"))
  #............
  range=as.numeric(substr(perio,1,4)):as.numeric(substr(perio,5,8))-1980
  #............
  mod_neural = nnetar(ero1[range], p=traini, size=5)
  fr<-forecast(mod_neural,pronost)
  observado<-as.numeric(ero1)
  simulado<-as.numeric(fr$mean)
  simulado[simulado<0]=0
  simulado[simulado==0]<-quantile(ero1,probs=0.1)
  how<-1981+length(observado)+length(simulado)-1
  df<-data.frame(year=1981:how,datos=c(observado,simulado),state=c(rep(1,length(observado)),rep(2,length(simulado))))
  ##---Poner Mediana--
  if(type== "MMC"){ 
  Me<-median(c(observado,simulado))
  ggplot(df,aes(x=year,y=datos,fill=state))+geom_bar(stat="identity")+theme_bw()+
    scale_x_continuous(expand=c(0,0),breaks = seq(from = 1981,to = df$year[length(df$year)],by=10))+
    scale_y_continuous(expand=c(0,0))+
    xlab("Year")+ylab("SY (MMC / Year)")+
    scale_fill_continuous(breaks=c(1,2),labels=c("Estimacion RUSLE","Simulacion RNA"),
                          guide=guide_legend(keywidth = 1,title=NULL))+
    geom_line(aes(x=year,Me),color="red",size=1)
  #ggtitle(paste0("Promedio de erosion ",Me))
}else{ 
  Me<-median(c(observado,simulado))
  ggplot(df,aes(x=year,y=datos,fill=state))+geom_bar(stat="identity")+theme_bw()+
    scale_x_continuous(expand=c(0,0),breaks = seq(from = 1981,to = df$year[length(df$year)],by=10))+
    scale_y_continuous(expand=c(0,0))+
    xlab("Year")+ylab("SY (Tn / Year)")+
    scale_fill_continuous(breaks=c(1,2),labels=c("Estimacion RUSLE","Simulacion RNA"),
                          guide=guide_legend(keywidth = 1,title=NULL))+
    geom_line(aes(x=year,Me),color="red",size=1)
  #ggtitle(paste0("Promedio de erosion ",Me))
  }  }