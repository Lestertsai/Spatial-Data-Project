#https://stackoverflow.com/questions/21977720/r-finding-closest-neighboring-point-and-number-of-neighbors-within-a-given-rad
#https://www.researchgate.net/post/Whats-the-best-package-in-R-for-calculating-the-distance-between-two-polygons-Should-I-convert-the-polygons-to-points-first
x=c("readr","rgeos","sp","rgdal","readxl","sf","ggplot2",
    "viridis","fields","spdep","INLA")
lapply(x, require, character.only = TRUE)
pop=read_excel('C:/Users/蔡昭誼/Desktop/專題/population.xls')
store=read_excel('C:/Users/蔡昭誼/Desktop/專題/store.xls')
fund=read_excel('C:/Users/蔡昭誼/Desktop/專題/fund.xls')
monitor=read.csv('C:/Users/蔡昭誼/Desktop/專題/moniter.csv')
bike=read_csv('C:/Users/蔡昭誼/Desktop/專題/bike.csv')
car=read_csv('C:/Users/蔡昭誼/Desktop/專題/car.csv')
school=read_excel('C:/Users/蔡昭誼/Desktop/專題/school.xlsx')
hospital=read_excel('C:/Users/蔡昭誼/Desktop/專題/hospital.xls')
new=readOGR("C:/G97_63000_U0202_2015.shp",encoding='ANSI')
ans=CRS(SRS_string="EPSG:3826")
an=CRS("+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
#Convert data to proper type
pop=as.data.frame(pop[-1,])
pop[,-c(1,11)]=sapply(pop[,-c(1,11)],as.numeric)
store=as.data.frame(store[-1,])
store[,2]=sapply(store[,2],as.numeric)
fund=as.data.frame(fund[-1,])
fund[,-c(1,10)]=sapply(fund[,-c(1,10)],as.numeric)
new$AREA=new$AREA/(10^6)
yg=new[-which(new$CODE2 %in% setdiff(new$CODE2,pop$CODE2)),]
ty=merge(new,pop,by="CODE2", all = TRUE)
styg=new[-which(new$CODE2 %in% setdiff(new$CODE2,store$CODE2)),]
styg=merge(new,store,by="CODE2", all = TRUE)
fundyg=merge(new,fund,by="CODE2", all = TRUE)
school=as.data.frame(school[-1,3:4])
school=sapply(school, as.numeric)
hos=as.data.frame(hospital[-1,-6])
hos[,2:5]=sapply(hos[,2:5], as.numeric)
hos[,2:5]=hos[,2:5]/new$AREA
#hospital
hos=merge(new,hos,by="CODE2", all = TRUE)
new1=st_as_sf(new1)
#H_BED:p-value(0.937005)
ggplot(data =hos) +
  geom_sf(aes(fill = H_BED)) +
  scale_fill_distiller(palette =  "Spectral",
                       direction = -1, name = "比例") +
  labs(title="台北市醫院病床數密度區域圖")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())
#H_CNT:p-value(2.758841e-54)
ggplot(data =hos) +
  geom_sf(aes(fill = H_CNT)) +
  scale_fill_distiller(palette =  "Spectral",
                       direction = -1, name = "比例") +
  labs(title="台北市醫療院所家數密度區域圖")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())
#H_SRVP:p-value(0.01188329),H_SRVB:p-value(0.9346543)
#age rate(p-value <2.2e-16)
ty$rate=(ty$A0A14_M_CNT+ty$A15A64_M_CNT+ty$A15A64_M_CNT)/
  (ty$A0A14_F_CNT+ty$A15A64_F_CNT+ty$A65UP_F_CNT)
ty=st_as_sf(ty)
ggplot(data =ty) +
  geom_sf(aes(fill = ty$rate)) +
  scale_fill_distiller(palette =  "Spectral",
                       direction = -1, name = "比例") +
  labs(title="台北市性別比區域圖")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())
ty=subset(ty,!is.na(tyy$rate))
ty=school
sp.na.omit <- function(x, margin=1) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
  if(margin == 1) {  
    cat("DELETING ROWS: ", na.index, "\n") 
    return( x[-na.index,]  ) 
  }
  if(margin == 2) {  
    cat("DELETING COLUMNS: ", na.index, "\n") 
    return( x[,-na.index]  ) 
  }
}
ty=sp.na.omit(ty,1)
ty=hos
nb <- poly2nb(ty, queen=TRUE)
sub_nb <- subset(nb, subset=card(nb) > 0)
lw <- nb2listw(sub_nb, style="B", zero.policy=TRUE)
tyy=ty[card(nb) > 0,]
cv=moran.test(tyy$H_SRVB, lw, zero.policy=TRUE)
#old people(p-value< 2.2e-16)
ty$oldrate=ty$A65UP_CNT/ty$AREA
ggplot(data =ty) +
  geom_sf(aes(fill=oldrate)) +
  scale_fill_distiller(palette =  "Spectral",
                       direction = -1, name = "人數/平方公里") +
  labs(title="台北市老年人口密度圖")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())
#Young people(p-value< 2.2e-16)
ty$ygrate=ty$A0A14_CNT/ty$AREA
ggplot(data =ty) +
  geom_sf(aes(fill = ygrate)) +
  scale_fill_distiller(palette =  "Spectral",
                       direction = -1, name = "人數/平方公里") +
  labs(title="台北市年輕人口密度圖")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())
#store(p-value= 1.531e-11)
styg$strate=styg$STORE/styg$AREA
ggplot(data =styg) +
  geom_sf(aes(fill = strate)) +
  scale_fill_distiller(palette =  "Spectral",
                       direction = -1, name = "數量/平方公里") +
  labs(title="台北市超商密度圖")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())
#fund(p-value= 1.474e-05)
fundyg$rate=(fundyg$TYPE1+fundyg$TYPE2+fundyg$TYPE3+fundyg$TYPE4+
               fundyg$TYPE5+fundyg$TYPE6+fundyg$TYPE7+fundyg$TYPE8)/fundyg$AREA
ggplot(data =fundyg) +
  geom_sf(aes(fill = rate)) +
  scale_fill_distiller(palette =  "Spectral",
                       direction = -1, name = "數量/平方公里") +
  labs(title="台北市社福機構密度圖")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())
#school(p-value< 2.2e-16)
new=readOGR("C:/G97_63000_U0202_2015.shp")
#Change data
ww1=as.data.frame(school)
colnames(ww1)=c("Latitude","Longitude")
coordinates(ww1) <- ~ Longitude + Latitude
proj4string(ww1) <- CRS("+init=epsg:4326")
state.merc <- spTransform(new, CRS=CRS("+init=epsg:4326"))
#Determine
qq=over(state.merc,ww1,returnList = TRUE)
#Draw Plot
q=as.data.frame(sapply(qq, length))
colnames(q)='sch'
school=cbind(new,q)
school$AREA=school$AREA/(10^6)
ggplot(data =school) +
  geom_sf(aes(fill =school$sch)) +
  scale_fill_distiller(palette =  "Spectral",
                       direction = -1, name = "比例") +
  labs(title="台北市教育機構密度圖")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())
#crime
#monitor(p-value< 2.2e-16)
ww=data.frame(monitor)
colnames(ww)=c("Latitude","Longitude")
coordinates(ww) <- ~ Longitude + Latitude
proj4string(ww) <- CRS("+init=epsg:4326")
state.merc <- spTransform(new, CRS=CRS("+init=epsg:4326"))
qq=over(state.merc,ww,returnList = TRUE)
#Draw Plot
q1=as.data.frame(sapply(qq, length))
ty2=cbind(new,q1)
ty2$AREA=ty2$AREA/(10^6)
ty2$den=ty2$sapply.qq..length./ty2$AREA
ggplot(data =ty2) +
  geom_sf(aes(fill = den)) +
  scale_fill_distiller(palette =  "Spectral",
                       direction = -1, name = "數量/平方公里") +
  labs(title="台北市監視器密度圖")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())
new=readOGR("C:/G97_63000_U0202_2015.shp")
my_data1 <- read_excel("C:/Users/蔡昭誼/Desktop/專題/homerob.xlsx",skip=1)
#Change data
ww1=data.frame(my_data1)
ww1=ww1[,3:4]
colnames(ww1)=c("Latitude","Longitude")
coordinates(ww1) <- ~ Longitude + Latitude
proj4string(ww1) <- CRS("+init=epsg:4326")
state.merc <- spTransform(new, CRS=CRS("+init=epsg:4326"))
#Determine
qq=over(state.merc,ww1,returnList = TRUE)
#Draw Plot
q1=as.data.frame(sapply(qq, length))
my_data2 <- read_excel("C:/Users/蔡昭誼/Desktop/專題/thief.xlsx",skip=1)
#Change data
ww2=data.frame(my_data2)
ww2=ww2[,3:4]
colnames(ww2)=c("Latitude","Longitude")
coordinates(ww2) <- ~ Longitude + Latitude
proj4string(ww2) <- CRS("+init=epsg:4326")
#Determine
qq2=over(state.merc,ww2,returnList = TRUE)
#Draw Plot
q2=as.data.frame(sapply(qq2, length))
my_data3 <- read_excel("C:/scooter.xlsx",skip=1)
#Change data
ww3=data.frame(my_data3)
ww3=ww3[,3:4]
colnames(ww3)=c("Latitude","Longitude")
coordinates(ww3) <- ~ Longitude + Latitude
proj4string(ww3) <- CRS("+init=epsg:4326")
#Determine
qq3=over(state.merc,ww3,returnList = TRUE)
#Draw Plot
q3=as.data.frame(sapply(qq3, length))
my_data4 <- read_excel("C:/rob.xlsx",skip=1)
#Change data
ww4=data.frame(my_data4)
ww4=ww4[,3:4]
colnames(ww4)=c("Latitude","Longitude")
coordinates(ww4) <- ~ Longitude + Latitude
proj4string(ww4) <- CRS("+init=epsg:4326")
#Determine
qq4=over(state.merc,ww4,returnList = TRUE)
#Draw Plot
q4=as.data.frame(sapply(qq4, length))
#Change data
ww1=data.frame(car)
ww1=ww1[,3:4]
colnames(ww1)=c("Latitude","Longitude")
coordinates(ww1) <- ~ Longitude + Latitude
proj4string(ww1) <- CRS("+init=epsg:4326")
state.merc <- spTransform(new, CRS=CRS("+init=epsg:4326"))
#Determine
qq5=over(state.merc,ww1,returnList = TRUE)
#Draw Plot
q5=as.data.frame(sapply(qq5, length))
#Change data
ww1=data.frame(bike)
ww1=ww1[,3:4]
colnames(ww1)=c("Latitude","Longitude")
coordinates(ww1) <- ~ Longitude + Latitude
proj4string(ww1) <- CRS("+init=epsg:4326")
state.merc <- spTransform(new, CRS=CRS("+init=epsg:4326"))
#Determine
qq6=over(state.merc,ww1,returnList = TRUE)
#Draw Plot(p-value< 2.2e-16)
q6=as.data.frame(sapply(qq6, length))
ty3=cbind(new,q1,q2,q3,q4,q5,q6)
ty3$summ=(ty3$sapply.qq..length.+ty3$sapply.qq2..length.+
           ty3$sapply.qq3..length.+ty3$sapply.qq4..length.+
           ty3$sapply.qq5..length.+ty3$sapply.qq6..length.)
ty3$den2=ty3$summ*(10^6)/ty3$AREA
ggplot(data =ty) +
  geom_sf(aes(fill = den)) +
  scale_fill_distiller(palette =  "Spectral",
                       direction = -1, name = "數量/平方公里") +
  labs(title="台北市竊盜案件密度圖")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())