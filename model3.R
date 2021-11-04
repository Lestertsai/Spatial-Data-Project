#https://www.paulamoraga.com/book-geospatial/sec-arealdatatheory.html
#https://spatialanalysis.github.io/lab_tutorials/Spatial_Weights_as_Distance_Functions.html
a=as.data.frame(cbind(CODE2=ty$CODE2,rate=ty$rate,
                      oldrate=ty$oldrate,ygrate=ty$ygrate,
                      CNT=hos$H_CNT,BED=hos$H_BED,
                      SRVP=hos$H_SRVP,SRVB=hos$H_SRVB,AREA=ty$AREA))
b=as.data.frame(cbind(CODE2=styg$CODE2,strate=styg$strate))
c=as.data.frame(cbind(CODE2=fundyg$CODE2,rateC=fundyg$rate,q))
d=as.data.frame(cbind(CODE2=ty2$CODE2,den=ty2$den))
e=as.data.frame(cbind(CODE2=ty2$CODE2,den2=ty3$den2))
final= Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "CODE2", all.x = TRUE),
              list(a,b,c,d,e))
x=c("readr","rgeos","sp","rgdal","readxl","sf","ggplot2",
    "spdep","INLA","gridExtra")
lapply(x, require, character.only = TRUE)
final1=read_excel('C:/Users/蔡昭誼/Desktop/finalNA.xlsx')
final1=read.xlsx('C:/Users/蔡昭誼/Desktop/finalNA.xlsx')
final[,-1]=scale(final[,-1])
new1=cbind(new,final1)
new1$idareav <- 1:nrow(new1@data)
new1$idareau <- 1:nrow(new1@data)
#建立矩陣
coords <- cbind(new1$X,new1$Y)
knn1 <- knearneigh(coords)
k1 <- knn2nb(knn1)
critical.threshold <- max(unlist(nbdists(k1,coords)))
nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
distances <- nbdists(nb.dist.band,coords)
invd1a <- lapply(distances, function(x) (1/(x/100)))
invd.weights <- nb2listw(nb.dist.band,glist = invd1a,style = "B")
nb2INLA("map.adj", invd.weights$neighbours)
g <- inla.read.graph(filename = "map.adj")
f = formula(den2~rate*oldrate*ygrate*CNT*BED*SRVP*SRVB*
              strate*rateC*sch*den)
terms = attr(terms.formula(f), "term.labels")
#包含interaction
f = as.formula(sprintf("den2 ~%s+f(idarea, model = 'bym2', graph = g, hyper = prior)",
                       paste(terms[1:66], collapse="+")))
#BYM Model
formula1=den2~rate+oldrate+ygrate+strate+den+rateC+sch+
  CNT+BED+SRVP+SRVB+f(idareau, model = "besag", graph = g, scale.model = TRUE) +
  f(idareav, model = "iid")
f1 = as.formula(sprintf("den2 ~%s+
    f(idareau, model = 'besag', graph = g, scale.model = TRUE) +
  f(idareav, model = 'iid')",paste(terms[1:66], collapse="+")))
res1 <- inla(f1,data = new1@data,
            control.predictor = list(compute = TRUE),
            control.family = list(link = "logit"),
            control.compute = list(cpo = TRUE,
                                   dic = TRUE, waic = TRUE))
summary(res1)
res2 <- inla(formula1,data = new1@data,
             control.predictor = list(compute = TRUE),
             control.family = list(link = "logit"),
             control.compute = list(cpo = TRUE,
                                    dic = TRUE, waic = TRUE))
summary(res2)
#BYM2 Model
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)),
  phi = list(
    prior = "pc",
    param = c(0.5, 2 / 3))
)
formula2=den2~rate+oldrate+ygrate+strate+den+rateC+sch+
  CNT+BED+SRVP+SRVB+
  f(idareav, model = "bym2", graph = g, hyper = prior)
res <- inla(f,data = new1@data,
            control.predictor = list(compute = TRUE),
            control.family = list(link = "logit"),
            control.compute = list(cpo = TRUE,
                                   dic = TRUE, waic = TRUE))
summary(res)
res3 <- inla(formula2,data = new1@data,
             control.predictor = list(compute = TRUE),
             control.family = list(link = "logit"),
             control.compute = list(cpo = TRUE,
                                    dic = TRUE, waic = TRUE))
summary(res3)
head(res$summary.fitted.values)
#map response variable(BYM2 Model only)
new1$`Crime Rate` <- res3$summary.fitted.values[, "mean"]
mapsf <- st_as_sf(new1)
gRR1 <- ggplot(mapsf) + geom_sf(aes(fill = `Crime Rate`)) +
  scale_fill_gradient2(
    midpoint = 0.5, low = "blue", high = "red",
    limits = c(0,1)
  ) +theme_bw()+ ggtitle('BYM2 Model')
plot(gRR1)
#map response variable(BYM Model+interactions)
new1$`Crime Rate` <- res1$summary.fitted.values[, "mean"]
mapsf <- st_as_sf(new1)
gRR2 <- ggplot(mapsf) + geom_sf(aes(fill = `Crime Rate`)) +
  scale_fill_gradient2(
    midpoint = 0.5, low = "blue", high = "red",
    limits = c(0,1)
  ) +theme_bw()+ ggtitle('BYM Model with Interactions')
plot(gRR2)
#map response variable(BYM Model only)
new1$`Crime Rate` <- res2$summary.fitted.values[, "mean"]
mapsf <- st_as_sf(new1)
gRR3 <- ggplot(mapsf) + geom_sf(aes(fill = `Crime Rate`)) +
  scale_fill_gradient2(
    midpoint = 0.5, low = "blue", high = "red",
    limits = c(0,1)
  ) +theme_bw()+ ggtitle('BYM Model')
plot(gRR3)
#map response variable(BYM2 Model+interactions)
new1$`Crime Rate` <- res$summary.fitted.values[, "mean"]
mapsf <- st_as_sf(new1)
gRR4 <- ggplot(mapsf) + geom_sf(aes(fill = `Crime Rate`)) +
  scale_fill_gradient2(
    midpoint = 0.5, low = "blue", high = "red",
    limits = c(0,1)
  ) +theme_bw()+ ggtitle('BYM2 Model with Interactions')
plot(gRR4)
grid.arrange(gRR3,gRR2,gRR1,gRR4,ncol=2,
             top = "Mean of Fitted Response in Taipei City")
#posterior mean of the BYM2 random effect 
mapsf$`Random Effect3` <- res3$summary.random$idareav[1:907, "mean"]
ggplot(mapsf) + geom_sf(aes(fill = `Random Effect3`)) +
  scale_fill_gradient2(
    midpoint = median(mapsf$`Random Effect3` ),
    low = "blue", high = "red"
  ) +theme_bw()+ ggtitle('Posterior Mean of the Random Effect')
mapsf$`Random Effectv2` <- res2$summary.random$idareav[1:907, "mean"]
ggplot(mapsf) + geom_sf(aes(fill = `Random Effectv2`)) +
  scale_fill_gradient2(
    midpoint = median(mapsf$`Random Effectv2` ),
    low = "blue", high = "red"
  ) +theme_bw()+ ggtitle('Posterior Mean of the Random Effect')
mapsf$`Random Effectu2` <- res2$summary.random$idareau[1:907, "mean"]
ggplot(mapsf) + geom_sf(aes(fill = `Random Effectu2`)) +
  scale_fill_gradient2(
    midpoint = median(mapsf$`Random Effectu2` ),
    low = "blue", high = "red"
  ) +theme_bw()+ ggtitle('Posterior Mean of the Random Effect')
mapsf$`Random Effect` <- res$summary.random$idarea[1:907, "mean"]
ggplot(mapsf) + geom_sf(aes(fill = `Random Effect`)) +
  scale_fill_gradient2(
    midpoint = median(mapsf$`Random Effectu2` ),
    low = "blue", high = "red"
  ) +theme_bw()+ ggtitle('Posterior Mean of the Random Effect')
#Moran's test
#Moran's test
moran.test(mapsf$`Random Effect3`,invd.weights,
           alternative = "two.sided")
moran.test(mapsf$`Random Effect`,invd.weights,
           alternative = "two.sided")
#Detection
par(mfrow=c(2,2))
qqnorm(mapsf$`Random Effect3`,main = "BYM2 Model")
qqline(mapsf$`Random Effect3`)
qqnorm(mapsf$`Random Effect`,main = "BYM2 Model with Interactions")
qqline(mapsf$`Random Effect`)
hist(mapsf$`Random Effect3`,freq = FALSE, breaks = 20,main = "",
     xlab='Random Effect of BYM2 Model')
hist(mapsf$`Random Effect`,freq = FALSE, breaks = 20,main = "",
     xlab='Random Effectn of \n BYM2 Model with Interactions')
par(mfrow=c(1,1))
plot(res$summary.fitted.values[,1],new1$den2,ylim=c(-1.5,4),
     xlim=c(0,1),xlab="Fitted Value", ylab="Observed Value")
abline(lm(new1$den2 ~res$summary.fitted.values[,1]))