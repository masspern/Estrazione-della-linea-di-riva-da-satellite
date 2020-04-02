#
#massimo perna
#Consorzio LaMMA
#
library(magick)
library(raster)
#library(scales)
require(rgdal)
library(rgeos)
library(princurve)
library(dplyr)
library(dismo)


# nella cartella di lavoro deve essere presente il clip dell'immagine
# pleiades clippata nell'area di battigia e il file rds di riclassificazione
# che deriva dall'analisi delle verità a terra acquisite con il gps


#------------------------------------------------------------
# VARIABILI DA INSERIRE:
#------------------------------------------------------------


# percorso cartella di lavoro

mywd = "..."


# nome delle immagini multispettrali pleiades 16bit clippate
# con un buffer di 7/8 metri intorno ad una vecchia linea di riva

# immagine da analizzare
img_1 = "...."

#------------------------------------------------------------



#------------------------------------------------------------
# Lavoro sull'immagine di clip per salvarla 8bit
#------------------------------------------------------------
setwd(mywd)

# carico l'immagine pleiades clippata
ras16b_br <- brick(img_1)


#trovo i minimi e i massimi della banda di infrarosso (banda 4)
minimo = minValue(ras16b_br[[4]])
massimo = maxValue(ras16b_br[[4]])

# trasformo in 8bit
ras8b_1 <- calc(ras16b_br[[4]], fun=function(x){((x - minimo) * 255)/(massimo - minimo) + 0})


#esporto l'8bit
writeRaster(ras8b_1, 'ras8b_1.tif', datatype='INT1U', overwrite=T)

#------------------------------------------------------------





#------------------------------------------------------------
# PROVO A NON FARE IL BLUR e utilizzo nell'analisi direttamente
# l'immagine a 8bit
# modifico la matrice di ricodifica in base all'indice 
# ricavato dal rapporto tra le spiagge asciutte nelle due aree
#------------------------------------------------------------
r_im = ras8b_1

# dopo aver fatto girare "indice_spiagge.r"
# con la stessa wd...
ind_sa = readRDS("ind_sa.rds")


# carico la matrice di ricodifica realizzata
# con i punti del riilievo GPS nell'area di riferimento (codice ground_truths)
# identica wd...

rclmat = readRDS("rclmat.rds")

# modifico in base all'indice (vedi sopra)
rclmat <-  rclmat * ind_sa

# correggo i valori modificati impropriamente dal rapporto...
rclmat[2,3] = 1 
rclmat[3,2] = 255
#------------------------------------------------------------



#------------------------------------------------------------
# ricodifico l'immagine utilizzando le verità a terra corrette
#------------------------------------------------------------

#ricodifico..... 
rc_1 <- reclassify(r_im, rclmat)
#plot(rc_1)
#------------------------------------------------------------




#------------------------------------------------------------
# provo a vettorializzare (punti)
#------------------------------------------------------------
#converto gli 0 in NA per la vettorializzazione
rc_1[rc_1 == 0] <- NA

# raster to vector (points)
rc_point = rasterToPoints(rc_1, spatial = TRUE)

#plot(rc_point)
#writeOGR(obj=rc_point , dsn=".", layer="rc_point", driver="ESRI Shapefile", overwrite_layer = T)
#------------------------------------------------------------




#------------------------------------------------------------
# creo un buffer per la pulizia dei punti che non sono 
# sulla battigia- considero due metri... ma vanno fatti dei test...
#------------------------------------------------------------
cleaner <- gBuffer(rc_point, width=2, byid=T)
#plot(cleaner)

#dissolvo
cleaner<- gUnaryUnion(cleaner)

#creo lo spatialpolygondataframe
p.df <- data.frame( ID=1:length(cleaner))
cleaner_spdf <- SpatialPolygonsDataFrame(cleaner, p.df)

#multipart to singlepart
cleaner_spdf <- disaggregate(cleaner_spdf)

#calcolo l'area in un attributo
cleaner_spdf@data$AREA <- gArea(cleaner_spdf,byid=TRUE)

#seleziono solo i poligoni grandi (anche qui posso fare dei test...)
cleaner_spdf <- cleaner_spdf[cleaner_spdf$AREA > 150,]

#salvo
#writeOGR(obj=cleaner_spdf , dsn=".", layer="cleaner", driver="ESRI Shapefile", overwrite_layer = T)
#------------------------------------------------------------




#------------------------------------------------------------
# pulisco
#------------------------------------------------------------
#seleziono i punti all'interno del buffer
rc_point_ok <- rc_point[complete.cases(over(rc_point, cleaner_spdf)),]
plot(rc_point_ok)


#salvo in shapefile
writeOGR(obj=rc_point_ok, dsn=".", layer="rc_point_ok_s_vinc_rec_ombrone_max_min_svinc", driver="ESRI Shapefile", overwrite_layer = T)
#------------------------------------------------------------




#------------------------------------------------------------
# creo la linea di riva (spline)
#------------------------------------------------------------
# trovo le coordinate
mp <- coordinates(rc_point_ok)

sp_line <- smooth.spline(mp[,1] ~  mp[,2], spar=0.3)
# ○pr_curve <- principal_curve(mp, smoother = "lowess")
# lines(pr_curve, lwd=1.25, col="red")

# n.b. quabdo creo la matrice di coordinate devo mettere prima la y e poi la x (in smooth.spline ho fatto il contrario...)
coors <- matrix(c(sp_line$y,sp_line$x),nrow=length(sp_line$x))

# creo uno SpatialLinesDataFrame che continene le lunghezze delle curve 
Sl1 <- Line(coors)
S1 <- Lines(list(Sl1), ID = "a")
Sl <- SpatialLines(list(S1))
df <- data.frame(len = sapply(1:length(Sl), function(i) gLength(Sl[i, ])))
rownames(df) <- sapply(1:length(Sl), function(i) Sl@lines[[i]]@ID)
Sldf <- SpatialLinesDataFrame(Sl, data = df)

plot(Sldf)

# salvo test reclass
writeOGR(obj=Sldf, dsn=".", layer="linea_riva", driver="ESRI Shapefile", overwrite_layer = T)
#------------------------------------------------------------


#EOF
