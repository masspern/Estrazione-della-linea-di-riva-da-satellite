#
#massimo perna
#Consorzio LaMMA
#
#ref:

# https://dahtah.github.io/imager/imager.html
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html#raster_images
# 



library(imager)
library(magick)
library(raster)
#library(scales)
require(rgdal)
library(rgeos)
library(princurve)
library(dplyr)
#library(spatstat)  # to calculate field of point density
library(dismo)


# nella cartella di lavoro deve essere presente il clip dell'immagine
# pleiades clippata nell'area di battigia e il file rds di riclassificazione
# che deriva dall'analisi delle verità a terra acquisite con il gps


#--------------------------------------------------
# VARIABILI DA INSERIRE:
#--------------------------------------------------
# percorso cartella di lavoro
mywd = "..."

#nome dell'immagine multispettrale pleiades clippata
pleiades_clip = "..."

#extent dell'immagine raster
xmin = .....
xmax = .....
ymin = .....
ymax = .....
#--------------------------------------------------



#--------------------------------------------------
# Lavoro sull'immagine di clip per importarla in imager
#--------------------------------------------------
setwd(mywd)

# carico l'immagine pleiades clippata
ras16b_br <- brick(pleiades_clip)

#trovo i minimi e i massimi della banda di infrarosso (banda 4)
minimo = minValue(ras16b_br[[4]])
massimo = maxValue(ras16b_br[[4]])

# trasformo in 8bit
ras8b <- calc(ras16b_br[[4]], fun=function(x){((x - minimo) * 255)/(massimo - minimo) + 0})

#esporto l'8bit
writeRaster(ras8b, 'ras8b.tif', datatype='INT1U', overwrite=T)

# lo ricarico in image magick
img_tif =  image_read("ras8b.tif")

#lo converto in jpg
img_jpg <- image_convert(img_tif, "jpg")

#e lo salvo
image_write(img_jpg, path = "img_jpg.jpg", format = "jpg")



#--------------------------------------------------
# carico l'immagine, processata con MAGICK, in IMAGER
#--------------------------------------------------


# leggo l'immagine processata già con image magick (vedi sopra)...
im <- load.image("img_jpg.jpg")

# faccio un blurring
im <- im %>% isoblur(1)

# trasformo in matrice
m_im <- as.matrix(im)

#salvo il blur in raster 
r <- raster(xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax, crs="+init=epsg:32632", resolution=0.5)

# N.B.:la matrice deve essere trasposta
r_im <- setValues(r, t(m_im))


#e lo salvo....
writeRaster(r_im, filename="test_blur.tif", format="GTiff", overwrite=TRUE)
#------------------------------------------------------------





#------------------------------------------------------------
# riclassifico utilizzando le verità a terra 
#------------------------------------------------------------

# matrice di riclassificazione dell'anno in corso 
# ottenuta dai rilievi GPS in una zona (verità a terra)
rclmat = readRDS("rclmat_2019.rds")


#riclassifico..... 
rc_1 <- reclassify(r_im, rclmat)

#plot(rc_1)
#--------------------------------------------------




#--------------------------------------------------
# provo a vettorializzare (punti)
#--------------------------------------------------
#converto gli 0 in NA per la vettorializzazione
rc_1[rc_1 == 0] <- NA

# raster to vector (points)
rc_point = rasterToPoints(rc_1, spatial = TRUE)

#plot(rc_point)
#--------------------------------------------------




#--------------------------------------------------
# creo un buffer per la pulizia dei punti che non sono 
# sulla battigia- considero un metro...
#--------------------------------------------------
cleaner <- gBuffer(rc_point, width=1.0, byid=T)
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

#seleziono solo i poligoni grandi
cleaner_spdf <- cleaner_spdf[cleaner_spdf$AREA > 150,]

#salvo
#writeOGR(obj=cleaner_spdf , dsn=".", layer="cleaner", driver="ESRI Shapefile", overwrite_layer = T)
#--------------------------------------------------




#--------------------------------------------------
# pulisco
#--------------------------------------------------
#seleziono i punti all'interno del buffer
rc_point_ok <- rc_point[complete.cases(over(rc_point, cleaner_spdf)),]
plot(rc_point_ok)


#salvo in shapefile
#writeOGR(obj=rc_point_ok, dsn=".", layer="rc_point_ok", driver="ESRI Shapefile", overwrite_layer = T)
#--------------------------------------------------




#--------------------------------------------------
#faccio la linea di riva (spline)
#--------------------------------------------------
#trovo le coordinate
mp <- coordinates(rc_point_ok)

sp_line <- smooth.spline(mp[,1] ~  mp[,2], spar=0.3)
#○pr_curve <- principal_curve(mp, smoother = "lowess")
#lines(pr_curve, lwd=1.25, col="red")

# n.b. quabdo creo la matrice di coordinate devo mettere prima la y e poi la x (in smooth.spline ho fatto il contrario...)
coors <- matrix(c(sp_line$y,sp_line$x),nrow=length(sp_line$x))

#creo uno SpatialLinesDataFrame che continene le lunghezze delle curve 
Sl1 <- Line(coors)
S1 <- Lines(list(Sl1), ID = "a")
Sl <- SpatialLines(list(S1))
df <- data.frame(len = sapply(1:length(Sl), function(i) gLength(Sl[i, ])))
rownames(df) <- sapply(1:length(Sl), function(i) Sl@lines[[i]]@ID)
Sldf <- SpatialLinesDataFrame(Sl, data = df)

plot(Sldf)

#salvo test reclass
writeOGR(obj=Sldf, dsn=".", layer="linea_riva", driver="ESRI Shapefile", overwrite_layer = T)
#--------------------------------------------------


#EOF
