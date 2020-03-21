#massimo perna
#Consorzio LaMMA
#
library(imager)
library(raster)
require(rgdal)
library(rgeos)
library(dplyr)
library(maptools)
#
#
#--------------------------------------------------
# VARIABILI DA INSERIRE:
#--------------------------------------------------
# percorso cartella di lavoro
mywd = "..."

#nome dell'immagine gia processata con magick
immagine = "img_truths_jpg.jpg"

#extent dell'immagine raster
xmin = .....
xmax = .....
ymin = .....
ymax = .....

# percorso delle verita a terra
url_truths = "..."

# nome dello shapefile (senza estensione) delle verita a terra
nome_truths = "..."
#--------------------------------------------------




#--------------------------------------------------
#leggo l'immagine relativa alle verità a terra in imager
#--------------------------------------------------
#la leggo in imager
im <- load.image(immagine)


im <- im %>% isoblur(1)

m_im <- as.matrix(im)

#salvo il blur  in raster
r <- raster(xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax, crs="+init=epsg:32632", resolution=0.5)
# N.B.:la matrice deve essere trasposta
r_im <- setValues(r, t(m_im))
#--------------------------------------------------
#
#
#
#--------------------------------------------------
# FACCIO UNA SORTA DI SUPERVISIONATA UTILIZZANTO DELLE VERITA A TERRA
# PRESE IN UN QUALUNUE RILIEVO GPS DELLA COSTA E LE UTILIZZO
# FACENDO LE MEDIE DEI VALORI ALL'INTERNO DI BUFFER SUI PUNTI GPS
# SELEZIONANDO I PUNTI MAGGIORI DI 0 COME QUELLI DI RUNUP
# E QUELLI MINORI DI 0 COME QUELLI DI STEP 
#--------------------------------------------------
# carico i punti GPS ("verità a terra")
truths <- readOGR(dsn = url_truths, layer = nome_truths)

#faccio il buffer per estrarre i valori del raster prossimi ai punti e poi fare la media
#va bene 50 cm altrimenti i buffer si sovrappongono
truths_buf <- gBuffer(truths,byid=T, width=0.5)


#salvo in shapefile
#writeOGR(obj=truths_buf , dsn=".", layer="truths_buf_spdf", driver="ESRI Shapefile", overwrite_layer = T)
#--------------------------------------------------




#--------------------------------------------------
# seleziono solo i buffer di step (quota sotto lo 0)
#--------------------------------------------------
truths_buf_st <- truths_buf[truths_buf$Z < 0,]

#sistema di riferimento di destinazione
utm_final <- "+init=epsg:32632 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs
+ellps=WGS84 +towgs84=0,0,0"

# riproietto per l'extract
truths_buf_st_proj <- spTransform(truths_buf_st, utm_final)

#media di ciascun poligono di step
media_st <- extract(r_im, truths_buf_st_proj, fun = mean, na.rm = TRUE)

#media delle medie dei poligoni di step, da utilizzare nel reclass
m_media_st <- mean(media_st)
#--------------------------------------------------



#--------------------------------------------------
# seleziono solo i buffer di runup (quota sopra lo 0)
#--------------------------------------------------
truths_buf_up <- truths_buf[truths_buf$Z > 0,]

# riproietto per l'extract
truths_buf_up_proj <- spTransform(truths_buf_up, utm_final)

#media di ciascun poligono di runup
media_up <- extract(r_im, truths_buf_up_proj, fun = mean, na.rm = TRUE)

#media delle medie dei poligoni di runup, da utilizzare nel reclass
m_media_up <- mean(media_up)
#--------------------------------------------------




#--------------------------------------------------
# salvo la matrice di riclassificazione che userò sulle altre immagini
# dello stesso tipo (pleiades)
#--------------------------------------------------
#matrice di riclassificazione
m <- c(0, m_media_st, 0,  m_media_st, m_media_up, 1,  m_media_up, 1, 0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

saveRDS(rclmat, "rclmat.rds")
#--------------------------------------------------

