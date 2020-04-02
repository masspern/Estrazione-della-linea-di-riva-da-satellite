
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

#nome dell'immagine multispettrale pleiades clippata
pleiades_clip = "...."


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
# Lavoro sull'immagine di clip per importarla in imager
#--------------------------------------------------
setwd(mywd)

# carico l'immagine pleiades clippata
ras16b_br <- brick(pleiades_clip)

#trovo i minimi e i massimi della banda di infrarosso
minimo = minValue(ras16b_br[[4]])
massimo = maxValue(ras16b_br[[4]])

# trasformo in 8bit
ras8b_ref <- calc(ras16b_br[[4]], fun=function(x){((x - minimo) * 255)/(massimo - minimo) + 0})

#export 8b raster
writeRaster(ras8b_ref, 'ras8b_ref.tif', datatype='INT1U', overwrite=T)


r_im <- ras8b_ref
#--------------------------------------------------




#--------------------------------------------------
# FACCIO UNA SORTA DI SUPERVISIONATA UTILIZZANTO DELLE VERITA A TERRA
# PRESE IN UN QUALUNUE RILIEVO GPS DELLA COSTA E LE UTILIZZO
# FACENDO LE MEDIE DEI VALORI ALL'INTERNO DI BUFFER SUI PUNTI GPS
# SELEZIONANDO I PUNTI CON QUOTA MAGGIORE DI 0 COME QUELLI DI RUNUP
# E QUELLICON QUOTA MINORE DI 0 COME QUELLI DI STEP 
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
# salvo la matrice di riclassificazione che userò nelle altre zone
# da analizzare
#--------------------------------------------------
#matrice di riclassificazione
m <- c(0, m_media_st, 0,  m_media_st, m_media_up, 1,  m_media_up,255, 0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

saveRDS(rclmat, "rclmat.rds")
#--------------------------------------------------

#EOF
