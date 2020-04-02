#Massimo Perna
#Consorzio LaMMA

library(raster)



#--------------------------------------------------
# VARIABILI DA INSERIRE:
#--------------------------------------------------

# percorso della directory di lavoro
mywd("...")

# shapefiles di aree di spiaggia asciutta
shp_1 = "..."
shp_ref = "..."


# nome delle immagini multispettrali pleiades 16bit clippate
# con un buffer di 7/8 metri intorno ad una vecchia linea di riva

# immagine da analizzare
img_1 = "...."

# immagine di reference (dove ho i punti del rilievo GPS)
img_ref = "...."

#--------------------------------------------------





#--------------------------------------------------
# rapporto tra media pixelvalue immagine di reference e immagine da elaborare
# basato su aree di spiaggia asciutta (pseudo-invariant feature)
#--------------------------------------------------

setwd(mywd)

# in ambiente GIS creo due shapefiles che insistono su aree di spiaggia asciutta
# sgombre da ombre e/o oggetti

# SHP spiaggia asciutta dell'area da elaborare
sa_1 <- readOGR(dsn = ".", layer = ".....")

# SHP spiaggia asciutta dell'area di riferimento
sa_ref <- readOGR(dsn = ".", layer = ".....")


# carico le due immagini
r_1 <- raster(img_1)
r_ref <- raster(img_ref)


#media di ciascun poligono di spiaggia asciutta
media_1 <- extract(r_1, sa_1, fun = mean, na.rm = TRUE)


#media di ciascun poligono di spiaggia asciutta
media_ref <- extract(r_omb, sa_ref, fun = mean, na.rm = TRUE)

media_1_tot <- mean(media_1)
media_ref_tot <- mean(media_ref)

ind_sa <- media_1_tot/media_ref_tot

saveRDS(ind_sa, "ind_sa.rds")

#EOF
