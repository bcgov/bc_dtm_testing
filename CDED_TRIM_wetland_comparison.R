library(stars)
library(sf)
library(raster)
library(ggplot2)
library(bcmaps)

#BC Albers proj4 string
bcalb = '+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +datum=NAD83'

####Load wetland AOI####
wetland_aoi <- read_sf('WetlandLyrs/wetland_aoi.gpkg')

####GEDI (Does not intersect?)####

#bc_gedi <- readRDS('LiDAR/british_columbia_gedi.RDS')
#bc_gedi_sf = st_as_sf(bc_gedi, coords = c("longitude", "latitude"), crs = 4326)
#wetland_gedi = st_intersection(bc_gedi_sf,wetland_aoi)


####Load icesat-2 data, clip to wetland AOI####

##Already Run
#bc_icesat <- read_sf('LiDAR/bc_icesat2_heights_espg3005.gpkg')
#wetland_icesat <- st_intersection(bc_icesat,wetland_aoi)
#ggplot()+geom_sf(data=wetland_icesat)
#write_sf(wetland_icesat,'WetlandLyrs/icesat2_wetlandaoi.gpkg')
wetland_icesat <- read_sf('WetlandLyrs/icesat2_wetlandaoi.gpkg')

####TRIM DEM####
trim_dem_wetland <- raster('DTMs/TRIM_BC_DEM_25m.tif')
wetland_aoi <- st_transform(wetland_aoi,crs = st_crs(trim_dem_wetland))
trim_dem_wetland <- crop(trim_dem_wetland,wetland_aoi)
crs(trim_dem_wetland)

####CDED DEM####
cded_dem_wetland <- cded_raster(wetland_aoi)
crs(cded_dem_wetland)

####Project DEMs####

writeRaster(cded_dem_wetland,file.path(tempdir(),'cded_wetland_dem.tif'))
writeRaster(trim_dem_wetland,file.path(tempdir(),'trim_wetland_dem.tif'))

gdal_call <- paste("gdalwarp -t_srs ","'",bcalb,"'"," -r cubic ",file.path(tempdir(),'cded_wetland_dem.tif')," ",file.path(tempdir(),'cded_wetland_dem_bcalb.tif'),sep='')
system(gdal_call)
cded_dem_wetland<-raster(file.path(tempdir(),'cded_wetland_dem_bcalb.tif'))
cded_dem_wetland

gdal_call <- paste("gdalwarp -t_srs ","'",bcalb,"'"," -r cubic ",file.path(tempdir(),'trim_wetland_dem.tif')," ",file.path(tempdir(),'trim_wetland_dem_bcalb.tif'),sep='')
system(gdal_call)
trim_dem_wetland<-raster(file.path(tempdir(),'trim_wetland_dem_bcalb.tif'))
trim_dem_wetland


####Extract ICESAT locations####

#match extents for stacking 
extent(cded_dem_wetland)<-extent(trim_dem_wetland)

#Compute basic terrain derivatives 
cded_terrain <- terrain(cded_dem_wetland,c('slope','aspect'),'radians',8)
cded_northness<-cos(cded_terrain$aspect)
cded_eastness<-sin(cded_terrain$aspect)
cded_slope<-cded_terrain$slope

trim_terrain <- terrain(trim_dem_wetland,c('slope','aspect'),'radians',8)
trim_northness<-cos(trim_terrain$aspect)
trim_eastness<-sin(trim_terrain$aspect)
trim_slope<-trim_terrain$slope

#Stack raster layers
stacked <- stack(cded_dem_wetland,trim_dem_wetland,cded_northness,cded_eastness,cded_slope,trim_eastness,trim_northness,trim_slope)
plot(stacked)

#Intersect ICESAT pnts w/ DEMs
intersected<-st_as_sf(extract(stacked,wetland_icesat,sp=T))
intersected <- intersected[!is.na(intersected$h_te_mean),]
colnames(intersected)[8]<-'cded_northness'
colnames(intersected)[9]<-'cded_eastness'
colnames(intersected)[10]<-'cded_slope'
colnames(intersected)[11]<-'trim_eastness'
colnames(intersected)[12]<-'trim_northness'
colnames(intersected)[13]<-'trim_slope'


#Visualize results, compute quantile for residuals for CDED
png('Plots/Wetland_Icesat_vs_cded.png')
plot(intersected$h_te_mean,intersected$cded_wetland_dem_bcalb,pch=1,cex=.1,xlab="Mean Icesat-2 Elev. (m)", ylab="Mean CDED Elev. (m)",asp=1)
abline(a=0,b=1,col='green')
dev.off()
quantile(intersected$h_te_mean-intersected$cded_wetland_dem_bcalb)  
intersected$cded_resid <- intersected$h_te_mean-intersected$cded_wetland_dem_bcalb
ggplot()+geom_sf(data=intersected[intersected$cded_resid<500,],aes(color=cded_resid))

plot(intersected$h_te_mean,intersected$cded_resid)
plot(intersected$cded_northness,intersected$cded_resid)
plot(intersected$cded_eastness,intersected$cded_resid)
plot(intersected$cded_slope,intersected$cded_resid)

#Visualize results, compute quantile for residuals for TRIM
png('Plots/Wetland_Icesat_vs_trim.png')
plot(intersected$h_te_mean,intersected$trim_wetland_dem_bcalb,pch=1,cex=.1,xlab="Mean Icesat-2 Elev. (m)", ylab="Mean TRIM Elev. (m)",asp=1)
abline(a=0,b=1,col='green')
dev.off()
quantile(intersected$h_te_mean-intersected$trim_wetland_dem_bcalb)  
intersected$trim_resid <- intersected$h_te_mean-intersected$trim_wetland_dem_bcalb
ggplot()+geom_sf(data=intersected[intersected$trim_resid<500,],aes(color=trim_resid))

plot(intersected$h_te_mean,intersected$trim_resid)
plot(intersected$cded_northness,intersected$trim_resid)
plot(intersected$cded_eastness,intersected$trim_resid)
plot(intersected$cded_slope,intersected$trim_resid)


write_sf(intersected,'LiDAR/icesat_cded_trim_wetland.gpkg')


