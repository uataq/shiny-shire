library(ncdf4)
library(RColorBrewer)
library(raster)
library(rgdal)
library("M3")
library(ggmap)
library(ggplot2)

# setwd("C:/Users/Aims/Desktop/Education/Ph.D/Campaigns/Utah2017/Data/STILT/UDAQ_emission_inventories/run_feb2016_4km_3nov2016/Ncf")
#setwd("C:/Users/Aims/Desktop/Education/Ph.D/Campaigns/Utah2017/Data/STILT/UDAQ_emission_inventories/run_feb2016_1-33km_3nov2016")

#Open the ncf file
# ncopen=nc_open("2016020107.1.DM2.feb2016_4km_3nov2016.ncf")
ncin=nc_open("raw/egts_l.20160201.1.DM3.feb2016_1-33km_3nov2016.ncf")

#Obtain the variable as an array 
# NH3=ncvar_get(ncopen,"NH3")
# SO2=ncvar_get(ncopen,"SO2")
# NO=ncvar_get(ncopen,"NO")
NH3=ncvar_get(ncin,"NH3")
SO2=ncvar_get(ncin,"SO2")
NO=ncvar_get(ncin,"NO")

#Conversion of mol/h to umol/m^2*s^1
#NH3_flux=NH3*(10^6)*(1/1776889)*(1/3600)   #for countywide gridspacing 1.333x1.333 km
NH3_flux=NH3*(10^6)*(1/16000000)*(1/3600)   #for statewide gridspacing 4x4 km

#Obtain grid information from netcdf
gridinfo=get.grid.info.M3("raw/egts_l.20160201.1.DM3.feb2016_1-33km_3nov2016.ncf")
#gridinfo=get.grid.info.M3("egts_l.20160201.1.DM3.feb2016_1-33km_3nov2016.ncf")

#Obtain projection information from netcdf
# get.proj.info.M3("2016020107.1.DM2.feb2016_4km_3nov2016.ncf")
#get.proj.info.M3("egts_l.20160201.1.DM3.feb2016_1-33km_3nov2016.ncf")

#obtain the extent information from gridinfo
xmn=gridinfo$x.orig
xmx=gridinfo$x.orig+(gridinfo$x.cell.width*gridinfo$nrows)
ymn=gridinfo$y.orig
ymx=gridinfo$y.orig+(gridinfo$y.cell.width*gridinfo$ncols)

#Define the new projection
newproj="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#Create a raster using the array of species, the extent and projection info
#Create a new raster(pr3) with the extent of our previous raster and new projection
NH3raster<- raster(NH3_flux[,,1],xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,crs="+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000")
pr3 <- projectExtent(NH3raster, newproj)

# Set the cell size, decreasing resolution averages the combines cell values.
res(pr3) <- 0.04    #resolution of the DAQ inventories 4x4 km scale
res(pr3) <- 0.01    
res(pr3) <- 0.1     #resolution of footprints calculated using STILT

#Log of NH3_flux values and generating a replacement raster (tranposed and flipped to correction for lat/lon position) to project
NH3_log <- log10(NH3_flux)
NH3raster_log <- raster(t(NH3_log[,,1]),xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,crs="+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000")
NH3raster_log_f <- flip(NH3raster_log, direction = 'y')

#Project the raster into the newprojection and plot
pr2 <- projectRaster(NH3raster,pr3,method="ngb")
pr3 <- projectRaster(NH3raster_log_f,pr3,method="ngb")
# pr3 <- projectRaster(NOraster_log_f,pr3,method="ngb")

#shifting DAQ lat/lon to southwest corner/matching footprint coordinates
pr2_corr<- shift(pr2,x=-0.0294,y=-0.12385)   
# pr3_corr<- shift(pr3,x=-0.0294,y=-0.12385)    

plot(pr2_corr, xlab="Long", ylab="Lat",col=rev(brewer.pal(11,"RdYlBu")),zlim=c(0,9),legend.lab = expression(paste(mu,"mol ", m^-2, s^-1)))
map("state",add=TRUE)