##### The script was written October 2018 and based on the files in the dropbox at the same time.
##### Changes may occur based on the files being changed in the folder

rm(list = ls())
#devtools::install_github('oswaldosantos/ggsn')
library(rdrop2)
library(tidyverse)
library(sf)
library(ggsn)
drop_auth()   ## Login to the PAWS dropbox folder

setwd('D:/github repos/mini---PAWS/')

hp <- st_read('/IND_adm1.shp')
hp <- hp %>% filter(NAME_1 == 'Himachal Pradesh')

sltheoretical <- st_read('HP_3200_5200UTM.shp') 

trap_info <- drop_search('trap_info  csv')
trap_info <- grep(unlist(lapply(lapply(trap_info$matches,'[[','metadata'),'[[','path_lower')), pattern = 'secr', value = T)
dat <- do.call(bind_rows,lapply(trap_info[1:6], drop_read_csv))
dat <- dat %>% dplyr::select(Area,Station,Longitude,Latitude)
colnames(dat) <- c('Region','Station','long','lat')

dat$Station <- trimws(dat$Station,which = 'both')

locations <- drop_search('species  csv')
locations <- grep(unlist(lapply(lapply(locations$matches,'[[','metadata'),'[[','path_lower')), pattern = 'secr', value = T)
species <- do.call(bind_rows,lapply(locations[1:6], drop_read_csv))
species$Station <- trimws(species$Station,'both')

species$Species[species$Species == 'Red fox'] <- 'Red Fox'

sp_sum <- species %>% group_by(Station,Species) %>% tally()

samp_cam <- expand.grid(Station = unique(species$Station), Species = unique(species$Species))

sp_sum <- left_join(samp_cam,sp_sum)

covered_regions <- na.omit(unique(right_join(dat,sp_sum) %>% dplyr::select(Region)))

##############
hp <- rbind(hp,hp,hp) ## Shouldn't be required. ggsn is acting strange with the new updated sf

species_capture_plot <- function(species_name){
  
  dat_sp <- sp_sum %>% filter(Species == species_name)
  
  dat_sp <- left_join(dat,dat_sp)
  
  dat_sp$n[dat_sp$Region %in% covered_regions$Region & is.na(dat_sp$n)] <- 0
  
  dat_sp <- st_as_sf(dat_sp, coords = c('long','lat'))

  st_crs(dat_sp) <- 4326
  st_crs(hp) <- 4326
  capture_plot <- ggplot() +
    geom_sf(data = hp, fill = 'grey70')+
    geom_sf(data = sltheoretical, fill = 'palegreen3', show.legend = F)+
    geom_sf(data = dat_sp[dat_sp$n == 0,], fill = 'white', shape = 21, show.legend = 'point',size =3)+
    geom_sf(data = dat_sp[!is.na(dat_sp$n)&dat_sp$n >0,], aes(fill = n), shape = 21, show.legend = 'point',size =3)+
    scale_fill_gradient(low = 'orange',high = 'red', name = paste(species_name,'Captures', sep = '\n'))+
    geom_sf(data = dat_sp[is.na(dat_sp$n),], shape = 21, show.legend = T,size =2, fill = 'grey30', alpha = 0.8)+
    geom_sf(data = hp, fill = NA, colour = 'black',size = 1)+
    north(hp,symbol = 12) +
    scalebar(hp, dist = 50,dd2km = T)+
    blank()
  
  png(paste0('Output/',species_name,'_capture.png'),width = 1000,height = 750, res = 100)

  print(capture_plot)
  
  dev.off()
  
}


species_capture_plot('Snow leopard')
species_capture_plot('Red Fox')
species_capture_plot('Stone Marten')
species_capture_plot('Brown Bear')
species_capture_plot('Dog')




###############################


sp_ung <- sp_sum %>% filter(Species %in% c('Ibex','Blue sheep'))

sp_ung <- sp_ung %>% group_by(Station) %>% filter(!is.na(n)) %>% summarise(ung = ifelse(n() == 2,'both',Species))

sp_ung <- left_join(dat,sp_ung)

sp_ung$ung[sp_ung$Region %in% unique(sp_ung$Region[!is.na(sp_ung$ung)]) & is.na(sp_ung$ung)] <- 'None'

sp_ung <- st_as_sf(sp_ung, coords = c('long','lat'))

st_crs(sp_ung) <- 4326

png('Output/ungulates.png',width = 1000,height = 750, res = 100)

ggplot() +
  geom_sf(data = hp, fill = 'grey70')+
  geom_sf(data = sltheoretical, fill = 'palegreen3', show.legend = F)+
  geom_sf(data = sp_ung[sp_ung$ung == 'None',], fill = 'white', shape = 21, show.legend = 'point', size = 3)+
  geom_sf(data = sp_ung[!is.na(sp_ung$ung)&sp_ung$ung != 'None',], aes(fill = ung), shape = 21, show.legend = 'point', size = 3)+
  scale_fill_manual(values = c('blue','red'), name = 'Ungulates')+
  geom_sf(data = sp_ung[is.na(sp_ung$ung),], shape = 21,size =2, fill = 'grey30', alpha = 0.8)+
  geom_sf(data = hp, fill = NA, colour = 'black',size = 1)+
  north(hp,symbol = 12) +
  scalebar(hp,dd2km = T, dist = 50) +
  blank()

dev.off()
