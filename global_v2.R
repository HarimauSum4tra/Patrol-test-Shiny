
setwd("D:/OneDrive - Fauna & Flora/3. West Kalimantan Analysis/D1-Patrols SMART/1.Shinypatrol_wk")
load("datatoshiny.RData")
load("datatoshiny_v2.RData")


library(dplyr)
library(reshape2)
library(data.table)

# Menghitung dan mengubah data matrix-------------
library(r2d3) 
library(nycflights13)            

rplist <- list(raw_tabelrp)
unik_site <- unique(raw_tabelrp$Site)
unik_site$`All Site` <- 99
unik_Year <- unique(raw_tabelrp$Patrol.Year)
unik_Year$`All Year` <- 99
data_id <- tabelrp_cobs %>%
  mutate(ID = paste(Site,Year))%>%
  select(-Jarak)
dataraw_id_am <- raw_tabelam %>%
  mutate(ID = paste(Site, Year))

as.data.frame(dataraw_id_am)


#Wramgling dan set data untuk shiny
Setvarkor <- dataraw_id_am %>% dplyr::count(Year, Site, Kategori.temuan)
Varkor2am <- dcast( data = Setvarkor, formula = Site+Year ~ Kategori.temuan ,  value.var = 'n',  fun.aggregate = sum )  # get sum of V3 by grouping V1 and V2


c1 = Var_korelasi2%>%
  select(-Year,-Site, -total.kegiatan)%>%
  names()
c2 = Var_korelasi2 %>%
  names()
c3 = Var_korelasi2%>%
  select( Year, Site, total.kegiatan)%>%
  names()
c5 <- row.names.data.frame(dataraw_id_am$Kategori.temuan)
c6 <- Var_korelasi1%>%select(-Year)




Kategori.temuan.Name = raw_tabelam%>%
  select(Kategori.temuan)%>%
  names()
kate <- unique(raw_tabelam$Kategori.temuan)
kates <- as.data.frame(kate)
kt_maps <- raw_tabelam %>%
  select(Year, Month, Site, Kategori.temuan,X, Y, Jumlah.temuan)
kt_mapsname <- raw_tabelam %>%
  select(Year, Month, Site, Kategori.temuan,X, Y, Jumlah.temuan)%>%
  names()


#persiapan - Selectinput ID 
Transnam <- unique(tabelrp_cobs_type$Patrol.Transport.Type)
Sitename <- unique(tabelrp_cobs_type$Site)
my_df <- Var_korelasi2 %>% 
  select(-Site)


dft1 <- reshape2::dcast( data = tabelrp_cobs_type, formula = Year + Site ~ Patrol.Transport.Type,  value.var = 'Jarak',  fun.aggregate = sum )  # get sum of V3 by grouping V1 and V2

dft12 <- dft1 %>% select(-Site)
dft13 <- dft1 %>% select(-Year)


pvt <- dft12 %>%pivot_longer(-Year, names_to = "variable", values_to = "values")
pvt2 <- dft13 %>%pivot_longer(-Site, names_to = "variable", values_to = "values")


#Visualisasi data Map-----------------
#install.packages("rgdal", repos = "https://packagemanager.posit.co/cran/2023-06-01")
#install.packages("rJava",repos="http://cran.r-project.org")

#remotes::install_github('r-tmap/tmap')
# Instalasi dan memuat paket yang dibutuhkan
#install.packages("rgdal", repos = "https://packagemanager.posit.co/cran/2023-06-01")
