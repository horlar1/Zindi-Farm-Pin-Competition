options(warn = -1)

library(plyr)
library(tidyverse)
library(raster)
library(sp)
library(sf)
library(rgdal)
library(rgeos)
library(velox)
library(celestial)
library(caret)
library(fastICA)
library(SOAR)
library(RStoolbox)
library(spdep)


source("utils.R")
#####
path.dir = getwd()
data.dir = paste0(path.dir,"/Data")

img.dir = paste0(data.dir,"/S2A_MSIL1C_20170101T082332_N0204_R121_T34JEP_20170101T084543.SAFE/GRANULE/L1C_T34JEP_A007983_20170101T084543/IMG_DATA")

img.dir3 = paste0(data.dir,"/S2A_MSIL1C_20170312T082001_N0204_R121_T34JEP_20170312T084235.SAFE/GRANULE/L1C_T34JEP_A008984_20170312T084235/IMG_DATA")

img.dir6 = paste0(data.dir,"/S2A_MSIL1C_20170131T082151_N0204_R121_T34JEP_20170131T084118.SAFE/GRANULE/L1C_T34JEP_A008412_20170131T084118/IMG_DATA")


img.dir4 = paste0(data.dir,"/S2B_MSIL1C_20170715T081609_N0205_R121_T34JEP_20170715T084650.SAFE/GRANULE/L1C_T34JEP_A001863_20170715T084650/IMG_DATA")


img.dir42 = paste0(data.dir,"/S2A_MSIL1C_20170710T082011_N0205_R121_T34JEP_20170710T084244.SAFE/GRANULE/L1C_T34JEP_A010700_20170710T084244/IMG_DATA")

img.dir7 = paste0(data.dir,"/S2B_MSIL1C_20170804T081559_N0205_R121_T34JEP_20170804T084631.SAFE/GRANULE/L1C_T34JEP_A002149_20170804T084631/IMG_DATA")

img.dir8 = paste0(data.dir,"/S2A_MSIL1C_20170819T082011_N0205_R121_T34JEP_20170819T084427.SAFE/GRANULE/L1C_T34JEP_A011272_20170819T084427/IMG_DATA")

img.dir5 = paste0(data.dir,"/S2A_MSIL1C_20170531T082011_N0205_R121_T34JEP_20170531T084246.SAFE/GRANULE/L1C_T34JEP_A010128_20170531T084246/IMG_DATA")


img.dir2 = paste0(data.dir,"/S2A_MSIL1C_20170210T082051_N0204_R121_T34JEP_20170210T083752.SAFE/GRANULE/L1C_T34JEP_A008555_20170210T083752/IMG_DATA")

img.dir66 = paste0(data.dir,"/S2A_MSIL1C_20170620T082011_N0205_R121_T34JEP_20170620T084200.SAFE/GRANULE/L1C_T34JEP_A010414_20170620T084200/IMG_DATA")

img.dir1 = paste0(data.dir,"/S2A_MSIL1C_20170322T081611_N0204_R121_T34JEP_20170322T084728.SAFE/GRANULE/L1C_T34JEP_A009127_20170322T084728/IMG_DATA")

#######
#dir.create(paste0(path.dir,"/tmp"))
save.files.dir = paste0(path.dir,"/tmp")



## sf
# shp = readOGR(paste0(data.dir,"/train2-fixup/new.shp"))
# 
# #### sp
# sh = readOGR(paste0(data.dir,"/train/train.shp"),stringsAsFactors = F,dropNULLGeometries = F)
#######
shp2 =  readOGR(paste0(data.dir,"/train/train.shp"),stringsAsFactors = F)
sh_trans <- spTransform(shp2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
df = cbind(shp2 %>% as.data.frame(),coordinates(sh_trans))

colnames(df) = c("Field_Id","Area","Subregion","Crop_Id_Ne","lon","lat")

m = df %>% group_by(Crop_Id_Ne) %>% summarise(mlo =mean(lon),lm = mean(lat))

test = readOGR(paste0(data.dir,"/test/test.shp"),stringsAsFactors = F)
test_trans = spTransform(test,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
test = cbind(test %>% as.data.frame(),coordinates(test_trans))
colnames(test) = c("Field_Id","Area","Subregion","lon","lat")


label = as.numeric(df$Crop_Id_Ne)
# library(CatEncoders)
# label_enc= LabelEncoder.fit(as.factor(label))
# label2 = transform(label_enc,as.factor(label))

train.id = df$Field_Id
test.id = test$Field_Id
df = df %>% within(rm("Crop_Id_Ne","Field_Id"))
test = test %>% within(rm("Field_Id"))
df = rbind(df,test)


########
#@@@@  TODO re-arrange by field id
########

df = df %>% 
  mutate(
    #lat_deg = as.numeric(deg2dms(lat)[,1]),
    #lon_deg = as.numeric(deg2dms(lon)[,1]),
    latmins = as.numeric(deg2dms(lat)[,2])*60,
    lonmins = as.numeric(deg2dms(lon)[,2])*60
  ) %>% 
  add_count(latmins) %>% 
  rename("lat_cnt" = n) %>% 
  add_count(lonmins) %>% 
  rename("lon_cnt" = n) %>% 
  add_count(latmins,lonmins) %>% 
  rename("lat_lon_cnt" = n) %>% 
  ##############
  ## add freq encoding 
  #################
  # group_by(lonmins,latmins) %>% 
  # mutate(m_area = median(Area),
  #        min = min(Area),
  #        max = max(Area)) %>% 
  #ungroup() %>% 
  # group_by(Subregion) %>% 
  # mutate(mean_area = median(Area),
  #        min_area = min(Area),
  #        max_area = max(Area))

dr.dat = cbind(lon=df$lon,lat=df$lat) %>% as.data.frame()
transform = preProcess(dr.dat, method = c("center","scale","pca"))
pc = predict(transform, dr.dat) %>% as.data.frame()
df$lat_pca = pc$PC1
df$lon_pca = pc$PC2
############
transform = preProcess(dr.dat, method = c("center","scale","ica"),n.comp=2)
ic = predict(transform, dr.dat) %>% as.data.frame()
df$lat_ica = ic$ICA1
df$lon_ica = ic$ICA2
###########
transform = preProcess(dr.dat, method = c("center","scale"))
pc = predict(transform, dr.dat) %>% as.data.frame()
df$lat_scale = pc$lat
df$lon_scale = pc$lon

df$lat1 = cos(df$lat_scale)*cos(df$lon_scale)
df$lat2 = cos(df$lat_scale)*sin(df$lon_scale)
#df$lat3 = sin(df$lat_scale)

#df$rot45_x = 0.707 * df$lat_scale + 0.707*df$lon_scale
#df$rot45_y = 0.707 * df$lon_scale - 0.707*df$lat_scale
df$rot30_x = 0.866 * df$lat_scale + 0.5*df$lon_scale
df$rot30_y = 0.866 * df$lon_scale - 0.5*df$lat_scale
df$rot60_x = 0.5 * df$lat_scale + 0.866*df$lon_scale
df$rot60_y = 0.5 * df$lon_scale - 0.866*df$lat_scale

df$lat = NULL
df$lon = NULL




########
##
#######
df_train = df[1:length(train.id),]
df_test = df[(length(train.id)+1):nrow(df),]




ftrs = data.frame(
  type = unlist(lapply(df[1:length(train.id),],class)),
  n.unique = unlist(lapply(df[1:length(train.id),],function(x)length(unique(x)))),
  f.missing = unlist(lapply(df[1:length(train.id),],function(x)mean(is.na(x)))),
  spear.cor = unlist(lapply(df[1:length(train.id),],function(x){idx = !is.na(x);
  if(is.factor(x)) x = as.numeric(x);
  if(is.character(x)) x = as.numeric(as.factor(x));
  if(is.integer(x)) x = as.numeric(x);
  if(is.logical(x)) x = as.numeric(x);
  cor(x[idx],y = label[idx], method = "spearman")
  }))
)

ftrs$name = rownames(ftrs)
ftrs =ftrs %>% drop_na()
df = df[,names(df) %in% ftrs$name]




#######
## GEOCODE
###
# library(ggmap)
# 
# register_google(key = "AIzaSyCKfgdgnKXUFLDM6gGpRr_4rDWxphqFXmc")
# 
# dr$nas = NA
# 
# 
# for (i in 1:nrow(d)) {
#   
#   input = c(d[i,1],d[i,2])
#   locations = revgeocode(input)
#   d[i,3] = locations
# }
# t = revgeocode(c(22.17,-28.98))
# 
# ### elevation
# q = google_elevation(df_locations = a[1:1000,])
# 


#########
df.dat = cbind(lon = df$lon,lat = df$lat)
sumsq = NULL
for (i in 1:15) {
  set.seed(1234)
 sumsq[i] = sum(kmeans(j,centers = i, iter.max = 1000,
                       algorithm = "Lloyd")$withinss)
}
plot(1:15,sumsq,type= "b")
###
set.seed(1234)
kmns = kmeans(dr.dat,8,nstart = 17,iter.max = 1000,
              algorithm = "Lloyd",trace = T)

cnts = kmns$centers
kmeans.distance = NULL
for (i in 1:nrow(cnts)) {
  kmeans.distance = cbind(kmeans.distance, sqrt(colSums((t(dr.dat)-unlist(cnts[i,]))^2)))

}
table(kmns$cluster)
save(kmeans.distance, file = paste(save.files.dir,"/kmeans_features.RData"))


# ndvi <- VI(, 8, 4)  band 8 - band4
###################
######  EXTRACT VALUES FROM IMAGES
#################
train_points = SpatialPoints(sh_trans,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
test_points = SpatialPoints(test_trans,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

####
#filenames = paste0(img.dir,"/T34JEP_20170101T082332_B0",1:9,".jp2")


#### Load Raster 
band1 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B01.jp2'))
band2 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B02.jp2'))
band3 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B03.jp2'))
band4 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B04.jp2'))
band5 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B05.jp2'))
band6 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B06.jp2'))
band7 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B07.jp2'))
band8 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B08.jp2'))
band8A = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B8A.jp2'))
band9 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B09.jp2'))
band10 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B10.jp2'))
band11 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B11.jp2'))
band12 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B12.jp2'))
bandTC = raster::stack(paste0(img.dir,'/T34JEP_20170101T082332_TCI.jp2'))
# ##### RASTER PCA
# library(doSNOW)
# # Time the code execution
# start.time <- Sys.time()
# # Create a cluster to work on 10 logical cores.
# cl <- makeCluster(8, type = "PSOCK")
# registerDoParallel(cl)
# stack1 = rasterPCA(raster::stack(band1,band9,band10))
# stack2 = rasterPCA(raster::stack(band2,band3,band4,band8))
# stack3 = rasterPCA(raster::stack(band5,band6,band7,band11))
# # Processing is done, stop cluster.
# stopCluster(cl)
# Sys.time() -  start.time

####
#ndvi_11_5 = NDVI(band11,band5)
#ndvi_8_2 = NDVI(band8,band2)

## band 1,band 6, band 3
df_train$band1 = raster::extract(band1,train_points)
#df_train$a = extract(band1,train_points,cellnumbers=T)[,1]/10000
df_train$band2 = raster::extract(band2,train_points)
#df_train$a2 = extract(band2,train_points,cellnumbers=T)[,1]/10000
df_train$band3 = raster::extract(band3,train_points)
#df_train$a3 = extract(band3,train_points,cellnumbers=T)[,1]/10000
df_train$band4 = raster::extract(band4,train_points)
#df_train$a4 = extract(band4,train_points,cellnumbers=T)[,1]/10000
df_train$band5 = raster::extract(band5,train_points)
#df_train$a5 = extract(band5,train_points,cellnumbers=T)[,1]/10000
df_train$band6 = raster::extract(band6,train_points)
#df_train$a6 = extract(band6,train_points,cellnumbers=T)[,1]/10000
df_train$band7 = raster::extract(band7,train_points)
#df_train$a7 = extract(band7,train_points,cellnumbers=T)[,1]/10000
df_train$band8 = raster::extract(band8,train_points)
df_train$band8A = raster::extract(band8A,train_points)
#df_train$a8 = extract(band8,train_points,cellnumbers=T)[,1]/10000
df_train$band9 = raster::extract(band9,train_points)
df_train$band10 = raster::extract(band10,train_points)
df_train$band11 = raster::extract(band11,train_points)
df_train$band12 = raster::extract(band12,train_points)
df_train$bandtc = extract(bandTC,train_points)
#df_train$ndvi_1 = extract(ndvi_11_5,train_points)
# pc = extract(stack1$map,train_points)
# colnames(pc) = c(paste0("stack1_PC",1:3))
# pc2 = extract(stack2$map,train_points)
# colnames(pc2) = c(paste0("stack2_PC",1:4))
# pc3 = extract(stack3$map,train_points)
# colnames(pc3) = c(paste0("stack3_PC",1:4))
# 
# df_train = cbind(df_train,pc2,pc3)
df_train$stack3_PC2=NULL
df_train$stack2_PC1=NULL
df_train$stack2_PC3=NULL
###
df_test$band1 = raster::extract(band1, test_points)
df_test$band2 = raster::extract(band2, test_points)
df_test$band3 = raster::extract(band3, test_points)
df_test$band4 = raster::extract(band4, test_points)
df_test$band5 = raster::extract(band5, test_points)
df_test$band6 = raster::extract(band6, test_points)
df_test$band7 = raster::extract(band7, test_points)
df_test$band8A = raster::extract(band8A, test_points)
df_test$band9 = raster::extract(band9, test_points)
df_test$band10 = raster::extract(band10, test_points)
df_test$band11 = raster::extract(band11, test_points)
df_test$bandtc = extract(bandTC,test_points)
#df_test$ndvi_1 = extract(ndvi_11_5,test_points)

# pc = extract(stack1$map,test_points)
# colnames(pc) = c(paste0("stack1_PC",1:3))
# pc2 = extract(stack2$map,test_points)
# colnames(pc2) = c(paste0("stack2_PC",1:4))
# pc3 = extract(stack3$map,test_points)
# colnames(pc3) = c(paste0("stack3_PC",1:4))
# #df_train$ndvi = raster::extract(ndvi, train_points)
# #df_train$band1 = raster::extract(band1,train_points)
# df_test = cbind(df_test,pc2,pc3)
# df_test$stack3_PC2=NULL
# df_test$stack2_PC1=NULL
# df_test$stack2_PC3=NULL

df_train$band1 = ifelse(df_train$band1==1682,1591,df_train$band1)
df_test$band1 = ifelse(df_test$band1==1040,1037,df_test$band1)
df_train$band6 = ifelse(df_train$band6 >4136,4136,df_train$band6) #band6
df_train$band3 = ifelse(df_train$band3 >2262,2262,df_train$band3)
#df_train$ndvi = ifelse(df_train$ndvi >4706, 4706,df_train$ndvi)
df_test$band4 = ifelse(df_test$band4 <483,483,df_test$band4)
#df_test$band2 = ifelse(df_test$band2 )
## read raster files

###### 2017-05-31
band1 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B01.jp2'))
band2 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B02.jp2'))
band3 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B03.jp2'))
band4 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B04.jp2'))
band5 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B05.jp2'))
band6 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B06.jp2'))
band7 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B07.jp2'))
band8 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B08.jp2'))
band8A = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B8A.jp2'))
band9 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B09.jp2'))
band10 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B10.jp2'))
band11 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B11.jp2'))
band12 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B12.jp2'))
bandTC5 = raster::stack(paste0(img.dir5,'/T34JEP_20170531T082011_TCI.jp2'))


##### RASTER PCA
library(doSNOW)
# Time the code execution
start.time <- Sys.time()
# Create a cluster to work on 10 logical cores.
cl <- makeCluster(8, type = "PSOCK")
registerDoParallel(cl)
stack12 = rasterPCA(raster::stack(band1,band9,band10))
stack2 = rasterPCA(raster::stack(band2,band3,band4))
stack3 = rasterPCA(raster::stack(band5,band6,band7))
# Processing is done, stop cluster.
stopCluster(cl)
Sys.time() -  start.time

pc = extract(stack1$map,train_points)
colnames(pc) = c(paste0("05_stack1_PC",1:3))
pc2 = extract(stack2$map,train_points)
colnames(pc2) = c(paste0("05_stack2_PC",1:3))
pc3 = extract(stack3$map,train_points)
colnames(pc3) = c(paste0("05_stack3_PC",1:3))

# ndvi_11_5 = NDVI(band11,band5)
df_train$band1_5 = raster::extract(band1,train_points)
df_train$band2_5 = raster::extract(band2,train_points)
df_train$band3_5 = raster::extract(band3,train_points)
df_train$band4_5 = raster::extract(band4,train_points)
df_train$band5_5 = raster::extract(band5,train_points)
df_train$band6_5 = raster::extract(band6,train_points)
df_train$band7_5 = raster::extract(band7,train_points)
df_train$band8_5 = raster::extract(band8,train_points)
df_train$band8A_5 = raster::extract(band8A,train_points)
df_train$band9_5 = raster::extract(band9,train_points)
df_train$band10_5 = raster::extract(band10,train_points)
df_train$band11_5 = raster::extract(band11,train_points)
df_train$band12_5 = raster::extract(band12,train_points)
df_train$bandtc_5 = extract(bandTC,train_points)



df_test$band1_5 = raster::extract(band1,test_points)
df_test$band2_5 = raster::extract(band2,test_points)
df_test$band3_5 = raster::extract(band3,test_points)
df_test$band4_5 = raster::extract(band4,test_points)
df_test$band5_5 = raster::extract(band5,test_points)
df_test$band6_5 = raster::extract(band6,test_points)
df_test$band7_5 = raster::extract(band7,test_points)
df_test$band8_5 = raster::extract(band8,test_points)
df_test$band9_5 = raster::extract(band9,test_points)
df_test$band10_5 = raster::extract(band10,test_points)
df_test$band11_5 = raster::extract(band11,test_points)
df_test$band12_5 = raster::extract(band12,test_points)
df_test$bandtc_5 = extract(bandTC,test_points)



###
raster::pairs(raster::stack(band5,band6,band7,band11))
raster::pairs(raster::stack(band1,band9,band10))
raster::pairs(raster::stack(band2,band3,band4,band8))

###### 2017-08-19
band1 = raster::raster(paste0(img.dir8,'/T34JEP_20170819T082011_B01.jp2'))
band2 = raster::raster(paste0(img.dir8,'/T34JEP_20170819T082011_B02.jp2'))
band3 = raster::raster(paste0(img.dir8,'/T34JEP_20170819T082011_B03.jp2'))
band4 = raster::raster(paste0(img.dir8,'/T34JEP_20170819T082011_B04.jp2'))
band5 = raster::raster(paste0(img.dir8,'/T34JEP_20170819T082011_B05.jp2'))
band6 = raster::raster(paste0(img.dir8,'/T34JEP_20170819T082011_B06.jp2'))
band7 = raster::raster(paste0(img.dir8,'/T34JEP_20170819T082011_B07.jp2'))
band8 = raster::raster(paste0(img.dir8,'/T34JEP_20170819T082011_B08.jp2'))
#band8A = raster::raster(paste0(img.dir8,'/T34JEP_20170819T082011_B8A.jp2'))
band9 = raster::raster(paste0(img.dir8,'/T34JEP_20170819T082011_B09.jp2'))
#band10 = raster::raster(paste0(img.dir8,'/T34JEP_20170819T082011_B10.jp2'))
band11 = raster::raster(paste0(img.dir8,'/T34JEP_20170819T082011_B11.jp2'))
band12 = raster::raster(paste0(img.dir8,'/T34JEP_20170819T082011_B12.jp2'))
bandTC = raster::raster(paste0(img.dir8,'/T34JEP_20170819T082011_TCI.jp2'))

#ndvi_11_5 = NDVI(band11,band5)

df_train$band1_8 = raster::extract(band1,train_points)
df_train$band2_8 = raster::extract(band2,train_points)
df_train$band3_8 = raster::extract(band3,train_points)
df_train$band4_8 = raster::extract(band4,train_points)
df_train$band5_8 = raster::extract(band5,train_points)
df_train$band6_8 = raster::extract(band6,train_points)
df_train$band7_8 = raster::extract(band7,train_points)
df_train$band8_8 = raster::extract(band8,train_points)
#df_train$band8A_8 = raster::extract(band8A,train_points)
df_train$band9_8 = raster::extract(band9,train_points)
#df_train$band10_8 = raster::extract(band10,train_points)
df_train$band11_8 = raster::extract(band11,train_points)
df_train$bandtc_8 = extract(bandTC,train_points)


df_test$band1_8 = raster::extract(band1,test_points)
df_test$band2_8 = raster::extract(band2,test_points)
df_test$band3_8 = raster::extract(band3,test_points)
df_test$band4_8 = raster::extract(band4,test_points)
df_test$band5_8 = raster::extract(band5,test_points)
df_test$band6_8 = raster::extract(band6,test_points)
df_test$band7_8 = raster::extract(band7,test_points)
df_test$band8_8 = raster::extract(band8,test_points)
df_test$band9_8 = raster::extract(band9,test_points)
df_test$band10_8 = raster::extract(band10,test_points)
df_test$band11_8 = raster::extract(band11,test_points)
df_test$bandtc_8 = extract(bandTC,test_points)



###### 2017-03-12
band1 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B01.jp2'))
band2 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B02.jp2'))
band3 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B03.jp2'))
band4 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B04.jp2'))
band5 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B05.jp2'))
band6 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B06.jp2'))
band7 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B07.jp2'))
band8 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B08.jp2'))
band8A = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B8A.jp2'))
band9 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B09.jp2'))
band10 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B10.jp2'))
band11 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B11.jp2'))
band12 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B12.jp2'))
bandTC3 = raster::stack(paste0(img.dir3,'/T34JEP_20170312T082001_TCI.jp2'))

#ndvi_11_5 = NDVI(band11,band5)

df_train$band1_12 = raster::extract(band1,train_points)
df_train$band2_12= raster::extract(band2,train_points)
df_train$band3_12 = raster::extract(band3,train_points)
df_train$band4_12 = raster::extract(band4,train_points)
df_train$band5_12 = raster::extract(band5,train_points)
df_train$band6_12 = raster::extract(band6,train_points)
df_train$band7_12 = raster::extract(band7,train_points)
df_train$band8_12 = raster::extract(band8,train_points)
df_train$band8A_12 = raster::extract(band8A,train_points)
df_train$band9_12 = raster::extract(band9,train_points)
df_train$band10_12 = raster::extract(band10,train_points)
df_train$band11_12 = raster::extract(band11,train_points)
df_train$band12_12 = raster::extract(band12,train_points)
df_train$bandtc_12 = extract(bandTC,train_points)

#df_train$ndvi_1_12 = extract(ndvi_11_5,train_points)

#
df_test$band1_12 = raster::extract(band1,test_points)
df_test$band2_12 = raster::extract(band2,test_points)
df_test$band3_12 = raster::extract(band3,test_points)
df_test$band4_12 = raster::extract(band4,test_points)
df_test$band5_12 = raster::extract(band5,test_points)
df_test$band6_12 = raster::extract(band6,test_points)
df_test$band7_12 = raster::extract(band7,test_points)
df_test$band8_12 = raster::extract(band8,test_points)
df_test$band9_12 = raster::extract(band9,test_points)
df_test$band10_12 = raster::extract(band10,test_points)
df_test$band11_12 = raster::extract(band11,test_points)
df_test$band12_12 = raster::extract(band12,test_points)
df_test$bandtc_12 = extract(bandTC,test_points)

#df_test$ndvi_1_12 = extract(ndvi_11_5,test_points)



###### 2017-08-04
band1 = raster::raster(paste0(img.dir7,'/T34JEP_20170804T081559_B01.jp2'))
band2 = raster::raster(paste0(img.dir7,'/T34JEP_20170804T081559_B02.jp2'))
band3 = raster::raster(paste0(img.dir7,'/T34JEP_20170804T081559_B03.jp2'))
band4 = raster::raster(paste0(img.dir7,'/T34JEP_20170804T081559_B04.jp2'))
band5 = raster::raster(paste0(img.dir7,'/T34JEP_20170804T081559_B05.jp2'))
band6 = raster::raster(paste0(img.dir7,'/T34JEP_20170804T081559_B06.jp2'))
band7 = raster::raster(paste0(img.dir7,'/T34JEP_20170804T081559_B07.jp2'))
band8 = raster::raster(paste0(img.dir7,'/T34JEP_20170804T081559_B08.jp2'))
band8A = raster::raster(paste0(img.dir7,'/T34JEP_20170804T081559_B8A.jp2'))
band9 = raster::raster(paste0(img.dir7,'/T34JEP_20170804T081559_B09.jp2'))
band10 = raster::raster(paste0(img.dir7,'/T34JEP_20170804T081559_B10.jp2'))
band11 = raster::raster(paste0(img.dir7,'/T34JEP_20170804T081559_B11.jp2'))
band12 = raster::raster(paste0(img.dir7,'/T34JEP_20170804T081559_B12.jp2'))
bandTC = raster::raster(paste0(img.dir7,'/T34JEP_20170804T081559_TCI.jp2'))

#ndvi_11_5 = NDVI(band11,band5)

df_train$band1_7 = raster::extract(band1,train_points)
df_train$band2_7 = raster::extract(band2,train_points)
df_train$band3_7 = raster::extract(band3,train_points)
df_train$band4_7 = raster::extract(band4,train_points)
df_train$band5_7 = raster::extract(band5,train_points)
df_train$band6_7 = raster::extract(band6,train_points)
df_train$band7_7 = raster::extract(band7,train_points)
df_train$band8_7 = raster::extract(band8,train_points)
df_train$band8A_7 = raster::extract(band8A,train_points)
df_train$band9_7 = raster::extract(band9,train_points)
df_train$band10_7 = raster::extract(band10,train_points)
df_train$band11_7 = raster::extract(band11,train_points)
df_train$band12_7 = raster::extract(band12,train_points)
df_train$bandtc_7 = extract(bandTC,train_points)

#df_train$ndvi_1_7 = extract(ndvi_11_5,train_points)


df_test$band1_7 = raster::extract(band1,test_points)
df_test$band2_7 = raster::extract(band2,test_points)
df_test$band3_7 = raster::extract(band3,test_points)
df_test$band4_7 = raster::extract(band4,test_points)
df_test$band5_7 = raster::extract(band5,test_points)
df_test$band6_7 = raster::extract(band6,test_points)
df_test$band7_7 = raster::extract(band7,test_points)
df_test$band8_7 = raster::extract(band8,test_points)
df_test$band9_7 = raster::extract(band9,test_points)
df_test$band10_7 = raster::extract(band10,test_points)
df_test$band11_7 = raster::extract(band11,test_points)
df_test$bandtc_7 = extract(bandTC,test_points)

#df_test$ndvi_1_7 = extract(ndvi_11_5,test_points)

###### 2017-01-31
band1 = raster::raster(paste0(img.dir6,'/T34JEP_20170131T082151_B01.jp2'))
band2 = raster::raster(paste0(img.dir6,'/T34JEP_20170131T082151_B02.jp2'))
band3 = raster::raster(paste0(img.dir6,'/T34JEP_20170131T082151_B03.jp2'))
band4 = raster::raster(paste0(img.dir6,'/T34JEP_20170131T082151_B04.jp2'))
band5 = raster::raster(paste0(img.dir6,'/T34JEP_20170131T082151_B05.jp2'))
band6 = raster::raster(paste0(img.dir6,'/T34JEP_20170131T082151_B06.jp2'))
band7 = raster::raster(paste0(img.dir6,'/T34JEP_20170131T082151_B07.jp2'))
band8 = raster::raster(paste0(img.dir6,'/T34JEP_20170131T082151_B08.jp2'))
band8A = raster::raster(paste0(img.dir6,'/T34JEP_20170131T082151_B8A.jp2'))
band9 = raster::raster(paste0(img.dir6,'/T34JEP_20170131T082151_B09.jp2'))
band10 = raster::raster(paste0(img.dir6,'/T34JEP_20170131T082151_B10.jp2'))
band11 = raster::raster(paste0(img.dir6,'/T34JEP_20170131T082151_B11.jp2'))
bandTC = raster::raster(paste0(img.dir6,'/T34JEP_20170131T082151_TCI.jp2'))


df_train$band1_1 = raster::extract(band1,train_points)
df_train$band2_1 = raster::extract(band2,train_points)
#df_train$band3_1 = raster::extract(band3,train_points)#
df_train$band4_1 = raster::extract(band4,train_points)
df_train$band5_1 = raster::extract(band5,train_points)
df_train$band6_1 = raster::extract(band6,train_points)
#df_train$band7_1 = raster::extract(band7,train_points)#
df_train$band8_1 = raster::extract(band8,train_points)
df_train$band8A_1 = raster::extract(band8A,train_points)
df_train$band9_1 = raster::extract(band9,train_points)
df_train$band10_1 = raster::extract(band10,train_points)
df_train$band11_1 = raster::extract(band11,train_points)
df_train$bandtc_1 = raster::extract(bandTC,train_points)

df_test$band1_1 = raster::extract(band1,test_points)
df_test$band2_1 = raster::extract(band2,test_points)
df_test$band3_1 = raster::extract(band3,test_points)
df_test$band4_1 = raster::extract(band4,test_points)
df_test$band5_1 = raster::extract(band5,test_points)
df_test$band6_1 = raster::extract(band6,test_points)
df_test$band7_1 = raster::extract(band7,test_points)
df_test$band8_1 = raster::extract(band8,test_points)
df_test$band9_1 = raster::extract(band9,test_points)
df_test$band10_1 = raster::extract(band10,test_points)
df_test$band11_1 = raster::extract(band11,test_points)
df_test$bandtc_1 = raster::extract(bandTC,test_points)


#### 10-02-2017
band1 = raster::raster(paste0(img.dir2,'/T34JEP_20170210T082051_B01.jp2'))
band2 = raster::raster(paste0(img.dir2,'/T34JEP_20170210T082051_B02.jp2'))
band3 = raster::raster(paste0(img.dir2,'/T34JEP_20170210T082051_B03.jp2'))
band4 = raster::raster(paste0(img.dir2,'/T34JEP_20170210T082051_B04.jp2'))
band5 = raster::raster(paste0(img.dir2,'/T34JEP_20170210T082051_B05.jp2'))
band6 = raster::raster(paste0(img.dir2,'/T34JEP_20170210T082051_B06.jp2'))
band7 = raster::raster(paste0(img.dir2,'/T34JEP_20170210T082051_B07.jp2'))
band8 = raster::raster(paste0(img.dir2,'/T34JEP_20170210T082051_B08.jp2'))
band8A = raster::raster(paste0(img.dir2,'/T34JEP_20170210T082051_B8A.jp2'))
band9 = raster::raster(paste0(img.dir2,'/T34JEP_20170210T082051_B09.jp2'))
band10 = raster::raster(paste0(img.dir2,'/T34JEP_20170210T082051_B10.jp2'))
band11 = raster::raster(paste0(img.dir2,'/T34JEP_20170210T082051_B11.jp2'))
bandTC = raster::stack(paste0(img.dir2,'/T34JEP_20170210T082051_TCI.jp2'))

df_train$band1_6 = raster::extract(band1,train_points)
df_train$band2_6 = raster::extract(band2,train_points)
df_train$band3_6 = raster::extract(band3,train_points)
df_train$band4_6 = raster::extract(band4,train_points)
df_train$band5_6 = raster::extract(band5,train_points)
df_train$band6_6 = raster::extract(band6,train_points)
df_train$band7_6 = raster::extract(band7,train_points)
df_train$band8_6 = raster::extract(band8,train_points)
df_train$band8A_6 = raster::extract(band8A,train_points)
df_train$band9_6 = raster::extract(band9,train_points)
df_train$band10_6 = raster::extract(band10,train_points)
df_train$band11_6 = raster::extract(band11,train_points)
df_train$bandtc_6 = raster::extract(bandTC,train_points)

df_test$band1_6 = raster::extract(band1,test_points)
df_test$band2_6 = raster::extract(band2,test_points)
df_test$band3_6 = raster::extract(band3,test_points)
df_test$band4_6 = raster::extract(band4,test_points)
df_test$band5_6 = raster::extract(band5,test_points)
df_test$band6_6 = raster::extract(band6,test_points)
df_test$band7_6 = raster::extract(band7,test_points)
df_test$band8_6 = raster::extract(band8,test_points)
df_test$band9_6 = raster::extract(band8,test_points)
df_test$band10_6 = raster::extract(band8,test_points)
df_test$band11_6 = raster::extract(band8,test_points)
df_test$bandtc_6 = raster::extract(bandTC,test_points)
#### 15-07-2017
band1 = raster::raster(paste0(img.dir4,'/T34JEP_20170715T081609_B01.jp2'))
band2 = raster::raster(paste0(img.dir4,'/T34JEP_20170715T081609_B02.jp2'))
band3 = raster::raster(paste0(img.dir4,'/T34JEP_20170715T081609_B03.jp2'))
band4 = raster::raster(paste0(img.dir4,'/T34JEP_20170715T081609_B04.jp2'))
band5 = raster::raster(paste0(img.dir4,'/T34JEP_20170715T081609_B05.jp2'))
band6 = raster::raster(paste0(img.dir4,'/T34JEP_20170715T081609_B06.jp2'))
band7 = raster::raster(paste0(img.dir4,'/T34JEP_20170715T081609_B07.jp2'))
band8 = raster::raster(paste0(img.dir4,'/T34JEP_20170715T081609_B08.jp2'))
band8A = raster::raster(paste0(img.dir4,'/T34JEP_20170715T081609_B8A.jp2'))
band9 = raster::raster(paste0(img.dir4,'/T34JEP_20170715T081609_B09.jp2'))
band10 = raster::raster(paste0(img.dir4,'/T34JEP_20170715T081609_B10.jp2'))
band11 = raster::raster(paste0(img.dir4,'/T34JEP_20170715T081609_B11.jp2'))
bandTC = raster::raster(paste0(img.dir4,'/T34JEP_20170715T081609_TCI.jp2'))

df_train$band1_4 = raster::extract(band1,train_points)
df_train$band2_4 = raster::extract(band2,train_points)
df_train$band3_4 = raster::extract(band3,train_points)
df_train$band4_4 = raster::extract(band4,train_points)
df_train$band5_4 = raster::extract(band5,train_points)
df_train$band6_4 = raster::extract(band6,train_points)
df_train$band7_4 = raster::extract(band7,train_points)
df_train$band8_4 = raster::extract(band8,train_points)
df_train$band9_4 = raster::extract(band9,train_points)
df_train$band10_4 = raster::extract(band10,train_points)
df_train$band11_4 = raster::extract(band11,train_points)
df_train$bandtc_4 = raster::extract(bandTC,train_points)

df_test$band1_4 = raster::extract(band1,test_points)
df_test$band2_4 = raster::extract(band2,test_points)
df_test$band3_4 = raster::extract(band3,test_points)
df_test$band4_4 = raster::extract(band4,test_points)
df_test$band5_4 = raster::extract(band5,test_points)
df_test$band6_4 = raster::extract(band6,test_points)
df_test$band7_4 = raster::extract(band7,test_points)
df_test$band8_4 = raster::extract(band8,test_points)
df_test$band9_4 = raster::extract(band9,test_points)
df_test$band10_4 = raster::extract(band10,test_points)
df_test$band11_4 = raster::extract(band11,test_points)
df_test$bandtc_4 = raster::extract(bandTC,test_points)

#### 10-07-2017
band1 = raster::raster(paste0(img.dir42,'/T34JEP_20170710T082011_B01.jp2'))
band2 = raster::raster(paste0(img.dir42,'/T34JEP_20170710T082011_B02.jp2'))
band3 = raster::raster(paste0(img.dir42,'/T34JEP_20170710T082011_B03.jp2'))
band4 = raster::raster(paste0(img.dir42,'/T34JEP_20170710T082011_B04.jp2'))
band5 = raster::raster(paste0(img.dir42,'/T34JEP_20170710T082011_B05.jp2'))
band6 = raster::raster(paste0(img.dir42,'/T34JEP_20170710T082011_B06.jp2'))
band7 = raster::raster(paste0(img.dir42,'/T34JEP_20170710T082011_B07.jp2'))
band8 = raster::raster(paste0(img.dir42,'/T34JEP_20170710T082011_B08.jp2'))
band8A = raster::raster(paste0(img.dir42,'/T34JEP_20170710T082011_B8A.jp2'))
band9 = raster::raster(paste0(img.dir42,'/T34JEP_20170710T082011_B09.jp2'))
band10 = raster::raster(paste0(img.dir42,'/T34JEP_20170710T082011_B10.jp2'))
band11 = raster::raster(paste0(img.dir42,'/T34JEP_20170710T082011_B11.jp2'))
bandTC = raster::raster(paste0(img.dir42,'/T34JEP_20170710T082011_TCI.jp2'))

df_train$band1_42 = raster::extract(band1,train_points)
df_train$band2_42 = raster::extract(band2,train_points)
df_train$band3_42 = raster::extract(band3,train_points)
df_train$band4_42 = raster::extract(band4,train_points)
df_train$band5_42 = raster::extract(band5,train_points)
df_train$band6_42 = raster::extract(band6,train_points)
df_train$band7_42 = raster::extract(band7,train_points)
df_train$band8_42 = raster::extract(band8,train_points)
df_train$band8A_42 = raster::extract(band8A,train_points)
df_train$band9_42 = raster::extract(band9,train_points)
df_train$band10_42 = raster::extract(band10,train_points)
df_train$band11_42 = raster::extract(band11,train_points)
df_train$bandtc_42 = raster::extract(bandTC,train_points)

df_test$band1_42 = raster::extract(band1,test_points)
df_test$band2_42 = raster::extract(band2,test_points)
df_test$band3_42 = raster::extract(band3,test_points)
df_test$band4_42 = raster::extract(band4,test_points)
df_test$band5_42 = raster::extract(band5,test_points)
df_test$band6_42 = raster::extract(band6,test_points)
df_test$band7_42 = raster::extract(band7,test_points)
df_test$band8_42 = raster::extract(band8,test_points)
#df_test$band8A_42 = raster::extract(band8A,test_points)
df_test$band9_42 = raster::extract(band9,test_points)
df_test$band10_42 = raster::extract(band10,test_points)
df_test$band11_42 = raster::extract(band11,test_points)
df_test$bandtc_42 = raster::extract(bandTC,test_points)


#### 20-06-2017
band1 = raster::raster(paste0(img.dir66,'/T34JEP_20170620T082011_B01.jp2'))
band2 = raster::raster(paste0(img.dir66,'/T34JEP_20170620T082011_B02.jp2'))
band3 = raster::raster(paste0(img.dir66,'/T34JEP_20170620T082011_B03.jp2'))
band4 = raster::raster(paste0(img.dir66,'/T34JEP_20170620T082011_B04.jp2'))
band5 = raster::raster(paste0(img.dir66,'/T34JEP_20170620T082011_B05.jp2'))
band6 = raster::raster(paste0(img.dir66,'/T34JEP_20170620T082011_B06.jp2'))
band7 = raster::raster(paste0(img.dir66,'/T34JEP_20170620T082011_B07.jp2'))
band8 = raster::raster(paste0(img.dir66,'/T34JEP_20170620T082011_B08.jp2'))
band8A = raster::raster(paste0(img.dir66,'/T34JEP_20170620T082011_B8A.jp2'))
band9 = raster::raster(paste0(img.dir66,'/T34JEP_20170620T082011_B09.jp2'))
band10 = raster::raster(paste0(img.dir66,'/T34JEP_20170620T082011_B10.jp2'))
band11 = raster::raster(paste0(img.dir66,'/T34JEP_20170620T082011_B11.jp2'))
bandTC = raster::raster(paste0(img.dir66,'/T34JEP_20170620T082011_TCI.jp2'))

df_train$band1_66 = raster::extract(band1,train_points)
df_train$band2_66 = raster::extract(band2,train_points)
df_train$band3_66 = raster::extract(band3,train_points)
df_train$band4_66 = raster::extract(band4,train_points)
df_train$band5_66 = raster::extract(band5,train_points)
df_train$band6_66 = raster::extract(band6,train_points)
df_train$band7_66 = raster::extract(band7,train_points)
df_train$band8_66 = raster::extract(band8,train_points)
df_train$band8A_66 = raster::extract(band8A,train_points)
df_train$band9_66 = raster::extract(band9,train_points)
df_train$band10_66 = raster::extract(band10,train_points)
df_train$band11_66 = raster::extract(band11,train_points)
df_train$bandtc_66 = raster::extract(bandTC,train_points)

df_test$band1_66 = raster::extract(band1,test_points)
df_test$band2_66 = raster::extract(band2,test_points)
df_test$band3_66 = raster::extract(band3,test_points)
df_test$band4_66 = raster::extract(band4,test_points)
df_test$band5_66 = raster::extract(band5,test_points)
df_test$band6_66 = raster::extract(band6,test_points)
df_test$band7_66 = raster::extract(band7,test_points)
df_test$band8_66 = raster::extract(band8,test_points)
df_test$band9_66 = raster::extract(band9,test_points)
df_test$band10_66 = raster::extract(band10,test_points)
df_test$band11_66 = raster::extract(band11,test_points)
df_test$bandtc_66 = raster::extract(bandTC,test_points)
#### 23-03-2017
band1 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B01.jp2'))
band2 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B02.jp2'))
band3 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B03.jp2'))
band4 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B04.jp2'))
band5 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B05.jp2'))
band6 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B06.jp2'))
band7 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B07.jp2'))
band8 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B08.jp2'))
band8A = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B8A.jp2'))
band9 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B09.jp2'))
band10 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B10.jp2'))
band11 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B11.jp2'))
band12 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B12.jp2'))
bandTC = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_TCI.jp2'))

df_train$band1_22 = raster::extract(band1,train_points)
df_train$band2_22 = raster::extract(band2,train_points)
df_train$band3_22 = raster::extract(band3,train_points)
df_train$band4_22 = raster::extract(band4,train_points)
df_train$band5_22 = raster::extract(band5,train_points)
df_train$band6_22 = raster::extract(band6,train_points)
df_train$band7_22 = raster::extract(band7,train_points)
df_train$band8_22 = raster::extract(band8,train_points)
df_train$band8A_22 = raster::extract(band8A,train_points)
df_train$band9_22 = raster::extract(band9,train_points)
df_train$band10_22 = raster::extract(band10,train_points)
df_train$band11_22 = raster::extract(band11,train_points)
df_train$band12_22 = raster::extract(band12,train_points)
df_train$bandtc_22 = raster::extract(bandTC,train_points)

df_test$band1_22 = raster::extract(band1,test_points)
df_test$band2_22 = raster::extract(band2,test_points)
df_test$band3_22 = raster::extract(band3,test_points)
df_test$band4_22 = raster::extract(band4,test_points)
df_test$band5_22 = raster::extract(band5,test_points)
df_test$band6_22 = raster::extract(band6,test_points)
df_test$band7_22 = raster::extract(band7,test_points)
df_test$band8_22 = raster::extract(band8,test_points)
df_test$band9_22 = raster::extract(band8,test_points)
df_test$band10_22 = raster::extract(band8,test_points)
df_test$band11_22 = raster::extract(band8,test_points)
df_test$bandtc_22 = raster::extract(band8,test_points)
## extract values from values
## extract values from values
features = raster::extract(a, trans)
###################
### XGB MODEL
###################


#####
duplicated.x = findCorrelation(cor(randomForest::na.roughfix(df_train)),
                               cutoff = 0.999, names = T, verbose = F)
length(duplicated.x)
df_train = df_train[, !names(df_train) %in% duplicated.x]

#### PCA Reduction





#run loop to create RasterStack with weighted mean of 
#neighbour values for each pixel and each band
#img_2017_2dates_4band is my raster file with several dates and bands for the same location
# library(raster)
# m=matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1), ncol=3, nrow=3)
# p = raster::stack(band5,band6,band7,band11)
# names(p) = c("B5","B6","B7","B8")
# rs_focal=focal(p[[1]], fun=mean, w=a)
# for (i in 2:nlayers(p))
# {
#   rl=focal(p[[i]], fun=mean, w=m)
#   rs_focal=stack(rs_focal, rl)
# }
# names(rs_focal)=paste(names(p), "f")
# #end of loop




# k nearest neighbors
d = cbind(dr.dat$lon, dr.dat$lat)
k <- 8
nn <- knearneigh(d, k)
orstationc.neighbors.knn <- knn2nb(nn)
plot(orstationc.neighbors.knn, d, add=TRUE, col="green")

knn.feature = matrix(unlist(orstationc.neighbors.knn), ncol = 8)


####
mat = matrix(0,ncol = 4, nrow = 3568)
nbl = tri2nb(d)
a = c()
b = c()
for (i in 1:3568) {
  a[i] = length(nbl[[i]])
  b[i] = sum(nbl[[i]])
}

for (i in 1:3568) {
  mat[i,] = nbl[[i]][1:4]
}




### Kmeans on Top features.
library(Rtsne)
#dat = df %>% dplyr::select(starts_with("stack"))
set.seed(1234)
tsne.res = Rtsne(umap_data2, check_duplicates = T, max_iter=1000,
                 perplexity = 50, #pca = TRUE,
                 theta = 0.5, dims = 2, verbose =T)
save(tsne.res, file = paste0(save.files.dir,"/tsne.RData"))


### Kmeans on top features
sumsq = NULL
for (i in 1:15) {
  set.seed(1234)
  sumsq[i] = sum(kmeans(umap_data2,centers = i, iter.max = 1000,
                        algorithm = "Lloyd")$withinss)
}
plot(1:15,sumsq,type= "b")
###
set.seed(1234)
kmns = kmeans(dr.dat,8,nstart = 17,iter.max = 1000,
              algorithm = "Lloyd",trace = T)





#######
df_train$stack2_PC1 = ifelse(is.na(df_train$stack2_PC1),trainpca2$stack2_PC1,df_train$stack2_PC1)
df_train$stack2_PC2 = ifelse(is.na(df_train$stack2_PC2),trainpca2$stack2_PC2,df_train$stack2_PC2)
df_train$stack2_PC3 = ifelse(is.na(df_train$stack2_PC3),trainpca2$stack2_PC3,df_train$stack2_PC3)
df_train$stack2_PC4 = ifelse(is.na(df_train$stack2_PC4),trainpca2$stack2_PC4,df_train$stack2_PC4)

df_train$stack3_PC1 = ifelse(is.na(df_train$stack3_PC1),trainpca2$stack3_PC1,df_train$stack3_PC1)
df_train$stack3_PC2 = ifelse(is.na(df_train$stack3_PC2),trainpca2$stack3_PC2,df_train$stack3_PC2)
df_train$stack3_PC3 = ifelse(is.na(df_train$stack3_PC3),trainpca2$stack3_PC3,df_train$stack3_PC3)
df_train$stack3_PC4 = ifelse(is.na(df_train$stack3_PC4),trainpca2$stack3_PC4,df_train$stack3_PC4)

####
df_train$stack2_5_PC1 = ifelse(is.na(df_train$stack2_5_PC1),trainpca2$stack2_5_PC1,df_train$stack2_5_PC1)
df_train$stack2_5_PC2 = ifelse(is.na(df_train$stack2_5_PC2),trainpca2$stack2_5_PC2,df_train$stack2_5_PC2)
df_train$stack2_5_PC3 = ifelse(is.na(df_train$stack2_5_PC3),trainpca2$stack2_5_PC3,df_train$stack2_5_PC3)


df_train$stack3_5_PC1 = ifelse(is.na(df_train$stack3_5_PC1),trainpca2$stack3_5_PC1,df_train$stack3_5_PC1)
df_train$stack3_5_PC2 = ifelse(is.na(df_train$stack3_5_PC2),trainpca2$stack3_5_PC2,df_train$stack3_5_PC2)
df_train$stack3_5_PC3 = ifelse(is.na(df_train$stack3_5_PC3),trainpca2$stack3_5_PC3,df_train$stack3_5_PC3)


#####
df_train$stack2_12_PC1 = ifelse(is.na(df_train$stack2_12_PC1),trainpca2$stack2_12_PC1,df_train$stack2_12_PC1)
df_train$stack2_12_PC2 = ifelse(is.na(df_train$stack2_12_PC2),trainpca2$stack2_12_PC2,df_train$stack2_12_PC2)
df_train$stack2_12_PC3 = ifelse(is.na(df_train$stack2_12_PC3),trainpca2$stack2_12_PC3,df_train$stack2_12_PC3)

df_train$stack3_12_PC1 = ifelse(is.na(df_train$stack3_12_PC1),trainpca2$stack3_12_PC1,df_train$stack3_12_PC1)
df_train$stack3_12_PC2 = ifelse(is.na(df_train$stack3_12_PC2),trainpca2$stack3_12_PC2,df_train$stack3_12_PC2)
df_train$stack3_12_PC3 = ifelse(is.na(df_train$stack3_12_PC3),trainpca2$stack3_12_PC3,df_train$stack3_12_PC3)

####
df_train$stack2_1_PC1 = ifelse(is.na(df_train$stack2_1_PC1),trainpca6$stack2_1_PC1,df_train$stack2_1_PC1)
df_train$stack2_1_PC2 = ifelse(is.na(df_train$stack2_1_PC2),trainpca6$stack2_1_PC2,df_train$stack2_1_PC2)

df_train$stack3_1_PC1 = ifelse(is.na(df_train$stack3_1_PC1),trainpca6$stack3_1_PC1,df_train$stack3_1_PC1)
df_train$stack3_1_PC2 = ifelse(is.na(df_train$stack3_1_PC2),trainpca6$stack3_1_PC2,df_train$stack3_1_PC2)

########
df_train$stack2_2_PC1 = ifelse(is.na(df_train$stack2_2_PC1),trainpca6$stack2_2_PC1,df_train$stack2_2_PC1)
df_train$stack2_2_PC2 = ifelse(is.na(df_train$stack2_2_PC2),trainpca6$stack2_2_PC2,df_train$stack2_2_PC2)

df_train$stack3_2_PC1 = ifelse(is.na(df_train$stack3_2_PC1),trainpca6$stack3_2_PC1,df_train$stack3_2_PC1)
df_train$stack3_2_PC2 = ifelse(is.na(df_train$stack3_2_PC2),trainpca6$stack3_2_PC2,df_train$stack3_2_PC2)

#########
df_train$stack2_6_PC1 = ifelse(is.na(df_train$stack2_6_PC1),trainpca6$stack2_6_PC1,df_train$stack2_6_PC1)
df_train$stack2_6_PC2 = ifelse(is.na(df_train$stack2_6_PC2),trainpca6$stack2_6_PC2,df_train$stack2_6_PC2)

df_train$stack3_6_PC1 = ifelse(is.na(df_train$stack3_6_PC1),trainpca6$stack3_6_PC1,df_train$stack3_6_PC1)
df_train$stack3_6_PC2 = ifelse(is.na(df_train$stack3_6_PC2),trainpca6$stack3_6_PC2,df_train$stack3_6_PC2)


#######
df_test$stack2_PC1 = ifelse(is.na(df_test$stack2_PC1),testpca2$stack2_PC1,df_test$stack2_PC1)
df_test$stack2_PC2 = ifelse(is.na(df_test$stack2_PC2),testpca2$stack2_PC2,df_test$stack2_PC2)
df_test$stack2_PC3 = ifelse(is.na(df_test$stack2_PC3),testpca2$stack2_PC3,df_test$stack2_PC3)
df_test$stack2_PC4 = ifelse(is.na(df_test$stack2_PC4),testpca2$stack2_PC4,df_test$stack2_PC4)

df_test$stack3_PC1 = ifelse(is.na(df_test$stack3_PC1),testpca2$stack3_PC1,df_test$stack3_PC1)
df_test$stack3_PC2 = ifelse(is.na(df_test$stack3_PC2),testpca2$stack3_PC2,df_test$stack3_PC2)
df_test$stack3_PC3 = ifelse(is.na(df_test$stack3_PC3),testpca2$stack3_PC3,df_test$stack3_PC3)
df_test$stack3_PC4 = ifelse(is.na(df_test$stack3_PC4),testpca2$stack3_PC4,df_test$stack3_PC4)

####
df_test$stack2_5_PC1 = ifelse(is.na(df_test$stack2_5_PC1),testpca2$stack2_5_PC1,df_test$stack2_5_PC1)
df_test$stack2_5_PC2 = ifelse(is.na(df_test$stack2_5_PC2),testpca2$stack2_5_PC2,df_test$stack2_5_PC2)
df_test$stack2_5_PC3 = ifelse(is.na(df_test$stack2_5_PC3),testpca2$stack2_5_PC3,df_test$stack2_5_PC3)


df_test$stack3_5_PC1 = ifelse(is.na(df_test$stack3_5_PC1),testpca2$stack3_5_PC1,df_test$stack3_5_PC1)
df_test$stack3_5_PC2 = ifelse(is.na(df_test$stack3_5_PC2),testpca2$stack3_5_PC2,df_test$stack3_5_PC2)
df_test$stack3_5_PC3 = ifelse(is.na(df_test$stack3_5_PC3),testpca2$stack3_5_PC3,df_test$stack3_5_PC3)


#####
df_test$stack2_12_PC1 = ifelse(is.na(df_test$stack2_12_PC1),testpca2$stack2_12_PC1,df_test$stack2_12_PC1)
df_test$stack2_12_PC2 = ifelse(is.na(df_test$stack2_12_PC2),testpca2$stack2_12_PC2,df_test$stack2_12_PC2)
df_test$stack2_12_PC3 = ifelse(is.na(df_test$stack2_12_PC3),testpca2$stack2_12_PC3,df_test$stack2_12_PC3)

df_test$stack3_12_PC1 = ifelse(is.na(df_test$stack3_12_PC1),testpca2$stack3_12_PC1,df_test$stack3_12_PC1)
df_test$stack3_12_PC2 = ifelse(is.na(df_test$stack3_12_PC2),testpca2$stack3_12_PC2,df_test$stack3_12_PC2)
df_test$stack3_12_PC3 = ifelse(is.na(df_test$stack3_12_PC3),testpca2$stack3_12_PC3,df_test$stack3_12_PC3)








# Ht = function(DF1, ntimes = 1){
#   w = function(k){
#     s1 = dwt(k, filter = "haar")
#     return(s1@V[[1]])
#   }
#   smt = DF1
#   for (i in 1:ntimes) {
#     smt = t(apply(smt,1,w))
#   }
#   return(data.frame(smt))
# }
# 
# a = Ht(as.matrix(dat,9))

dat = df %>% dplyr::select(ends_with("_5"))
der = function(x,d=1){
  df = t(diff(t(x), differences = d))
  return(df)
}

a5 = der(a,1)
colnames(a5)  = paste0("der5_",1:ncol(a5))
s = ssvd(as.matrix(j), k = 4,n =4,maxit = 1000)
colnames(s$u) = paste0("svd",1:ncol(s$u))
svd.features = data.frame(s$u)









### AUToencoder
library(h2o)
h2o.init(nthreads = -1)
dr.dat.h2o = as.h2o(umap_data2)
m.aec = h2o.deeplearning(
  x = names(dr.dat),
  training_frame= dr.dat.h2o,
  autoencoder = T,
  activation = "Tanh",
  model_id = "autoenc1",
  hidden = c(5,2,5),
  epochs =100,
  seed = 1234
)

deep.fea = as.data.frame(h2o.deepfeatures(m.aec, dr.dat.h2o, layer = 2))
save(deep.fea, file = paste0(save.files.dir,"/autoenc_output.RData"))

h2o.shutdown()

prostate.anon = h2o.anomaly(m.aec, dr.dat.h2o, per_feature=FALSE)
head(prostate.anon)
err <- as.data.frame(prostate.anon)



### DBSCAN
set.seed(1234)
# fpc package
res.fpc <- fpc::dbscan(dr.dat, eps = 4, MinPts = 8)
summary(as.factor(res.fpc$cluster))
save(res.fpc, file = paste0(save.files.dir,"/dbscan_clus.RData"))




