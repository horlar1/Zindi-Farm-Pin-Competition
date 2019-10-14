
img.dir3 = paste0(data.dir,"/S2A_MSIL1C_20170312T082001_N0204_R121_T34JEP_20170312T084235.SAFE/GRANULE/L1C_T34JEP_A008984_20170312T084235/IMG_DATA")
img.dir5 = paste0(data.dir,"/S2A_MSIL1C_20170531T082011_N0205_R121_T34JEP_20170531T084246.SAFE/GRANULE/L1C_T34JEP_A010128_20170531T084246/IMG_DATA")

img.dir1 = paste0(data.dir,"/S2A_MSIL1C_20170322T081611_N0204_R121_T34JEP_20170322T084728.SAFE/GRANULE/L1C_T34JEP_A009127_20170322T084728/IMG_DATA")

####
band2 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B02.jp2'))
band3 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B03.jp2'))
band4 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B04.jp2'))
band8 = raster::raster(paste0(img.dir5,'/T34JEP_20170531T082011_B08.jp2'))


a = raster::stack(band2,band3,band4,band8)
names(a) = c("B2","B3","B4","B8")
sp = spectralIndices(a, blue = "B2",red = "B4",green = "B3",nir = "B8",
                     indices = c("NDVI","GEMI"))
o = extract(sp,train_points)
df_train = cbind(df_train,o[-2402,])
o = extract(sp,test_points)
df_test = cbind(df_test,o)

### 12-03-2017
band2 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B02.jp2'))
band3 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B03.jp2'))
band4 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B04.jp2'))
band8 = raster::raster(paste0(img.dir3,'/T34JEP_20170312T082001_B08.jp2'))
a = raster::stack(band2,band3,band4,band8)
names(a) = c("B2","B3","B4","B8")
sp = spectralIndices(a, blue = "B2",red = "B4",green = "B3",nir = "B8",
                     indices = c("NDVI","GEMI"))
o = extract(sp,train_points)
colnames(o) = c("NDVI_3","GEMI_3")
df_train = cbind(df_train,o[-2402,])
o = extract(sp,test_points)
colnames(o) = c("NDVI_3","GEMI_3")
df_test = cbind(df_test,o)

### 01-01-2017
band2 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B02.jp2'))
band3 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B03.jp2'))
band4 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B04.jp2'))
band8 = raster::raster(paste0(img.dir,'/T34JEP_20170101T082332_B08.jp2'))
a = raster::stack(band2,band3,band4,band8)
names(a) = c("B2","B3","B4","B8")
sp = spectralIndices(a, blue = "B2",red = "B4",green = "B3",nir = "B8",
                     indices = c("NDVI","GEMI"))
o = extract(sp,train_points)
colnames(o) = c("NDVI_1","GEMI_1")
df_train = cbind(df_train,o[-2402,])
o = extract(sp,test_points)
colnames(o) = c("NDVI_1","GEMI_1")
df_test = cbind(df_test,o)

### 22-03-2017
band2 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B02.jp2'))
band3 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B03.jp2'))
band4 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B04.jp2'))
band8 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B08.jp2'))
a = raster::stack(band2,band3,band4,band8)
names(a) = c("B2","B3","B4","B8")
sp = spectralIndices(a, blue = "B2",red = "B4",green = "B3",nir = "B8",
                     indices = c("NDVI","GEMI"))
o = extract(sp,train_points)
colnames(o) = c("NDVI_22","GEMI_22")
df_train = cbind(df_train,o[-2402,])
o = extract(sp,test_points)
colnames(o) = c("NDVI_22","GEMI_22")
df_test = cbind(df_test,o)




###################
img.dir3 = paste0(data.dir,"/S2A_MSIL1C_20170312T082001_N0204_R121_T34JFP_20170312T084235.SAFE/GRANULE/L1C_T34JFP_A008984_20170312T084235/IMG_DATA")
img.dir5 = paste0(data.dir,"/S2A_MSIL1C_20170531T082011_N0205_R121_T34JFP_20170531T084246.SAFE/GRANULE/L1C_T34JFP_A010128_20170531T084246/IMG_DATA")

img.jfp = paste0(data.dir,"/S2A_MSIL1C_20170101T082332_N0204_R121_T34JFP_20170101T084543.SAFE/GRANULE/L1C_T34JFP_A007983_20170101T084543/IMG_DATA")
img.dir1 = paste0(data.dir,"/S2A_MSIL1C_20170322T081611_N0204_R121_T34JFP_20170322T084728.SAFE/GRANULE/L1C_T34JFP_A009127_20170322T084728/IMG_DATA")


band2 = raster::raster(paste0(img.dir5,'/T34JFP_20170531T082011_B02.jp2'))
band3 = raster::raster(paste0(img.dir5,'/T34JFP_20170531T082011_B03.jp2'))
band4 = raster::raster(paste0(img.dir5,'/T34JFP_20170531T082011_B04.jp2'))
band8 = raster::raster(paste0(img.dir5,'/T34JFP_20170531T082011_B08.jp2'))

a = raster::stack(band2,band3,band4,band8)
names(a) = c("B2","B3","B4","B8")
sp = spectralIndices(a, blue = "B2",red = "B4",green = "B3",nir = "B8",
                     indices = c("NDVI","GEMI"))
o = extract(sp,train_points)
df_train$NDVI = ifelse(is.na(df_train$NDVI),o[,1],df_train$NDVI)
df_train$GEMI = ifelse(is.na(df_train$GEMI),o[,2],df_train$GEMI)

o = extract(sp,test_points)

df_test$NDVI = ifelse(is.na(df_test$NDVI),o[,1],df_test$NDVI)
df_test$GEMI = ifelse(is.na(df_test$GEMI),o[,2],df_test$GEMI)

### 12-03-2017
band2 = raster::raster(paste0(img.dir3,'/T34JFP_20170312T082001_B02.jp2'))
band3 = raster::raster(paste0(img.dir3,'/T34JFP_20170312T082001_B03.jp2'))
band4 = raster::raster(paste0(img.dir3,'/T34JFP_20170312T082001_B04.jp2'))
band8 = raster::raster(paste0(img.dir3,'/T34JFP_20170312T082001_B08.jp2'))
a = raster::stack(band2,band3,band4,band8)
names(a) = c("B2","B3","B4","B8")
sp = spectralIndices(a, blue = "B2",red = "B4",green = "B3",nir = "B8",
                     indices = c("NDVI","GEMI"))
o = extract(sp,train_points)

df_train$NDVI_3 = ifelse(is.na(df_train$NDVI_3),o[,1],df_train$NDVI_3)
df_train$GEMI_3 = ifelse(is.na(df_train$GEMI_3),o[,2],df_train$GEMI_3)

o = extract(sp,test_points)
colnames(o) = c("NDVI_3","GEMI_3")
df_test$NDVI_3 = ifelse(is.na(df_test$NDVI_3),o[,1],df_test$NDVI_3)
df_test$GEMI_3 = ifelse(is.na(df_test$GEMI_3),o[,2],df_test$GEMI_3)


band2 = raster::raster(paste0(img.jfp,'/T34JFP_20170101T082332_B02.jp2'))
band3 = raster::raster(paste0(img.jfp,'/T34JFP_20170101T082332_B03.jp2'))
band4 = raster::raster(paste0(img.jfp,'/T34JFP_20170101T082332_B04.jp2'))
band8 = raster::raster(paste0(img.jfp,'/T34JFP_20170101T082332_B08.jp2'))
a = raster::stack(band2,band3,band4,band8)
names(a) = c("B2","B3","B4","B8")
sp = spectralIndices(a, blue = "B2",red = "B4",green = "B3",nir = "B8",
                     indices = c("NDVI","GEMI"))
o = extract(sp,train_points)
df_train$NDVI_1 = ifelse(is.na(df_train$NDVI_1),o[,1],df_train$NDVI_1)
df_train$GEMI_1 = ifelse(is.na(df_train$GEMI_1),o[,2],df_train$GEMI_1)
o = extract(sp,test_points)
df_test$NDVI_1 = ifelse(is.na(df_test$NDVI_1),o[,1],df_test$NDVI_1)
df_test$GEMI_1 = ifelse(is.na(df_test$GEMI_1),o[,2],df_test$GEMI_1)

### 22-03-2017
band2 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B02.jp2'))
band3 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B03.jp2'))
band4 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B04.jp2'))
band8 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B08.jp2'))
a = raster::stack(band2,band3,band4,band8)
names(a) = c("B2","B3","B4","B8")
sp = spectralIndices(a, blue = "B2",red = "B4",green = "B3",nir = "B8",
                     indices = c("NDVI","GEMI"))
o = extract(sp,train_points)
colnames(o) = c("NDVI_22","GEMI_22")
df_train$NDVI_22 = ifelse(is.na(df_train$NDVI_22),o[,1],df_train$NDVI_22)
df_train$GEMI_22 = ifelse(is.na(df_train$GEMI_22),o[,2],df_train$GEMI_22)
o = extract(sp,test_points)
colnames(o) = c("NDVI_22","GEMI_22")
df_test$NDVI_22 = ifelse(is.na(df_test$NDVI_22),o[,1],df_test$NDVI_22)
df_test$GEMI_22 = ifelse(is.na(df_test$GEMI_22),o[,2],df_test$GEMI_22)

