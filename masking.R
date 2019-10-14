
img.dir = paste0(data.dir,"/S2A_MSIL1C_20170101T082332_N0204_R121_T34JEP_20170101T084543.SAFE/GRANULE/L1C_T34JEP_A007983_20170101T084543/QI_DATA")

img.dir3 = paste0(data.dir,"/S2A_MSIL1C_20170312T082001_N0204_R121_T34JEP_20170312T084235.SAFE/GRANULE/L1C_T34JEP_A008984_20170312T084235/QI_DATA")

img.dir6 = paste0(data.dir,"/S2A_MSIL1C_20170131T082151_N0204_R121_T34JEP_20170131T084118.SAFE/GRANULE/L1C_T34JEP_A008412_20170131T084118/QI_DATA")

img.dir4 = paste0(data.dir,"/S2B_MSIL1C_20170715T081609_N0205_R121_T34JEP_20170715T084650.SAFE/GRANULE/L1C_T34JEP_A001863_20170715T084650/QI_DATA")


img.dir42 = paste0(data.dir,"/S2A_MSIL1C_20170710T082011_N0205_R121_T34JEP_20170710T084244.SAFE/GRANULE/L1C_T34JEP_A010700_20170710T084244/QI_DATA")

img.dir7 = paste0(data.dir,"/S2B_MSIL1C_20170804T081559_N0205_R121_T34JEP_20170804T084631.SAFE/GRANULE/L1C_T34JEP_A002149_20170804T084631/QI_DATA")

img.dir8 = paste0(data.dir,"/S2A_MSIL1C_20170819T082011_N0205_R121_T34JEP_20170819T084427.SAFE/GRANULE/L1C_T34JEP_A011272_20170819T084427/QI_DATA")

img.dir5 = paste0(data.dir,"/S2A_MSIL1C_20170531T082011_N0205_R121_T34JEP_20170531T084246.SAFE/GRANULE/L1C_T34JEP_A010128_20170531T084246/QI_DATA")

img.dir2 = paste0(data.dir,"/S2A_MSIL1C_20170210T082051_N0204_R121_T34JEP_20170210T083752.SAFE/GRANULE/L1C_T34JEP_A008555_20170210T083752/QI_DATA")

img.dir66 = paste0(data.dir,"/S2A_MSIL1C_20170620T082011_N0205_R121_T34JEP_20170620T084200.SAFE/GRANULE/L1C_T34JEP_A010414_20170620T084200/QI_DATA")

img.dir1 = paste0(data.dir,"/S2A_MSIL1C_20170322T081611_N0204_R121_T34JEP_20170322T084728.SAFE/GRANULE/L1C_T34JEP_A009127_20170322T084728/QI_DATA")

img.dir8A = paste0(data.dir,"/S2A_MSIL1C_20170819T082011_N0205_R121_T34JFP_20170819T084427.SAFE/GRANULE/L1C_T34JFP_A011272_20170819T084427/QI_DATA")

## 01-01-2017
d1 = readOGR(paste0(img.dir,"/MSK_DETFOO_B01.gml"))
d2 = readOGR(paste0(img.dir,"/MSK_DETFOO_B02.gml"))
d3 = readOGR(paste0(img.dir,"/MSK_DETFOO_B03.gml"))
d4 = readOGR(paste0(img.dir,"/MSK_DETFOO_B04.gml"))
d5 = readOGR(paste0(img.dir,"/MSK_DETFOO_B05.gml"))
d6 = readOGR(paste0(img.dir,"/MSK_DETFOO_B06.gml"))
d7 = readOGR(paste0(img.dir,"/MSK_DETFOO_B07.gml"))
d8 = readOGR(paste0(img.dir,"/MSK_DETFOO_B08.gml"))
d9 = readOGR(paste0(img.dir,"/MSK_DETFOO_B09.gml"))
d11 = readOGR(paste0(img.dir,"/MSK_DETFOO_B11.gml"))
crs(d1) = "+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
d1 = spTransform(d1,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

crs(d5) = "+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
d5 = spTransform(d5,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
a = extract(d1, train_points)
a = a %>% filter(!duplicated(point.ID))
df_train$d1 = a$poly.ID
a2 = extract(d5, train_points)
a2 = a2 %>% filter(!duplicated(point.ID))
df_train$d5 = a2$poly.ID

### 19-08-2017
p1 = readOGR(paste0(img.dir8,"/MSK_CLOUDS_B00.gml"))
crs(p1) = "+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
p1 = spTransform(p1,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

p2 = readOGR(paste0(img.dir8A,"/MSK_CLOUDS_B00.gml"))
crs(p2) = "+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
p2 = spTransform(p2,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
c2 = extract(p2,train_points)[,2]

df_train$clou = ifelse(is.na(df_train$clou),c2, df_train$clou)



#### 12-03-2017  PVI
p = raster::stack(paste0(img.dir,"/T34JEP_20170101T082332_PVI.jp2"))
names(p) = c("PV1_3","PV2_3","PV3_3")
o = extract(p,train_points)
