
library(RStoolbox)
library(raster)
library(doSNOW)
library(doParallel)
library(parallel)


# img.dir1 = paste0(data.dir,"/S2A_MSIL1C_20170322T081611_N0204_R121_T34JEP_20170322T084728.SAFE/GRANULE/L1C_T34JEP_A009127_20170322T084728/IMG_DATA")
# 
# cat("Mapping PCA 1.........")
# #### Load Raster 
# #### 23-03-2017
# band1 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B01.jp2'))
# band2 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B02.jp2'))
# band3 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B03.jp2'))
# band4 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B04.jp2'))
# band5 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B05.jp2'))
# band6 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B06.jp2'))
# band7 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B07.jp2'))
# band8 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B08.jp2'))
#band8A = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B8A.jp2'))
# band9 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B09.jp2'))
# band10 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B10.jp2'))
# band11 = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_B11.jp2'))
# bandTC = raster::raster(paste0(img.dir1,'/T34JEP_20170322T081611_TCI.jp2'))

##### RASTER PCA
# Time the code execution
# start.time <- Sys.time()
# # Create a cluster to work on 10 logical cores.
# cl <- makeCluster(8, type = "PSOCK")
# registerDoParallel(cl)

# #stack1 = rasterPCA(raster::stack(band1,band9,band10))
# stack2 = rasterPCA(raster::stack(band2,band4))
# stack3 = rasterPCA(raster::stack(band5,band6))
# 
# # pc = extract(stack1$map,train_points)
# # colnames(pc) = c(paste0("stack1_PC",1:3))
# pc2 = extract(stack2$map,train_points)
# colnames(pc2) = c(paste0("stack2_PC",1:2))
# pc3 = extract(stack3$map,train_points)
# colnames(pc3) = c(paste0("stack3_PC",1:2))
# 
# trainpca3 = cbind(pc2,pc3)
# 
# ### test
# # pc = extract(stack1$map,test_points)
# # colnames(pc) = c(paste0("stack1_5_PC",1:3))
# pc2 = extract(stack2$map,test_points)
# colnames(pc2) = c(paste0("stack2_PC",1:2))
# pc3 = extract(stack3$map,test_points)
# colnames(pc3) = c(paste0("stack3_PC",1:2))
# testpca3 = cbind(pc2,pc3)
# 
# 
# img.dir1 = paste0(data.dir,"/S2A_MSIL1C_20170322T081611_N0204_R121_T34JFP_20170322T084728.SAFE/GRANULE/L1C_T34JFP_A009127_20170322T084728/IMG_DATA")
# 
# cat("Mapping PCA 2.........")
# ###### 2017-03-22
# band1 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B01.jp2'))
# band2 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B02.jp2'))
# band3 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B03.jp2'))
# band4 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B04.jp2'))
# band5 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B05.jp2'))
# band6 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B06.jp2'))
# band7 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B07.jp2'))
# band8 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B08.jp2'))
# #band8A = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B8A.jp2'))
# # band9 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B09.jp2'))
# # band10 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B10.jp2'))
# # band11 = raster::raster(paste0(img.dir1,'/T34JFP_20170322T081611_B11.jp2'))
# 
# ##### RASTER PCA
# 
# #stack1 = rasterPCA(raster::stack(band1,band9,band10))
# stack2 = rasterPCA(raster::stack(band2,band4))
# stack3 = rasterPCA(raster::stack(band5,band6))
# 
# # pc = extract(stack1$map,train_points)
# # colnames(pc) = c(paste0("stack1_PC",1:3))
# pc2 = extract(stack2$map,train_points)
# colnames(pc2) = c(paste0("stack2_PC",1:2))
# pc3 = extract(stack3$map,train_points)
# colnames(pc3) = c(paste0("stack3_PC",1:2))
# 
# trainpca4 = cbind(pc2,pc3)
# 
# # pc = extract(stack1$map,test_points)
# # colnames(pc) = c(paste0("stack1_5_PC",1:3))
# pc2 = extract(stack2$map,test_points)
# colnames(pc2) = c(paste0("stack2_PC",1:2))
# pc3 = extract(stack3$map,test_points)
# colnames(pc3) = c(paste0("stack3_PC",1:2))
# testpca4 = cbind(pc2,pc3)






img.dir6 = paste0(data.dir,"/S2A_MSIL1C_20170131T082151_N0204_R121_T34JFP_20170131T084118.SAFE/GRANULE/L1C_T34JFP_A008412_20170131T084118/IMG_DATA")

###### 2017-01-31
band1 = raster::raster(paste0(img.dir6,'/T34JFP_20170131T082151_B01.jp2'))
band2 = raster::raster(paste0(img.dir6,'/T34JFP_20170131T082151_B02.jp2'))
band3 = raster::raster(paste0(img.dir6,'/T34JFP_20170131T082151_B03.jp2'))
band4 = raster::raster(paste0(img.dir6,'/T34JFP_20170131T082151_B04.jp2'))
band5 = raster::raster(paste0(img.dir6,'/T34JFP_20170131T082151_B05.jp2'))
band6 = raster::raster(paste0(img.dir6,'/T34JFP_20170131T082151_B06.jp2'))
band7 = raster::raster(paste0(img.dir6,'/T34JFP_20170131T082151_B07.jp2'))
band8 = raster::raster(paste0(img.dir6,'/T34JFP_20170131T082151_B08.jp2'))
band8A = raster::raster(paste0(img.dir6,'/T34JFP_20170131T082151_B8A.jp2'))
band9 = raster::raster(paste0(img.dir6,'/T34JFP_20170131T082151_B09.jp2'))
band10 = raster::raster(paste0(img.dir6,'/T34JFP_20170131T082151_B10.jp2'))
band11 = raster::raster(paste0(img.dir6,'/T34JFP_20170131T082151_B11.jp2'))
bandTC = raster::raster(paste0(img.dir6,'/T34JFP_20170131T082151_TCI.jp2'))

##### RASTER PCA
# Time the code execution
start.time <- Sys.time()
# Create a cluster to work on 10 logical cores.
cl <- makeCluster(8, type = "PSOCK")
registerDoParallel(cl)
 cat("Mapping PCA 1.........")

#stack1 = rasterPCA(raster::stack(band1,band9,band10))
stack2 = rasterPCA(raster::stack(band2,band3,band4,band8),nComp = 2)
stack3 = rasterPCA(raster::stack(band5,band6,band7,band11),nComp = 2)

# pc = extract(stack1$map,train_points)
# colnames(pc) = c(paste0("stack1_PC",1:3))
pc2 = extract(stack2$map,train_points)
colnames(pc2) = c(paste0("stack2_1_PC",1:2))
pc3 = extract(stack3$map,train_points)
colnames(pc3) = c(paste0("stack3_1_PC",1:2))

trainpca6 = cbind(pc2,pc3)

### test
# pc = extract(stack1$map,test_points)
# colnames(pc) = c(paste0("stack1_5_PC",1:3))
pc2 = extract(stack2$map,test_points)
colnames(pc2) = c(paste0("stack2_1_PC",1:2))
pc3 = extract(stack3$map,test_points)
colnames(pc3) = c(paste0("stack3_1_PC",1:2))
testpca6 = cbind(pc2,pc3)

img.dir2 = paste0(data.dir,"/S2A_MSIL1C_20170210T082051_N0204_R121_T34JFP_20170210T083752.SAFE/GRANULE/L1C_T34JFP_A008555_20170210T083752/IMG_DATA")
#### 10-02-2017
band1 = raster::raster(paste0(img.dir2,'/T34JFP_20170210T082051_B01.jp2'))
band2 = raster::raster(paste0(img.dir2,'/T34JFP_20170210T082051_B02.jp2'))
band3 = raster::raster(paste0(img.dir2,'/T34JFP_20170210T082051_B03.jp2'))
band4 = raster::raster(paste0(img.dir2,'/T34JFP_20170210T082051_B04.jp2'))
band5 = raster::raster(paste0(img.dir2,'/T34JFP_20170210T082051_B05.jp2'))
band6 = raster::raster(paste0(img.dir2,'/T34JFP_20170210T082051_B06.jp2'))
band7 = raster::raster(paste0(img.dir2,'/T34JFP_20170210T082051_B07.jp2'))
band8 = raster::raster(paste0(img.dir2,'/T34JFP_20170210T082051_B08.jp2'))
band8A = raster::raster(paste0(img.dir2,'/T34JFP_20170210T082051_B8A.jp2'))
band9 = raster::raster(paste0(img.dir2,'/T34JFP_20170210T082051_B09.jp2'))
band10 = raster::raster(paste0(img.dir2,'/T34JFP_20170210T082051_B10.jp2'))
band11 = raster::raster(paste0(img.dir2,'/T34JFP_20170210T082051_B11.jp2'))
bandTC = raster::stack(paste0(img.dir2,'/T34JFP_20170210T082051_TCI.jp2'))

cat("Mapping PCA 2.........")
#stack1 = rasterPCA(raster::stack(band1,band9,band10))
stack2 = rasterPCA(raster::stack(band2,band3,band4,band8),nComp = 2)
stack3 = rasterPCA(raster::stack(band5,band6,band7,band11),nComp = 2)

# pc = extract(stack1$map,train_points)
# colnames(pc) = c(paste0("stack1_PC",1:3))
pc2 = extract(stack2$map,train_points)
colnames(pc2) = c(paste0("stack2_2_PC",1:2))
pc3 = extract(stack3$map,train_points)
colnames(pc3) = c(paste0("stack3_2_PC",1:2))

trainpca6 = cbind(trainpca6,pc2,pc3)

### test
# pc = extract(stack1$map,test_points)
# colnames(pc) = c(paste0("stack1_5_PC",1:3))
pc2 = extract(stack2$map,test_points)
colnames(pc2) = c(paste0("stack2_2_PC",1:2))
pc3 = extract(stack3$map,test_points)
colnames(pc3) = c(paste0("stack3_2_PC",1:2))
testpca6 = cbind(testpca6,pc2,pc3)


img.dir66 = paste0(data.dir,"/S2A_MSIL1C_20170620T082011_N0205_R121_T34JFP_20170620T084200.SAFE/GRANULE/L1C_T34JFP_A010414_20170620T084200/IMG_DATA")

#### 20-06-2017
band1 = raster::raster(paste0(img.dir66,'/T34JFP_20170620T082011_B01.jp2'))
band2 = raster::raster(paste0(img.dir66,'/T34JFP_20170620T082011_B02.jp2'))
band3 = raster::raster(paste0(img.dir66,'/T34JFP_20170620T082011_B03.jp2'))
band4 = raster::raster(paste0(img.dir66,'/T34JFP_20170620T082011_B04.jp2'))
band5 = raster::raster(paste0(img.dir66,'/T34JFP_20170620T082011_B05.jp2'))
band6 = raster::raster(paste0(img.dir66,'/T34JFP_20170620T082011_B06.jp2'))
band7 = raster::raster(paste0(img.dir66,'/T34JFP_20170620T082011_B07.jp2'))
band8 = raster::raster(paste0(img.dir66,'/T34JFP_20170620T082011_B08.jp2'))
band8A = raster::raster(paste0(img.dir66,'/T34JFP_20170620T082011_B8A.jp2'))
band9 = raster::raster(paste0(img.dir66,'/T34JFP_20170620T082011_B09.jp2'))
band10 = raster::raster(paste0(img.dir66,'/T34JFP_20170620T082011_B10.jp2'))
band11 = raster::raster(paste0(img.dir66,'/T34JFP_20170620T082011_B11.jp2'))
bandTC = raster::raster(paste0(img.dir66,'/T34JFP_20170620T082011_TCI.jp2'))


cat("Mapping PCA 3.........")
#stack1 = rasterPCA(raster::stack(band1,band9,band10))
stack2 = rasterPCA(raster::stack(band2,band3,band4,band8),nComp = 2)
stack3 = rasterPCA(raster::stack(band5,band6,band7,band11),nComp = 2)

# pc = extract(stack1$map,train_points)
# colnames(pc) = c(paste0("stack1_PC",1:3))
pc2 = extract(stack2$map,train_points)
colnames(pc2) = c(paste0("stack2_6_PC",1:2))
pc3 = extract(stack3$map,train_points)
colnames(pc3) = c(paste0("stack3_6_PC",1:2))

trainpca6 = cbind(trainpca6,pc2,pc3)

### test
# pc = extract(stack1$map,test_points)
# colnames(pc) = c(paste0("stack1_5_PC",1:3))
pc2 = extract(stack2$map,test_points)
colnames(pc2) = c(paste0("stack2_6_PC",1:2))
pc3 = extract(stack3$map,test_points)
colnames(pc3) = c(paste0("stack3_6_PC",1:2))
testpca6 = cbind(testpca6,pc2,pc3)


stopCluster(cl)
Sys.time() -  start.time
