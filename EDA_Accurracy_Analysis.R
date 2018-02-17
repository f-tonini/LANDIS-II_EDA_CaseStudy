library(sp)
library(raster)
library(rgdal)
library(dismo)

setwd("C:/Users/Chris/Desktop/LandisII_EDA_Example")
# Make a list of directories that can be looped over that can be looped over
dirs = list.dirs("./BigSur_EDA/eda", full.names = TRUE)
#open Original raster (OR) to get extent and projection
OR = raster("./BigSur_EDA/initialcommunitiesmap.img")

# Using visual symptoms
s2006PS = readOGR("./PlotData\\2006DiseasestatusPS.shp")
s2007PS = readOGR("./PlotData\\2007DiseasestatusPS.shp")
s2009PS = readOGR("./PlotData\\2009DiseasestatusPSUB.shp")
s2010PS = readOGR("./PlotData\\2010DiseasestatusPSUB.shp")
s2011PS = readOGR("./PlotData\\2011DiseasestatusPSUB.shp")
s2013PS = readOGR("./PlotData\\2013DiseasestatusPSUB.shp")

# Create a holding data frame for the output
oddR2006 <- data.frame("Number"=0,"OddsRatio1"=0, "pospos" = 0,"posneg"=0, "negpos"=0, "negneg"=0, "total"=0) 
oddR2007 <- data.frame("Number"=0,"OddsRatio1"=0, "pospos" = 0,"posneg"=0, "negpos"=0, "negneg"=0, "total"=0) 
oddR2009 <- data.frame("Number"=0,"OddsRatio1"=0, "pospos" = 0,"posneg"=0, "negpos"=0, "negneg"=0, "total"=0) 
oddR2010 <- data.frame("Number"=0,"OddsRatio1"=0, "pospos" = 0,"posneg"=0, "negpos"=0, "negneg"=0, "total"=0) 
oddR2011 <- data.frame("Number"=0,"OddsRatio1"=0, "pospos" = 0,"posneg"=0, "negpos"=0, "negneg"=0, "total"=0) 
oddR2013 <- data.frame("Number"=0,"OddsRatio1"=0, "pospos" = 0,"posneg"=0, "negpos"=0, "negneg"=0, "total"=0) 

#Loop over dirs to create confusion tables and 
for (i in 1:length(dirs)){
  file6 = list.files(path= dirs[i], pattern="ramorum-16.img", full.names = TRUE)
  file7 = list.files(path= dirs[i], pattern="ramorum-17.img", full.names = TRUE)
  file9 = list.files(path= dirs[i], pattern="ramorum-19.img", full.names = TRUE)
  file10 = list.files(path= dirs[i], pattern="ramorum-20.img", full.names = TRUE)
  file11 = list.files(path= dirs[i], pattern="ramorum-21.img", full.names = TRUE)
  file13 = list.files(path= dirs[i], pattern="ramorum-23.img", full.names = TRUE)

  #open raster that needs shifted, projected and rescaled
  rast6 = raster(file6)
  rast7 = raster(file7)
  rast9 = raster(file9)
  rast10 = raster(file10)
  rast11 = raster(file11)
  rast13 = raster(file13)
  
  #Create raster same size and projection as the Original then set the values to that of the raster of interest
  rast26 = raster(ext=OR@extent, resolution=res(OR),crs=projection(OR))
  rast26[] = getValues(rast6)
  
  rast27 = raster(ext=OR@extent, resolution=res(OR),crs=projection(OR))
  rast27[] = getValues(rast7)
  
  rast29 = raster(ext=OR@extent, resolution=res(OR),crs=projection(OR))
  rast29[] = getValues(rast9)
  
  rast210 = raster(ext=OR@extent, resolution=res(OR),crs=projection(OR))
  rast210[] = getValues(rast10)
  
  rast211 = raster(ext=OR@extent, resolution=res(OR),crs=projection(OR))
  rast211[] = getValues(rast11)
  
  rast213 = raster(ext=OR@extent, resolution=res(OR),crs=projection(OR))
  rast213[] = getValues(rast13)
  
  #Extract the data from the raster to the points
  s2006PS$ModelSOD <- extract(rast26, s2006PS)
  s2007PS$ModelSOD <- extract(rast27, s2007PS)
  s2009PS$ModelSOD <- extract(rast29, s2009PS)
  s2010PS$ModelSOD <- extract(rast210, s2010PS)
  s2011PS$ModelSOD <- extract(rast211, s2011PS)
  s2013PS$ModelSOD <- extract(rast213, s2013PS)

  # Use with other datasets
  actual = as.vector(as.numeric(s2006PS$pram2006))
  model= as.vector(s2006PS$ModelSOD)
  eda <- data.frame("model"= model, "actual"=actual)
  eda$model[eda$model==3]=2
  Pos <- eda[eda$actual==2,]
  Neg <- eda[eda$actual==1,]
  comp <- data.frame(pos=NA, neg=NA)
  Pos$comp = Pos$actual-Pos$model
  Neg$comp = Neg$actual-Neg$model
  comp$neg = sum(Pos$comp)
  comp$pos = nrow(Pos)-sum(Pos$comp)
  comp[2,1] = abs(sum(Neg$comp))
  comp[2,2] = nrow(Neg)-abs(sum(Neg$comp))
  row.names(comp) <- c("actual positive", "actual negative")
  names(comp) <- c("model positive", "model negative")
  comp$rate = c(comp[1,1]/sum(comp[1,])*100,comp[2,2]/sum(comp[2,])*100)
  oddsratio =(comp[1,1]*comp[2,2])/(comp[1,2]*comp[2,1])
  comps = data.frame(c(comp[1,1],comp[1,2],comp[2,1],comp[2,2],nrow(eda)))
  
  oddR2006[i,]= c(file6,oddsratio,comps[1,1],comps[2,1],comps[3,1],comps[4,1],comps[5,1])
  
  actual = as.vector(as.numeric(s2007PS$pram2006))
  model= as.vector(s2007PS$ModelSOD)
  eda <- data.frame("model"= model, "actual"=actual)
  eda$model[eda$model==3]=2
  Pos <- eda[eda$actual==2,]
  Neg <- eda[eda$actual==1,]
  comp <- data.frame(pos=NA, neg=NA)
  Pos$comp = Pos$actual-Pos$model
  Neg$comp = Neg$actual-Neg$model
  comp$neg = sum(Pos$comp)
  comp$pos = nrow(Pos)-sum(Pos$comp)
  comp[2,1] = abs(sum(Neg$comp))
  comp[2,2] = nrow(Neg)-abs(sum(Neg$comp))
  row.names(comp) <- c("actual positive", "actual negative")
  names(comp) <- c("model positive", "model negative")
  comp$rate = c(comp[1,1]/sum(comp[1,])*100,comp[2,2]/sum(comp[2,])*100)
  oddsratio =(comp[1,1]*comp[2,2])/(comp[1,2]*comp[2,1])
  comps = data.frame(c(comp[1,1],comp[1,2],comp[2,1],comp[2,2],nrow(eda)))
  
  oddR2007[i,]= c(file7,oddsratio,comps[1,1],comps[2,1],comps[3,1],comps[4,1],comps[5,1])
  
  actual = as.vector(as.numeric(s2009PS$pram2006))
  model= as.vector(s2009PS$ModelSOD)
  eda <- data.frame("model"= model, "actual"=actual)
  eda$model[eda$model==3]=2
  Pos <- eda[eda$actual==2,]
  Neg <- eda[eda$actual==1,]
  comp <- data.frame(pos=NA, neg=NA)
  Pos$comp = Pos$actual-Pos$model
  Neg$comp = Neg$actual-Neg$model
  comp$neg = sum(Pos$comp)
  comp$pos = nrow(Pos)-sum(Pos$comp)
  comp[2,1] = abs(sum(Neg$comp))
  comp[2,2] = nrow(Neg)-abs(sum(Neg$comp))
  row.names(comp) <- c("actual positive", "actual negative")
  names(comp) <- c("model positive", "model negative")
  comp$rate = c(comp[1,1]/sum(comp[1,])*100,comp[2,2]/sum(comp[2,])*100)
  oddsratio =(comp[1,1]*comp[2,2])/(comp[1,2]*comp[2,1])
  comps = data.frame(c(comp[1,1],comp[1,2],comp[2,1],comp[2,2],nrow(eda)))
  
  oddR2009[i,]= c(file9,oddsratio,comps[1,1],comps[2,1],comps[3,1],comps[4,1],comps[5,1])
  
  actual = as.vector(as.numeric(s2010PS$pram2006))
  model= as.vector(s2010PS$ModelSOD)
  eda <- data.frame("model"= model, "actual"=actual)
  eda$model[eda$model==3]=2
  Pos <- eda[eda$actual==2,]
  Neg <- eda[eda$actual==1,]
  comp <- data.frame(pos=NA, neg=NA)
  Pos$comp = Pos$actual-Pos$model
  Neg$comp = Neg$actual-Neg$model
  comp$neg = sum(Pos$comp)
  comp$pos = nrow(Pos)-sum(Pos$comp)
  comp[2,1] = abs(sum(Neg$comp))
  comp[2,2] = nrow(Neg)-abs(sum(Neg$comp))
  row.names(comp) <- c("actual positive", "actual negative")
  names(comp) <- c("model positive", "model negative")
  comp$rate = c(comp[1,1]/sum(comp[1,])*100,comp[2,2]/sum(comp[2,])*100)
  oddsratio =(comp[1,1]*comp[2,2])/(comp[1,2]*comp[2,1])
  comps = data.frame(c(comp[1,1],comp[1,2],comp[2,1],comp[2,2],nrow(eda)))
  
  oddR2010[i,]= c(file10,oddsratio,comps[1,1],comps[2,1],comps[3,1],comps[4,1],comps[5,1])
  
  actual = as.vector(as.numeric(s2011PS$pram2006))
  model= as.vector(s2011PS$ModelSOD)
  eda <- data.frame("model"= model, "actual"=actual)
  eda$model[eda$model==3]=2
  Pos <- eda[eda$actual==2,]
  Neg <- eda[eda$actual==1,]
  comp <- data.frame(pos=NA, neg=NA)
  Pos$comp = Pos$actual-Pos$model
  Neg$comp = Neg$actual-Neg$model
  comp$neg = sum(Pos$comp)
  comp$pos = nrow(Pos)-sum(Pos$comp)
  comp[2,1] = abs(sum(Neg$comp))
  comp[2,2] = nrow(Neg)-abs(sum(Neg$comp))
  row.names(comp) <- c("actual positive", "actual negative")
  names(comp) <- c("model positive", "model negative")
  comp$rate = c(comp[1,1]/sum(comp[1,])*100,comp[2,2]/sum(comp[2,])*100)
  oddsratio =(comp[1,1]*comp[2,2])/(comp[1,2]*comp[2,1])
  comps = data.frame(c(comp[1,1],comp[1,2],comp[2,1],comp[2,2],nrow(eda)))
  
  oddR2011[i,]= c(file11,oddsratio,comps[1,1],comps[2,1],comps[3,1],comps[4,1],comps[5,1])

  actual = as.vector(as.numeric(s2013PS$pram2006))
  model= as.vector(s2013PS$ModelSOD)
  eda <- data.frame("model"= model, "actual"=actual)
  eda$model[eda$model==3]=2
  Pos <- eda[eda$actual==2,]
  Neg <- eda[eda$actual==1,]
  comp <- data.frame(pos=NA, neg=NA)
  Pos$comp = Pos$actual-Pos$model
  Neg$comp = Neg$actual-Neg$model
  comp$neg = sum(Pos$comp)
  comp$pos = nrow(Pos)-sum(Pos$comp)
  comp[2,1] = abs(sum(Neg$comp))
  comp[2,2] = nrow(Neg)-abs(sum(Neg$comp))
  row.names(comp) <- c("actual positive", "actual negative")
  names(comp) <- c("model positive", "model negative")
  comp$rate = c(comp[1,1]/sum(comp[1,])*100,comp[2,2]/sum(comp[2,])*100)
  oddsratio =(comp[1,1]*comp[2,2])/(comp[1,2]*comp[2,1])
  comps = data.frame(c(comp[1,1],comp[1,2],comp[2,1],comp[2,2],nrow(eda)))
  
  oddR2013[i,]= c(file13,oddsratio,comps[1,1],comps[2,1],comps[3,1],comps[4,1],comps[5,1])
}

oddR <- oddR2006

for (i in 3:7) {
  oddR2006[,i] <- as.numeric(oddR2006[,i])
  oddR2007[,i] <- as.numeric(oddR2007[,i])
  oddR2009[,i] <- as.numeric(oddR2009[,i])
  oddR2010[,i] <- as.numeric(oddR2010[,i])
  oddR2011[,i] <- as.numeric(oddR2011[,i])
  oddR2013[,i] <- as.numeric(oddR2013[,i])
}

oddR[,3:7] <- oddR2006[,3:7]+oddR2007[,3:7]+oddR2009[,3:7]+oddR2010[,3:7]+oddR2011[,3:7]+oddR2013[,3:7]
oddR[,2] <- (oddR[,3]*oddR[,6])/(oddR[,4]*oddR[,5])

#write.csv(oddR, 'C:\\Users\\Chris\\Desktop\\OddsRatioEDAFixed.csv')