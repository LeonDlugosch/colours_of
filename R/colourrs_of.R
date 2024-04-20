library(png)
library(jpeg)
library(gridExtra)
library(plotrix)
library(ggtern)
library(OpenImageR)
library(grid)
library(Cairo)
library(imager)
library(vegan)

setwd("C:/Users/leond/OneDrive/Bilder/Colours of/ver")
files = dir(pattern = "jpg")
k = 6

for(l in 1:length(files)){
  
  img = readJPEG(files[l])
  dim = dim(img)
  img = resizeImage(image = img, height = ifelse(dim[1] > dim[2], 300, 200), width = ifelse(dim[1] > dim[2], 200, 300))
  
  # reshape image rgb data into a data frame
  df = data.frame(
    red = matrix(img[,,1], ncol=1),
    green = matrix(img[,,2], ncol=1),
    blue = matrix(img[,,3], ncol=1))
  
  ### compute the k-means clustering
  kdist = vegdist(df, method = "euc")
  K = hclust(kdist, method = "ward.D2")
  cut = cutree(K, k = k)
  
  ### calculate cluster colour means
  for (i in 1:k){
    if(i == 1){
      df.2 = df
    }
    means = as.data.frame(round(colMeans(df[which(cut == i),])))
    df.2$red[which(cut == i)] = means[1,1]
    df.2$green[which(cut == i)] = means[2,1]
    df.2$blue[which(cut == i)] = means[3,1]
  }
  
  ### get hex colour values from rgb
  mean.cols = as.data.frame(unique(round(df.2)))/255
  rownames(mean.cols) = 1:k
  k.colous = rgb(mean.cols$red,mean.cols$green,mean.cols$blue)
  cols = rgb(df$red/255,df$green/255,df$blue/255)
  
  
  # perform PCA on the mandril data and add the uv coordinates to the dataframe
  PCA = prcomp(df[,c("red","green","blue")], center=TRUE, scale=TRUE)
  df$u = PCA$x[,1]
  df$v = PCA$x[,2]
  
  
  true = ggplot(df, aes(x=u, y=v, col=cols)) + 
    geom_point(size=2) + scale_color_identity()+
    theme_void()+
    theme(panel.background = element_rect(fill = "transparent", colour = "transparent", size = 0, linetype = 0))
  
  
  kmeans = ggplot(df, aes(x=u, y=v, col=  rgb(df.2$red/255, df.2$green/255, df.2$blue/255))) + 
    geom_point(size=2) + scale_color_identity()+
    theme_void()+
    theme(panel.background = element_rect(fill = "transparent", colour = "transparent", size = 0, linetype = 0))
  
  Cairo(file = paste(gsub(".jpg", "", files[l]), "_colPCA.pdf", sep = ""), 
        type = "pdf",
        bg = "transparent",
        units ="px", 
        width = ifelse(dim[1] > dim[2], 2600, 1000), 
        height = ifelse(dim[1] > dim[2], 1000, 2600), 
        dpi=96)
  grid.arrange(true, kmeans, nrow = ifelse(dim[1] > dim[2], 2, 1)) 
  
}
