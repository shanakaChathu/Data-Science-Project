#check cluster tendency
library(ggplot2)
library(factoextra)
library(caTools)
library(clustertend)
library(seriation)
library(cluster)
library(clValid)


#load csv 
df=read.csv("dataAnalyse.csv")
df[is.na(df)]<-0
df=as.data.frame(df[,c(20,22,27,29,31,32,33,34)])
data=df


#Clustering tendency
data1=df[sample(nrow(df), 1000), ]
n=nrow(data1)
hopkins(data1, n = nrow(data1)-1)


#Select best algorithm to cluster
exprs=scale(as.data.frame(df[sample(nrow(df), 550), ]))
clmethods = c("hierarchical","kmeans","pam","model")
intern = clValid(exprs, nClust = 2:6, clMethods = clmethods, validation = "internal")

# Summary
summary(intern)
plot(intern)

#Selecting number of clusters 
data1=df[sample(nrow(df), 10000), ]
fviz_nbclust(data1, hcut, method = "wss")+geom_vline(xintercept = 5, linetype = 2)


#fitting the model
d=dist(data, method = "euclidean") 
fitw = hclust(d, method="ward.D2")
plot(fitw, hang = -1,cex = 0.6)
# Add rectangle around 3 groups
rect.hclust(fitw, k = 4, border = 2:6) 
rect.hclust(fitw, k=4, border="red")

hww= cutree(fitw, k=5)# cut tree into 5 cluster
aggregate(cent[,c(2,3,4,5)],by=list(hww),FUN=mean)

#cluster plot
library(factoextra)
fviz_cluster(list(data = data, cluster = hww))


df$clust=hww

write.csv(df,"data.csv")









