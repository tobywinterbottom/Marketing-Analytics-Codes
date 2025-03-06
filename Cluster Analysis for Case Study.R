#Install necessary packages
install.packages("readxl")
install.packages("tidyverse")
install.packages("cluster")
install.packages("openxlsx")

library(readxl)
library(tidyverse)
library(cluster)
library(openxlsx)

#Importing Data from Excel
SmartWatchData <- read_excel(file.choose())
View(SmartWatchData)

#Initial Data exploration
names(SmartWatchData)
summary(SmartWatchData)
str(SmartWatchData)

#Standardise Data
df <- scale(SmartWatchData)
view(df)


##########Segmentation Step##############

#Calcualting Euclidean Distance
distance <- dist(df, method='euclidean')

#Cluster Dendrogram
#perform hierarchical clustering
hc.w <- hclust(distance, method='ward.D')
#Plot the dendrogram to visualise clustering
plot(hc.w, main="Cluster Dendrogram", xlab="Observation", ylab="Height")

#Determine the optimal number of clusters (using elbow method)
x <- c(1:10)
sort_height <- sort(hc.w$height, decreasing=TRUE)
y <- sort_height[1:10]
#Plot elbow plot
plot(x, y, type="b", main="Elbow Plot", xlab="Cluster", ylab="WCV")
lines(x,y,col="blue")

#After cluster 4 adding more clusters doesn't significantly improve the fit, therefore, we can consider 4 as the optimal number of clusters

#Display 4 clusters on dendrogram 
plot(hc.w, main="Cluster Dendrogram", xlab="Observations", ylab="Height")
rect.hclust(hc.w, k=4, border=2:5)

#Cut Dendrogram into 4 Clusters
cluster <- cutree(hc.w, k=4)
table(cluster)
df_final <- cbind(SmartWatchData, cluster)
view(df_final)


##################Description Step#####################

#Calculate Segment Size
class(df_final)
proportions <- table(df_final$cluster) / nrow(df_final)
percentages <- proportions * 100
#display segment sizes in percentages 
print(percentages)


#explore mean values of variables in each cluster
segments<-
  df_final %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, .names = "{col}_mean"))
segments


#Save Mean Table to Excel 
write.xlsx(segments, 'Smartwatchsegments.xlsx')
getwd()

