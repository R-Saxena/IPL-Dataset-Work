#For the unsupervised Learning, I am going to use Kmeans and hierarical Clustering

deliveries = read.csv("data/deliveries.csv")
most_run_avg = read.csv("data/most_runs_average_strikerate.csv")

View(deliveries)
View(most_run_avg)

deliveries$is_four = ifelse(deliveries$batsman_runs == 4, 1,0)
deliveries$is_six = ifelse(deliveries$batsman_runs == 6, 1,0)


players = deliveries %>% 
                     group_by(batsman) %>%
                     summarise(
                             total_four = sum(is_four),
                             total_six = sum(is_six)
                     )

players = merge(most_run_avg,players, by = ('batsman'), x.all = TRUE)

write.csv(players, "Preprocessed Data/players_data.csv")

View(players)

#<-------------------------------- K-Means Clustering----------------------------->

dataset = players[2:8]

#removing all the players having insufficient data
dataset <- na.omit(dataset)

# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

#for me 4 clusters would be better because after 4 the curve has became plane
# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = dataset, centers = 4)
y_kmeans = kmeans$cluster


#visulizing the clusters
library(factoextra)
fviz_cluster(kmeans, data = dataset)



#<---------------------------Hierarchical Clustering--------------------------->

# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances')

#same number of cluster like previous one. but You can try as many as you want by just changing the value of number of clusters in the below line and can visualize the clusters

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 4)

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 0,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of Players'),
         xlab = 'Dim 1',
         ylab = 'Dim 2')
