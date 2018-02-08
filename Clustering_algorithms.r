# Load the data in your R environment
# This should give you dataframe "cluster.data" 
load("cluster.RData")

###### Part 1: Distance Calculation ######
# TASK 1. Writing a function for distance calculation:

# parameters "a" and "b" are two vectors 
# Manhattan distance is calculated between "a" and "b" when metric = "manhattan"
# otherwise Euclidean distance is calculated and returned by the function
#*************************KLAR 4P******************************************

my_dist_calculator <- function(a, b, metric){
  
  if(metric == "manhattan"){
    # write code for calculating manhattan distance
    distance <- abs(a[1]-b[1]) + abs(a[2]-b[2])

    
    
  }else if(metric == "euclid") {
    # write code for calculating euclidean distance
    distance = as.double(distance <- dist(cbind(a,b)))
  
  }
  
 return(distance) 
}


###### K-Means Clustering #######
# Write a function for performing K-Means clustering

# Function takes parameter "x" as dataframe to be clustered
# "k" as a number of clusters and "max.iter" as maximum allowed iteration before convergence
# Use the Euclidean distance calculator from part 1.

k_means <- function(x, k, max.iter = 20){
  
  # assign each observation in x a random cluster number (id) from 1 to k
  random_index <- sample(1:k, nrow(x), replace = TRUE)
  
  # add a new column "cluster" to dataset (x) that carries cluster id for each observation
  # we have a new dataset with random cluster id assigned to each observations 
  data_with_cluster <- cbind(x, clusterID = random_index)
  print(data_with_cluster)
  # initialize number of iteration counts to stop iterating when maximum limit is reached
  iterations = 1
  
  # plot the points
  plot(data_with_cluster[,1:2])
  
  # this block of code tries to adjust the centroids until they cannot be changed anymore
  # interate until stopping critera are met
  while(TRUE){
    
    # create a new object "centroids" to hold the mean (centroid) of each cluster
    # It is a matrix whose number of rows equals to k
    # and number of columns equals number of columns in original data (x)
    # initialize this matrix with 0 values
    centroids <- matrix(rep(0, times = k * ncol(x)), nrow = k, ncol = ncol(x))
    
    # compute the mean of each cluster and store them in "centroids"
    for(i in 1:k){
      # find observations of cluster i
      obs_of_cluster_i <- (data_with_cluster$clusterID == i)

      #colMeans does not work..
      #test_new <- colMeans(newDataX,na.rm = TRUE)
      
      #the_point <- c(test_new[["x"]],test_new[["y"]])
      x_value <- mean(data_with_cluster[obs_of_cluster_i,]$x)
      y_value <- mean(data_with_cluster[obs_of_cluster_i,]$y)
      
      
      centroids[i,] <- cbind(x_value,y_value)

    }
    #print("Initial centroids")
    #print(centroids)
    
    # ---------------------------------------------- #
    # plots the centroids discovered in each iteration
    points(centroids[,1:2], pch = 20, cex = 2)
    # waits until some charater is fed
    # done to be able to see the change in centroids
    readline(prompt = "Press Enter to continue:")
    # ---------------------------------------------- #
    
    # calculate a distance of each point in dataset x to each of k cluster centroid
    # distance measured are stored in a new matrix.
    # Dimension of this new matrix is such that rows equal to total number of observations in x
    # and columns equal to number of clusters k.
    # First column stores distance of each point to first cluster centroid, 
    # second column stores distance of each point to second cluster centroid and so on.
    # initlaize this matrix with 0 values
    dist_from_centroids <- matrix(rep(0, nrow(x) * k), nrow = nrow(x), ncol = k)
    
    # Compute the distance between each centroid and each point 
    
    for(i in 1:nrow(x)){
      for(j in 1:nrow(centroids)){
        # Use the euclidean distance calculation function written in TASK 1.
        point_val = c(x$x[i],x$y[i])
        #print(point_val)
        
        dist_from_centroids[i,j] <- my_dist_calculator(point_val,centroids[j,],"euclid")
        
      }
    }
    
    print("New Centroids")
    print(centroids)
    
    #cat("Dist from centroids",dist_from_centroids)
    # from the distance matrix computed, find for each observation the closest cluster centroid
    obs_new_clusterID <- apply(dist_from_centroids, 1, which.min)
    
    # If the centroid is not changing any more for each observation, stop the iteration
    if(all(obs_new_clusterID == data_with_cluster$clusterID)){ 
      km.clusters <- obs_new_clusterID
      centroid.matrix <- centroids
      break
      # If number of iterations exceed the maximum iterations allowed, stop the iteration
    }else if(iterations > max.iter){
      break
      
      # Otherwise, assign the new centroids to the dataset, and continue the iteration
    }else{ 
      data_with_cluster$clusterID <- obs_new_clusterID
      iterations <- iterations + 1
    }
  }
  
  # Plot the final cluster after iteration stops
  plot(data_with_cluster[,1:2], col = data_with_cluster$clusterID)
  points(centroid.matrix[,1:2], pch = 20, cex = 2, col = 1:k)
  
  # when iterations are done, return clusters assigned and final centroid
  return(list("clusters" = km.clusters, "centroids" = centroid.matrix))
}

# call the K-Means function we just created
km_clusters <- k_means(cluster.data, k = 3, max.iter = 15)

# look into the object returned by our function
print(km_clusters)