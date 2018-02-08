############# Perceptron Learning Algorithm ###############

Download "Perceptron_data.RData" file and load it in R environment
# We will train perceptron algorithm on this data (perceptron.data)

load("Perceptron_data.RData")
head(perceptron.data)

# plot the perceptron.data
# we need to find a plane that separates green from red points
plot(perceptron.data[,1:2], xlim = c(-1,1), ylim = c(-1,1), col = ifelse(perceptron.data$y == -1, "red", "green"))


# Function that implements Perceptron learning algorithm 
# Arguments: data.X is a dataset with all attributes but class attribute
#            data.Y is a vector of class labels
#            max.iter is a maximum number of allowed iterations
# -------------------------------------------------------------
my.perceptron <- function(data.X, data.Y, max.iter = 500){
  
  # Add X0, which is 1 for all examples
  X.new <- cbind(1, data.X) 
  
  # Initial weight vector with all 0 elements, note that the first element is w0
  w <- matrix(0, 1, ncol(X.new))
  
  # track itertaions, counter for number of iterations
  iterations <- 0
  
  # matrix to keep track of weight changes
  weight.trace <- X.new[numeric(0),]
  
  
  
  while(TRUE){
    # use matrix multiplication of X.new and w to calculate the hypothesis.
    
    # *** ATTENTION !!! *** #
    # Must be converted to matrix before calculating cross product
    convertedMatrix <- as.matrix(sapply(X.new,as.numeric))
    colnames(convertedMatrix) <- NULL
    hypothesis <- convertedMatrix %*% t(w)

    print(hypothesis)

    # use the sign of hypothesis to predict new class label for each observation
    # if hypothesis is positive (greater or equal to zero) assign it a class 1 else -1
    label.new <- ifelse(hypothesis >= 0, 1, -1) 
    
    # if the new class labels from hypothesis are same as the true class labels (data.Y) 
    # for all observations, then stop iterating
    if(all(label.new == data.Y)){ 
      
      #return(list("final.weight" = w, "weight.trace" = weight.trace))
      # return(w)
      break
      
      # if number of iterations exceeds maximum limit, stop iterating
    }else if(iterations >= max.iter){
      
      # return(w)
      break 
      
    }else{ 
      # if the new class labels from hypothesis are not the same with the true class label, 
      # update the weight vector and continue the iteration. 
      
      # index for those that had incorrect predictions
      row.index.wrong.pred <- (label.new != data.Y)
      
      # randomly select one misclassified point
      set.seed(5)
      wrong.predicted.idx <- which(row.index.wrong.pred == TRUE)
      rand.sel.index <- sample(wrong.predicted.idx, 1)
      
      # update the weight vector using this randomly selected misclassified point
      iterations <- iterations + 1
      
      # *** ATTENTION !!! *** #
      w <- (w + convertedMatrix[rand.sel.index,] * data.Y[rand.sel.index])
      
      # save the intermediate weights to see the changes in weight vector after each iteration 
      weight.trace <- rbind(weight.trace, w)
    }   
  }
  
  # remove the row names
  rownames(weight.trace) <- NULL
  
  return(list("final.weight" = w, "iterations" = iterations, "weight.trace" = weight.trace))
} 

# use our function that implements perceptron algorithm 
model <- my.perceptron(data.X = perceptron.data[,1:2], data.Y = perceptron.data[,3])

# see the final value of weight vector
print(model$final.weight)
#  Values for final weight should be, (1, -5.829599, 8.167969)

# how many iterations it took to converge
print(model$iterations)

# plot the final splitting hyperplane discovered by the algorithm
plot(perceptron.data[,1:2], xlim = c(-1,1), ylim = c(-1,1), col = ifelse(perceptron.data$y == -1, "red", "green"), pch = 20)
intercept.final <- -(model$final.weight[1])/model$final.weight[3]
slope.final <- -(model$final.weight[2])/model$final.weight[3]
abline(as.numeric(intercept.final), as.numeric(slope.final), lwd = 2)

# plot split planes and see how the plane adjust at each iteration
# and gets closer and closer to the correct spliting hyperplane
plot(perceptron.data[,1:2], xlim = c(-1,1), ylim = c(-1,1), col = ifelse(perceptron.data$y == -1, "red", "green"), pch = 20)
for(i in 1:model$iterations){
  intercept.i <- -(model$weight.trace[i,1])/model$weight.trace[i,3]
  slope.i <- -(model$weight.trace[i,2])/model$weight.trace[i,3]
  abline(intercept.i, slope.i, col = "blue")
  Sys.sleep(1)
  if(i == model$iterations)
    abline(intercept.i, slope.i,lwd = 3)
}
