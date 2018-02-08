## ----------------------------------- ##
## Function to calculate entropy
## ----------------------------------- ##
# This function takes class distribution and calculates entropy
# Argument: class.freq, number of observations belonging to differnt class categories 
# example: for class attribute "buy", class.freq would be
#         no yes 
#         5   9 
# returns: entropy of a node (assuming that all observations are members of a tree node)

getEntropy <- function(class.freq){
  
  # First convert class frequency into class probabilities (p) by dividing 
  # frequencies by total number of observations
  p <- class.freq/sum(class.freq)
  
  # Use formula to compute entropy using class probabilities "p"
    
  entropy <- -sum(p * log2(p))
  
  # Probability p = 0 creates NaN value
  # we need to remove such value before computing sum
  # is.nan() returns TRUE if value is NaN
  entropy_final <- sum(entropy[!is.nan(entropy)])
  
  return(entropy_final)
}

# call the function to compute entropy, using class distribution of given data
getEntropy(table(entropy.data$buys))
# 0.940286

# ------------------------------------------------------------------- #
# This function computes information gain when a node is split on a given attribute
# Arguments: split_var = attribute column to be evaluated to split examples of a node
#            class_var = vector of class labels
# returns:   information_gain
# ------------------------------------------------------------------- #
getInfoGain <- function(split_var, class_var){
  
  class.freq <- as.matrix(table(class_var))
  
  # Calculate entropy of this parent node using class.freq
  entropy_parent <- getEntropy(class.freq)
  
  # We want to split this parent node using an attribute (e.g. income).
  # Create contingency table using the attribute and class label
  attr.class.freq <- as.matrix(table(split_var, class_var))
  
  # For example: attr.class.freq for attribute "income" and class "buys"
  #         no yes
  # high    2   2
  # low     1   3
  # medium  2   4
  
  # We investigate the split with value of income as "high", "low" and "medium"
  # First row of attr.class.freq in example above gives class distribution of examples that have high income
  
  # Intialize the overall entropy of split to 0.
  entropy_split <- 0
  num_observations <- length(split_var)
  
  # First, calculate entropy of each child node
  # e.g. "high", "low" and "medium" income results in three child nodes that contain 
  # observations with respective income type.
  for(i in 1:nrow(attr.class.freq)){
    
    # each row in attr.class.freq is class frequency of one node
    # For example: class distribution of child node with "high" income
    #         no yes
    # high    2   2 
    
    # *** ATTENTION !!! *** #
    child.node.freq <- attr.class.freq[i,]
 
    # In order to find entropy of the split, 
    # first calculate entropy in each child node
    child.node.entropy <- getEntropy(child.node.freq)
    
    
    # number of examples in this child node
    num_obs_child.node <- sum(child.node.freq)
    # Add child node entropies weighted by fraction of examples they receive from parent node.
    
    # *** ATTENTION !!! *** #
    entropy_split <- entropy_split + child.node.entropy * (num_obs_child.node / num_observations)
  }
  # print(entropy_split)
  # Information gain is difference between parent node entropy and split entropy
  information_gain <- (entropy_parent - entropy_split)
  
  return(information_gain)
}


# test the function on categorical attributes, income, student and credit
getInfoGain(entropy.data$income, entropy.data$buys)
# 0.02922257
getInfoGain(entropy.data$student, entropy.data$buys)
# 0.1518355
getInfoGain(entropy.data$credit, entropy.data$buys)
# 0.04812703



# ---------------------------------------------------------------------- #
# Function that finds best attribute to split a given tree node
# Numeric attribute needs to be discretized and changed to categorical 

# Arguments: data.X = dataframe without class attribute
#            data.Y = vector of class labels

# Attribute that gives highest information gain on split is selected
# --------------------------------------------------------------------- #

findBestAttribute <- function(data.X, data.Y){
  
  number_of_columns <- ncol(data.X)
  attribute_names <- colnames(data.X)
  info_gains <- numeric(number_of_columns)
  best_cutoffs <- rep(NA, number_of_columns)
  
  for(i in 1:number_of_columns){
    
    # If an attribute is numeric, it has to be discretized (changed into categorical)
    if(class(data.X[,i]) == "numeric"){
      
      # find all the numeric values, order them in ascending order and select unique values
      x_sort_unique <- unique(sort(data.X[,i]))
      
      
      # for any two consecutive numbers, find the midpoint and use this midpoint to discretize
      # All observations with value for this attribute less than this midpoint gets "YES" category while rest gets "NO" 
      # Once all observations have value either "YES" or "NO", it can be treated as categorical
      
      max_info_gain_this_var <- 0
      best_cutoff_point <- NA
      
      for(j in 1:(length(x_sort_unique)-1)){
        
        # Calculate midpoint betweent two consecutive numeric values of numeric variable
        
        # *** ATTENTION !!! *** #
        cutoff_point <- (x_sort_unique[j] + x_sort_unique[j+1]) / 2
        print(cutoff_point)
        
        
        # Categorize all observations with numeric value less than cutoff as "YES" else "NO" 
        lessthan_cutoff <- ifelse(data.X[,i] <= cutoff_point, "YES", "NO")
        print(lessthan_cutoff)
        # Compute information gain on descritized numeric variable (now categorical) "lessthan_cutoff"
        
        # *** ATTENTION !!! *** #
        new <- as.factor(lessthan_cutoff)
        info_gain_for_this_split <- getInfoGain(data.X[,i], new)
        
        # If information gain is bettr than previous information gain, set it as new best
        if(info_gain_for_this_split > max_info_gain_this_var){
          max_info_gain_this_var <- info_gain_for_this_split
          best_cutoff_point <- cutoff_point
        }
      }
      info_gains[i] <- max_info_gain_this_var
      best_cutoffs[i] <- best_cutoff_point
      
    }else{ 
      # If attribute is categorical, simply use contingency table to calculate info gain calling getInfoGain function
      info_gains[i] <- getInfoGain(data.X[,i], data.Y)
    }
  }
  
  return_df <- data.frame("Attributes"=attribute_names, "Info.Gain" = info_gains, "Cutoff.at" = best_cutoffs)
  
  return(return_df) 
}


# Call the function that calculates information gain for each attribute
# ---------------------------------------------------------------------
attr_info_gains <- findBestAttribute(data.X = entropy.data[,1:4], data.Y = entropy.data[,5])
print(attr_info_gains)