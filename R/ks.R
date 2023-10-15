#library(profvis)

#' @title Solution for the knapsack problem by dynamic programming
#' @export knapsack_dynamic
#' @description
#' knapsack_dynamic search, i.e. going through all possible alternatives and return the maximum value
#'found. This approach is of complexity O(2n) since all possible combinations 2n needs to be evaluated
#' @param x knapsack object as dataframe
#' @param W knapsack size
#' @return result the list with maximum values and elements.
#p1 <- profvis({
knapsack_dynamic<-function(x, W){
  v <-x$v
  w <-x$w
  n<-length(x$v)
  mat <- matrix(NA,nrow = n+1,ncol = W+1)

  for (j in 1:(W+1)) {
    mat[1,j] <- 0
  }
  for (i in 1:(n+1)) {
    mat[i,1] <- 0
  }

  for (i in 2:(n+1)) {
    for (j in 1:W) {
      if (w[i-1] > j){
        mat[i, j+1] <- mat[i-1, j+1]
      }else{
        mat[i, j+1] <- max(mat[i-1, j+1], mat[i-1, j+1-w[i-1]] + v[i-1])
      }
    }
  }

  max_value <-max(as.numeric(unlist(mat)))
  current_max_value<-max_value
  max_elements <- c()

  while(current_max_value>0){
    max_value_index<-as.data.frame(which(mat == current_max_value, arr.ind=TRUE))
    current_max_elements <- unique(max_value_index$row-1)
    selected_index <- -1
    current_max_elements<-unlist(lapply(current_max_elements,function(x) x[which(v[x]<=current_max_value)]))
    current_max_elements <- current_max_elements[!current_max_elements %in% max_elements]
    if(length(current_max_elements)>1){
      selected_index <- which(v == max(v[current_max_elements]), arr.ind=TRUE)[1]
    }else if(length(current_max_elements)==1){
      selected_index <- current_max_elements[1]
    }else{
      break
    }
    max_elements <- c(max_elements, selected_index)
    current_max_value <- current_max_value - v[selected_index]
  }

  rtn_value<- list("value"= c(round(max_value)),"elements"=sort(max_elements))
  return(rtn_value)

}


  #a <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#})
#print(p1)
