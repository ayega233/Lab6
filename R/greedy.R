
#' @title Solution for the knapsack problem by greedy
#' @export greedy_knapsack
#' @description
#' greedy_knapsack search, i.e. going through all possible alternatives and return the maximum value
#'found. This approach is of complexity O(2n) since all possible combinations 2n needs to be evaluated
#' @param x knapsack object as dataframe
#' @param W knapsack size
#' @return result the list with maximum values and elements.
greedy_knapsack<-function(x, W){
  v <-x$v
  w <-x$w
  n<-length(x$v)
  value_per_unit_func <- function(x,w,i){
    return(list("index"=i,"wpu"=x[i]/w[i]))
  }
  combination_weight<-lapply(seq_along(v), value_per_unit_func,w=w,x=v)
  combination_weight <- sort(sapply(combination_weight, function(x) x$wpu), index.return=TRUE, decreasing=TRUE)
  max_elements <- c()
  current_max_weight<-W
  running_value <-0.0
  running_weight <-0.0
  for(i in 1:length(combination_weight$ix)){
    if(w[combination_weight$ix[i]]<=current_max_weight){
      max_elements<-c(max_elements,combination_weight$ix[i])
      running_value<- running_value + v[combination_weight$ix[i]]
      current_max_weight <- current_max_weight - w[combination_weight$ix[i]]
      running_weight <- running_weight+w[combination_weight$ix[i]]
    }
  }

  rtn_value<- list("value"= c(round(running_value)),"elements"=max_elements)
  return(rtn_value)

}
#y<-greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
#y
#greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
#greedy_knapsack(x = knapsack_objects[1:1000000,], W = 2000)

