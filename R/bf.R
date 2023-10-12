#' Solution for the knapsack problem
#' @import rlist


#' @export knapsack_brute_force
#' @description
#' knapsack_brute_force search, i.e. going through all possible alternatives and return the maximum value
#'found. This approach is of complexity O(2n) since all possible combinations 2n needs to be evaluated
#' @param x snapsack object as dataframe
#' @param W snapsack size
#' @return result the list with maximum values and elements.
knapsack_brute_force<-function(x, W){
  v <-x$v
  w <-x$w
  n<-length(x$v)
  all_combinations<-matrix(0,ncol = n)
  for (i in 1:2^n) {
    bits<-intToBits(i)
    combination<-c()
    for (j in 1:n) {
      combination <-c(combination,as.numeric(bits[j]))
    }
    #cat("combination",combination,"\n")
    all_combinations <- rbind(all_combinations, combination)
  }
  combination_weight <- list()

  for (i in 1:2^n){
    current_combination <- all_combinations[i,]
    current_weight <-0
    current_value <-0
    for (j in 1:n) {
      if(current_combination[j]==1){
        current_weight <- current_weight + w[j]
        current_value <- current_value + v[j]
      }
    }
    item<-list(list("w"=current_weight,"v"=current_value,"combination"=current_combination))
    combination_weight <- append(combination_weight,item)

  }
  #print(combination_weight)


  #selected_index <- which(combination_weight ==(combination_weight<=W), arr.ind=TRUE)
  max_value<-0
  max_value_item <- NULL
  for (i in 1:length(combination_weight)){
    item <- combination_weight[[i]]
    #cat("item",i,",",item$v)
    #print(item)
    if(item$w<=W){
      if(max_value<item$v){
        max_value<-item$v
        max_value_item <- item
      }
    }
  }
  #list.filter(combination_weight, w <= W)

  #print(combination_weight)
  #max_values <-lapply(combination_weight,max)
  #print(max_value_item)

  rtn_value<- list("value"= c(round(max_value_item$v)),"elements"=which(max_value_item$combination %in% c(1)))
  return(rtn_value)
  #return(combination_weight)
}


