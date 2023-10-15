#' Solution for the knapsack problem
#' @import rlist
#' @import parallel

#' @title Solution for the knapsack problem by brute force
#' @export brute_force_knapsack
#' @description
#' knapsack_brute_force search, i.e. going through all possible alternatives and return the maximum value
#'found. This approach is of complexity O(2n) since all possible combinations 2n needs to be evaluated
#' @param x knapsack object as dataframe
#' @param W knapsack size
#' @param parallel enable disable parallel processing
#' @return result the list with maximum values and elements.

brute_force_knapsack<-function(x, W,parallel=FALSE){
  v <-x$v
  w <-x$w
  n<-length(x$v)

  evaluation_func <-function(itor,w,v){
    combination_weight <- list()
    n<-length(v)
    #binary representation of the item
    bits<-intToBits(itor)
    combination<-c()
    for (j in 1:n) {
      combination <-c(combination,as.numeric(bits[j]))
    }

    current_combination <- combination
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
    return(combination_weight)
  }
  if(parallel){
    print("Parallel Execution"  )
    cl <- makeCluster(parallel::detectCores(), type = "PSOCK")
    combination_weight<-parLapply(cl,c(1:2^n), evaluation_func,w=w,v=v)
    stopCluster(cl)
  }else{
    combination_weight<-lapply(c(1:2^n), evaluation_func,w=w,v=v)
  }

  max_value<-0
  max_value_item <- NULL
  for (i in 1:length(combination_weight)){
    item <- combination_weight[[i]][[1]]
    if(item$w<=W){
      if(max_value<item$v){
        max_value<-item$v
        max_value_item <- item
      }
    }
  }

  rtn_value<- list("value"= c(round(max_value_item$v)),"elements"=which(max_value_item$combination %in% c(1)))
  return(rtn_value)
}
