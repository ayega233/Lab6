##old sampler used for backward compatibility
suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

knapsack_objects2 <-
  data.frame(
    w=c(4,3,2,1),
    v=c(5,4,3,2)
  )

knapsack_objects3 <-
  data.frame(
    w=c(1,2,3,4),
    v=c(5,4,3,2)
  )

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


