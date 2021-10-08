data25 <- as.matrix(read.table("input/day25.txt", sep =","))

rem <- seq_along(data25[,1])

cl_list <- list()
n_cl <- 0
while(length(rem) > 0) {
  
  cl <- rem[1]
  j <- 1L
  while (j <= length(cl)) {
    idx <- which(colSums(abs(t(data25) - as.numeric(data25[cl[j],]))) <= 3)
    rem <- setdiff(rem, idx)
    idx <- setdiff(idx, cl)
    cl <- c(cl, idx)
    j <- j + 1L
  }
  n_cl <- n_cl + 1
}

n_cl
