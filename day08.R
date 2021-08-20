data08 <- as.integer(strsplit(read.table("input/day08.txt", sep = ",")[,1], " ")[[1]])

#part 1-----------
sum_metadata <- function(idx_sum = c(1, 0)) {
  nm <- data08[idx_sum[1] + 1]
  idx_sum[1] <- idx_sum[1] + 2
  for (k in seq_len(data08[idx_sum[1] - 2])) idx_sum <- sum_metadata(idx_sum) 
  return(c(nm , sum(data08[seq_len(nm) + idx_sum[1] - 1])) + idx_sum)
}

sum_metadata()[2]

#part2----------

value_node <- function(idx = 1L) {
  nc <- data08[idx]
  nm <- data08[idx + 1]
  if (nc == 0) return(sum_metadata(c(idx, 0))[2])
  node_start_idx <- integer(nc)
  idx <- idx + 2L
  for (k in seq_len(nc)) {
    node_start_idx[k] <- idx
    idx <- sum_metadata(c(idx, 0))[1]
  }
  meta <- data08[seq_len(nm) + idx - 1]
  sum(sapply(node_start_idx[meta[meta > 0 & meta <= nc]], value_node), na.rm = TRUE)

}

value_node()
