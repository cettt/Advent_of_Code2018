data11 <- as.integer(read.table("input/day11.txt"))

calculate_pow <- function(x, y) {
  z <- (floor(((x + 10L) * y + data11) * (x + 10L) / 100L)) %% 10L - 5L
}

pow_field <- outer(seq_len(300), seq_len(300), calculate_pow)

run_cumsum <- function(x, n) {
  y <- cumsum(x)
  (y - c(rep(0, n), y[seq_len(300L - n)]))[-seq_len(n - 1L)]
}

find_field <- function(n, part1 = TRUE) {
  pf <- apply(apply(pow_field, 1, run_cumsum, n = n), 1, run_cumsum, n = n)
  
  res <- paste(which(pf == max(pf), arr.ind = TRUE)[1,], collapse = ",")
  if (part1) res else c(max(pf), paste(res, n, sep = ","))
}

#part1-----
find_field(3)

#part2----------
a <- sapply(2:299, find_field, part1 = FALSE)
a[2, which.max(as.integer(a[1, ]))]
