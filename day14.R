ll <- as.integer(513401)
x <- integer(ll + 10)
x[1:2] <- c(3L, 7L)
pos <- seq_len(2)
l <- 2L

#part1--------

while(l < ll + 10L) {
  x_new <- sum(x[pos])
  if (x_new >= 10) {
    x[l + 1:2] <- c(1L, x_new - 10L)
    l <- l + 2L
  } else {
    x[l + 1L] <- x_new
    l <- l + 1L
  }
  pos <- (pos + x[pos]) %% l + 1
    
}

paste0(x[-seq_len(ll)][1:10], collapse = "")


#part2-----------------------
x2 <- floor((ll / 10^(5:0))) %% 10

while (any(tail(x, 6) != x2)) {
  x_new <- sum(x[pos])
  if (x_new >= 10) {
    x[l + 1:2] <- c(1L, x_new - 10L)
    l <- l + 2L
  } else {
    x[l + 1L] <- x_new
    l <- l + 1L
  }
  pos <- (pos + x[pos]) %% l + 1
  
}

paste0(l - 6L, collapse = "")
