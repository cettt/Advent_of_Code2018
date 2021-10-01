data07 <- readLines("input/day07.txt")

f <- sub("^Step (.).*", "\\1", data07)
b <- sub(".*step (.).*", "\\1", data07)
a <- unique(c(b, f))

#part1-------
finished <- c()
while(length(finished) < length(a)) {
  av <- a[sapply(a, function(k) all(f[b == k] %in% finished))]
  finished <- c(finished, sort(setdiff(av, finished))[1])
}

paste(finished, collapse = "")

#part2--------
finished2 <- c()
t_all <- 0
t_next <- 0
w <- c()
w_t <- c()

while(length(finished2) < length(a)) {
  av <- setdiff(sort(a[sapply(a, function(k) all(f[b == k] %in% finished2))]), c(finished2, w))
  if (length(av)) {
    new_w <- sort(av)[seq_len(min(5 - length(w), length(av)))]
    w <- c(w, new_w)
    w_t <- c(w_t - t_next, 60 + sapply(new_w, function(x) which(LETTERS == x)))
  } else w_t <- w_t - t_next
  t_next <- min(w_t)
  t_all <- t_all + t_next
  finished2 <- c(finished2, w[which.min(w_t)])
  w <- w[-which.min(w_t)]
  w_t <- w_t[-which.min(w_t)]
}

t_all
