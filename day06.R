data06 <- apply(read.table("input/day06.txt", sep = ","), 1, function(x) x[1] + x[2]*1i)

r <- seq(min(Re(data06)), max(Re(data06)))
i <- seq(min(Im(data06)), max(Im(data06)))
a <- rep(r, length(i)) + rep(i, each = length(r)) * 1i


find_min <- function(y) {
  d <- abs(Re(data06 - y)) + abs(Im(data06 - y))
  d2 <- d[d == min(d)]
  if (length(d2) > 1) NA_complex_ else data06[which.min(d)]
}

sort(table(sapply(a, find_min)), decreasing = TRUE)[1]


#part2------------
res2 <- sapply(a, function(y) sum(abs(Re(data06 - y)) + abs(Im(data06 - y))))
length(res2[res2 < 1e4])
