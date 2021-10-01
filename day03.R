data03 <- read.table("input/day03.txt", comment.char = "", sep = "@")

x1 <- as.integer(sub(  " (\\d+),.*", "\\1", data03[,2]))
y1 <- as.integer(sub(".*,(\\d+):.*", "\\1", data03[,2]))
x2 <- x1 + as.integer(sub(".* (\\d+)x.*", "\\1", data03[,2])) - 1
y2 <- y1 + as.integer(sub(".*x(\\d+)", "\\1", data03[,2])) - 1

#part1---------
make_square <- function(i, j) {
  xd <-  max(x1[c(i, j)])
  xu <-  min(x2[c(i, j)])
  yd <-  max(y1[c(i, j)])
  yu <-  min(y2[c(i, j)])
  rep(seq(xd, xu), yu - yd + 1) + rep(seq(yd, yu), each = xu - xd + 1)*1i
}

check_intersect <- function(i) {
  idx <- which((x2 >= x1[i] & x1 <= x2[i] & y2 >= y1[i] & y1 <= y2[i]))
  unlist(sapply(idx[idx > i], function(j) make_square(i, j)))
}

length(unique(unlist(sapply(seq_along(x1), check_intersect))))


#part2---------
check_nointersect <- function(i) {
  all(x2[-i] < x1[i] | x1[-i] > x2[i] | y2[-i] < y1[i] | y1[-i] > y2[i])
}

which(sapply(seq_along(x1), check_nointersect))
