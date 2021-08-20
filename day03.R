data03 <- read.table("input/day03.txt", comment.char = "", sep = "@")

data03 <- data.frame(v1 = 1:3, v2 = c(" 1,3: 4x4", " 3,1: 4x4", " 5,5: 2x2"))

make_square <- function(x) {
 s <- as.integer(sub(" (\\d+),.*", "\\1", x)) + as.integer(sub(".*,(\\d+):.*", "\\1", x))*1i
 w <- as.integer(sub(".* (\\d+)x.*", "\\1", x))
 h <- as.integer(sub(".*x(\\d+)", "\\1", x))
 rep(Re(s) + seq_len(w) - 1, h) + rep(Im(s) + seq_len(h) - 1, each = w)*1i
}

#part1-------
squares <- sapply(data03, make_square)
sum(table(unlist(squares)) > 1)

#part2---------
data03$x1 <- as.integer(sub(  " (\\d+),.*", "\\1", data03[,2]))
data03$y1 <- as.integer(sub(".*,(\\d+):.*", "\\1", data03[,2]))
data03$x2 <- data03$x1 + as.integer(sub(".* (\\d+)x.*", "\\1", data03[,2])) - 1
data03$y2 <- data03$y1 + as.integer(sub(".*x(\\d+)", "\\1", data03[,2])) - 1

check_nointersect <- function(i) {
  all(
    (data03$x2[-i] < data03$x1[i] | data03$x1[-i] > data03$x2[i]) | 
    (data03$y2[-i] < data03$y1[i] | data03$y1[-i] > data03$y2[i])
  )
}


data03[which(sapply(seq_len(nrow(data03)), check_nointersect)), 1]
