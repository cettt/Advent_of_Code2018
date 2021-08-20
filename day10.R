data10 <- readLines("input/day10.txt")

s2c <- function(x) as.numeric(x[1]) + as.numeric(x[2]) * 1i

z <- sapply(strsplit(substr(data10, 11, 24), ", "), s2c) 
v <- sapply(strsplit(substr(data10, 37, 42), ", "), s2c)
sec <- 0L

while (diff(range(Im(z))) > 10) {
  sec <- sec + 1L
  z <- z + v
}

#part1-------
plot(
  Re(z), -Im(z), pch = 15, cex = 1.8, ylim = range(-Im(z))*c(1.02, 0.95),
  col = "hotpink2", xaxt= "n", yaxt = "n", xlab = "", ylab = "", bty = "n"
)
#EKALLKLB

#part2------
sec

