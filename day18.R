data18 <- apply(read.table("Input/day18.txt", comment.char = "", sep = ""),
                1, function(x) unlist(unname(strsplit(x, split = ""))))


co <- complex(real = rep(seq_along(data18[1, ]), nrow(data18)), im = rep(seq_along(data18[, 1]), each = ncol(data18)))

mymap <- as.character(data18)
lookup <- lapply(seq_along(mymap), function(k) which(abs(co[k] - co) < 2))


update_map <- function(k, themap) {
  z <- themap[k]
  n <- sum(c("|" = 1, "#" = 10, "." = 0)[themap[lookup[[k]]]])
  n2 <- n %% 10 #number of trees
  if (z == "." & n2 >= 3) "|" else if (z == "|" & n >= 30) "#" else if (z == "#" & (n <= 19 | n2 == 0)) "." else z  
}

#part1------
for (r in 1:10) {
  mymap <- sapply(seq_along(mymap), update_map, themap = mymap)
}

prod(table(mymap)[-2]) #614812

#part2------
.T <- 1000000000
mymap <- as.character(data18)

#after a while the pattern repeats itself after 28 steps
for (r in 1:600) {
  mymap <- sapply(seq_along(mymap), update_map, themap = mymap)
}
rv0 <- prod(table(mymap)[-2])

rv_vec <- integer(28)
for (r in 1:28) {
  mymap <- sapply(seq_along(mymap), update_map, themap = mymap)
  rv_vec[r] <-  prod(table(mymap)[-2])
}


rv_vec[which(((601 %% 28) + 0:27) %% 28 == (.T %% 28))]
