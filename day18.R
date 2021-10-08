data18 <- apply(read.table("Input/day18.txt", comment.char = "", sep = ""),
                1, function(x) unlist(unname(strsplit(x, split = ""))))


co <- complex(real = rep(seq_along(data18[1, ]), nrow(data18)), im = rep(seq_along(data18[, 1]), each = ncol(data18)))

mymap <- integer(length(data18))
mymap[as.character(data18) == "."] <- 0L
mymap[as.character(data18) == "|"] <- 1L
mymap[as.character(data18) == "#"] <- 10L

lookup <- lapply(seq_along(mymap), function(k) which(abs(co[k] - co) < 2))


update_map <- function(k, themap) {
  z <- themap[k]
  n <- sum(themap[lookup[[k]]])
  n2 <- n %% 10L #number of trees
  if (z == 0L & n2 >= 3L) 1L else if (z == 1L & n >= 30L) 10L else if (z == 10L & (n <= 19L | n2 == 0L)) 0L else z  
}

#part1------
for (r in 1:10) {
  mymap <- sapply(seq_along(mymap), update_map, themap = mymap)
}

prod(table(mymap)[-1]) 

#part2------
.T <- 1000000000L

#after a while the pattern repeats itself white a cycle of length k
for (r in seq_len(600)) mymap <- sapply(seq_along(mymap), update_map, themap = mymap)

k <- 1L
mymap0 <- mymap
rv_vec <- integer(k)
while (TRUE) {
  mymap <- sapply(seq_along(mymap), update_map, themap = mymap)
  rv_vec[k] <- prod(table(mymap)[-1])
  if (identical(mymap, mymap0)) break
  k <- k + 1L
} 


rv_vec[which(((611L %% k) + seq_len(k) - 1L) %% k == (.T %% k))]
