data18 <- apply(read.table("Input/day18.txt", comment.char = "", sep = ""),
                1, function(x) unlist(unname(strsplit(x, split = ""))))


co <- complex(real = rep(seq_along(data18[1, ]), nrow(data18)), im = rep(seq_along(data18[, 1]), each = ncol(data18)))

mymap <- integer(length(data18))
mymap[as.character(data18) == "."] <- 0L
mymap[as.character(data18) == "|"] <- 1L
mymap[as.character(data18) == "#"] <- 10L

lookup <- lapply(seq_along(mymap), function(k) which(abs(co[k] - co) < 2))

sum_cell <- function(k, themap) sum(themap[lookup[[k]]])

update_map <- function(themap) {
  sumvec <- sapply(seq_along(themap), function(k) sum(themap[lookup[[k]]]))
  sumvec2 <- sumvec %% 10L
  ifelse(
    themap == 0L & sumvec2 < 3L, 0L, ifelse(
      themap == 0L, 1L, ifelse(
        themap == 1L & sumvec >= 30L, 10L, ifelse(
          themap == 1L, 1L, ifelse(
            sumvec >= 20L & sumvec2 > 0L, 10L, 0L
          )
        )
      )
    )
  )
}

#part1------
for (r in 1:10) mymap <- update_map(mymap)

prod(table(mymap)[-1]) 

#part2------
.T <- 1000000000L

#after a while the pattern repeats itself white a cycle of length k
for (r in 1:600) mymap <- update_map(mymap)

k <- 1L
mymap0 <- mymap
rv_vec <- integer(k)
while (TRUE) {
  mymap <- update_map(mymap)
  rv_vec[k] <- prod(table(mymap)[-1])
  if (identical(mymap, mymap0)) break
  k <- k + 1L
} 


rv_vec[which(((611L %% k) + seq_len(k) - 1L) %% k == (.T %% k))]
