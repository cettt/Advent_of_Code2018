data18 <- unlist(read.fwf("Input/day18.txt", rep(1, 50), comment.char = ""))

lookup_fun <- function(k) {
  res <- k + c(0, if (k %% 50 != 1) -1, if (k %% 50 != 0) 1)
  c(res, if (k > 50) res - 50, if (k < 2451) res + 50)
}
lookup <- lapply(seq_along(data18), lookup_fun)

mymap <- c("." = 0L, "|" = 1L, "#" = 10L)[data18]

compute_rv <- function(x) {
  sumvec <- sapply(seq_along(mymap), function(k) sum(mymap[lookup[[k]]]))
  sumvec2 <- sumvec %% 10L
  mymap <<- ifelse(
    mymap == 0L & sumvec2 < 3L, 0L, ifelse(
      mymap == 0L, 1L, ifelse(
        mymap == 1L & sumvec >= 30L, 10L, ifelse(
          mymap == 10L & (sumvec < 20L | sumvec2 == 0L), 0L, mymap
        )
      )
    )
  )
  prod(table(mymap)[-1]) 
}

rv_vec <- sapply(1:600, compute_rv)

#part1------
rv_vec[10]

#part2------
#after a while the pattern repeats itself white a cycle of length k
k <- diff(which(rv_vec == tail(rv_vec)[1]))[1] #cycle length 
rv_vec[max(which(seq_len(600) %% k == (1000000000L %% k)))]
