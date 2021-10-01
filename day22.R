data22 <- readLines("input/day22.txt")

depth <- as.integer(gsub("\\D", "", data22[1]))
tar <- as.integer(strsplit(gsub("target: ", "", data22[2]), ",")[[1]])

erro_mat <- matrix(NA_real_, nrow = tar[2] + 20, ncol = tar[1] + 20)
erro_mat[1, 1] <- depth %% 20183
erro_mat[-1, 1] <- ((seq_len(nrow(erro_mat) - 1) * 48271L) + depth) %% 20183 #x = 0
erro_mat[1, -1] <- ((seq_len(ncol(erro_mat) - 1) * 16807L) + depth) %% 20183 #y = 0

for (k in 2:nrow(erro_mat)) {
  for (i in 2:ncol(erro_mat)) {
    erro_mat[k, i] <- (erro_mat[k - 1, i] * erro_mat[k, i - 1] + depth) %% 20183
  }
}

erro_mat[tar[2] + 1, tar[1] + 1] <- depth %% 20183

#part 1-----------
sum(table(erro_mat[seq_len(tar[2] + 1), seq_len(tar[1] + 1)] %% 3) * 0:2)

#part2-----------
co_2d <- complex(
  im = rep(seq_along(erro_mat[, 1]) - 1, ncol(erro_mat)),
  re = rep(seq_along(erro_mat[1, ]) - 1, each = nrow(erro_mat)),
)

co <- c(co_2d, co_2d + 1000 + 1000*1i, co_2d + 2000 + 2000*1i)

names(co) <- rep(as.numeric(erro_mat) %% 3, 3)
co <- co[as.character(floor(Re(co) / 1000)) != names(co)] #remove invalid squares


make_dist_data <- function(x) {
  lvl <- floor(Re(x) / 1e3) 
  data.frame(from = x, to = c(co[abs(co - x) == 1], x - ((lvl- 2:0)*1e3*(1 + 1i))))
}

make_dist_data2 <- function(x) {
  data.frame(from = x, to = co[abs(co - x) <= 1])
}


mymaze <- subset(do.call(rbind, lapply(co, make_dist_data)), to %in% co & from != to) # takes 1-2 minutes
mymaze <- subset(do.call(rbind, lapply(co, make_dist_data2)), from != to) # takes 1-2 minutes
mymaze$dist <- pmin(abs(mymaze$from - mymaze$to), 7)


start_co <- 1000 + 1000*1i
tar_co <- sum(tar * c(1, 1i)) + start_co

#new bfs approach---------
bfs <- function(.from = start_co) { #breadth fist search
  queue <- .from
  visited <- complex()
  visited_timevec <- integer()
  j <- 1L
  step <- 0L
  while (length(queue) > 0) {
    idx <- step == min(step)
    parent <- queue[idx]
    step_j <- step[idx]
    
    visited <- c(parent, visited)
    visited_timevec <- c(step_j, visited_timevec)
    
    nei <- mymaze[mymaze$from %in% parent & !(mymaze$to %in% visited), ]
    idx <- queue %in% parent
    
    step <-  c(step[!idx], nei$dist + step_j[1])
    queue <- c(queue[!idx], nei$to)
    j <- j + 1
  }
  data.frame(v = visited, time = visited_timevec)
}

mybfs <- bfs() #takes 1-2 minutes
mybfs[mybfs$v == tar_co, 2]
