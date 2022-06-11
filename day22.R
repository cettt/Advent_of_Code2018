data22 <- readLines("input/day22.txt")

depth <- as.integer(gsub("\\D", "", data22[1]))
tar <- as.integer(strsplit(gsub("target: ", "", data22[2]), ",")[[1]])

erro_mat <- matrix(NA_integer_, nrow = tar[2] + 20, ncol = tar[1] + 20)
erro_mat[1, 1] <- depth %% 20183L
erro_mat[-1, 1] <- ((seq_len(nrow(erro_mat) - 1L) * 48271L) + depth) %% 20183L #x = 0
erro_mat[1, -1] <- ((seq_len(ncol(erro_mat) - 1L) * 16807L) + depth) %% 20183L #y = 0

for (k in 2:nrow(erro_mat)) {
  for (i in 2:ncol(erro_mat)) {
    erro_mat[k, i] <- (erro_mat[k - 1, i] * erro_mat[k, i - 1] + depth) %% 20183L
  }
}

erro_mat <- erro_mat %% 3L
erro_mat[tar[2] + 1L, tar[1] + 1L] <- 0L

#part 1-----------
sum(table(erro_mat[seq_len(tar[2] + 1), seq_len(tar[1] + 1)]) * 0:2)

#part2-----------
## the cave can be represented by a matrix (erro_mat) with N entries (N = nrow * ncol)
## the cave with tools can be imagined as three times erro_mat
##    - the first N entries correspond to the cave with no tool equipped (tool == 0L)
##    - the next  N entries correspond to the cave with torch equipped (tool == 1L)
##    - the last  N entries correspond to the cave with climbing gear equipped (tool == 2L).
## Our graph will be an integer vector of length 3*N:
##   note that some vertices are invalid:
##     if a vertex satisfies tool == region that vertex is invalid and should not be in the graph,
##       however this won't be an issue later on.
##   The function find_adjacent finds all adjacent vertices given a current vertex

nr <- nrow(erro_mat)
N <- length(erro_mat)
grph <- seq_len(N * 3L)
target <- N + tar[1]*nr + tar[2] + 1L

find_adjacent <- function(k) {
  
  tool <- as.integer((k - 1L) / N)
  k_2d <- k - N * tool
  reg <- erro_mat[k_2d] #current region
  
  tool_change <- 3L - tool - reg #only works for tool != reg
  m <- k_2d %% nr
  
  n_2d <- k_2d + c(if (k_2d <= N - nr) nr , if (k_2d > nr) -nr, if (m != 1L) -1L, if (m != 0L) 1L)
  n_2d <- n_2d[erro_mat[n_2d] != tool]
  
  c(k_2d + tool_change * N, n_2d + tool * N)
}

q <- collections::priority_queue(1L + N, priorities = 0L)

cost <- rep.int(10000L, length(grph))
cost[1L + N] <- 0L
cur <- 0L

while (cur != target) {
  cur <- q$pop() #current vertex
  adj <- find_adjacent(cur) #adjacent vertices to cur
  new_cost <- cost[cur] + c(7L, rep.int(1L, length(adj) - 1L))
  for (k in seq_along(adj)[new_cost < cost[adj]]) {
    cost[adj[k]] <- new_cost[k]
    q$push(adj[k], priority = -new_cost[k])
  }
}

cost[target]

