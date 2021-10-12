data22 <- readLines("input/day22.txt")

depth <- as.integer(gsub("\\D", "", data22[1]))
tar <- as.integer(strsplit(gsub("target: ", "", data22[2]), ",")[[1]])

erro_mat <- matrix(NA_real_, nrow = tar[2] + 20, ncol = tar[1] + 20)
erro_mat[1, 1] <- depth %% 20183
erro_mat[-1, 1] <- ((seq_len(nrow(erro_mat) - 1L) * 48271L) + depth) %% 20183L #x = 0
erro_mat[1, -1] <- ((seq_len(ncol(erro_mat) - 1L) * 16807L) + depth) %% 20183L #y = 0

for (k in 2:nrow(erro_mat)) {
  for (i in 2:ncol(erro_mat)) {
    erro_mat[k, i] <- (erro_mat[k - 1, i] * erro_mat[k, i - 1] + depth) %% 20183
  }
}

erro_mat <- erro_mat %% 3L
erro_mat[tar[2] + 1L, tar[1] + 1L] <- 0L

#part 1-----------
sum(table(erro_mat[seq_len(tar[2] + 1), seq_len(tar[1] + 1)]) * 0:2)

#part2-----------
co_2d <- complex(
  im = rep(seq_along(erro_mat[, 1]) - 1, ncol(erro_mat)),
  re = rep(seq_along(erro_mat[1, ]) - 1, each = nrow(erro_mat)),
)

co <- c(co_2d, co_2d + 1000, co_2d + 2000)

names(co) <- rep(as.integer(erro_mat), 3)
co <- co[as.character(floor(Re(co) / 1000)) != names(co)] #remove invalid squares

find_neighbours <- function(z) { #given a square find all connected squares
  lvl <- floor(Re(z) / 1e3)
  type0 <- erro_mat[c(Im(z) + 1), c(Re(z) + 1 - lvl*1e3)]
  new_lvl <- setdiff(0:2, c(lvl, type0))
  x <- z - lvl*1e3 +  c(1i, -1, 1, -1i) 
  x <- x[Re(x) >= 0 & Im(x) >= 0 & Re(x) < ncol(erro_mat) & Im(x) < nrow(erro_mat)] + lvl*1e3
  type <- diag(erro_mat[c(Im(x) + 1), c(Re(x) + 1 - lvl*1e3)])
  res <- x[type != lvl]
  setNames(c(res, z - lvl*1e3 + new_lvl*1e3), c(rep("1", length(res)), "7")) #name of the square is its distance to z
}

lookup <- lapply(co, find_neighbours)
names(lookup) <- co


find_shortest_path <- function(.from, .to) { #breadth fist search
  queue <- .from
  visited <- complex()
  visited_timevec <- integer()
  step <- 0L
  
  while (TRUE) {
    idx <- step == min(step)
    parent <- queue[idx]
    step_j <- step[idx][1]
    parent <- unique(unname(parent))
    if (.to %in% parent) return(step_j)
    
    visited <- c(parent, visited)
    
    nei <- unlist(unname(lookup[as.character(parent)]))
    nei <- nei[!nei %in% visited]
    
    dist <- as.integer(names(nei))
    idx <- queue %in% parent
    
    step <-  c(step[!idx], dist + step_j)
    queue <- c(queue[!idx], nei)
  }
  return(-1)
}

find_shortest_path(.from = 1000 + 0*1i, .to = sum(tar * c(1, 1i)) + 1e3)
