data14 <- as.integer(readLines("input/day14.txt"))

rec <- integer(data14 + 10L)
rec[1:2] <- c(3L, 7L)
pos <- seq_len(2)
j <- 2L

#part1--------

while(j < data14 + 10L) {
  rec_new <- sum(rec[pos])
  if (rec_new >= 10L) {
    rec[j + 1:2] <- c(1L, rec_new - 10L)
    j <- j + 2L
  } else {
    rec[j + 1L] <- rec_new
    j <- j + 1L
  }
  pos <- (pos + rec[pos]) %% j + 1L
    
}

#part1---------
paste0(rec[-seq_len(data14)][1:10], collapse = "")

#part2------
make_traj <- function(i, z) {
  res <- integer()
  k <- 1L
  while (i <= length(z)) {
    res[k] <- i
    i <- i + z[i] + 1L
    k <- k + 1L
    if (is.na(i)) break
  }
  res
}

add_recipe <- function(rec, new_rec) {
  idx <- new_rec > 9L
  idx2 <- which(idx) + seq_len(sum(idx))
  nr <- rep(1L, length(new_rec) + sum(idx))
  nr[seq_along(new_rec) + cumsum(idx)] <- new_rec
  nr[idx2] <- nr[idx2] - 10L
  c(rec, nr)
}

check_target <- function(z) { #check if the condition for part 2 is satisfied
  tar <- as.integer(floor((data14 / 10L^(5:0))) %% 10L)
  check_mat <- lapply(seq_along(tar), function(a) c(z[seq(a, length(z))], rep(0L, a - 1L)) == tar[a])
  which(Reduce(`&`, check_mat))
}

traj <- make_traj(24L, rec) #trajectory starting from 24
relpos1 <- which(traj == max(pos))
dist <- relpos1 - which(traj == min(pos))
check_idx <- 6L

while(TRUE) { 
  while (TRUE) {
    new_rec <- rec[traj[relpos1:length(traj)]] + rec[traj[(relpos1:length(traj)) - dist]]
    rec <- add_recipe(rec, new_rec)
    j <- length(rec)
    relpos1 <- length(traj)
    new_traj <- make_traj(max(traj), rec)[-1]
    
    if (length(new_traj) == 0L) break
    traj[seq_along(new_traj) + length(traj)] <- new_traj
    relpos1 <- relpos1 + 1L
  }
  # at this point relpos1 goes back all the way to the beginning
  pos <- traj[relpos1 - c(0L, dist)]
  pos <- (pos + rec[pos]) %% j + 1L
  relpos1 <- relpos1 - dist + 1L
  
  while (min(pos) < 24L) {
    rec_new <- sum(rec[pos])
    if (rec_new >= 10L) {
      rec[j + 1:2] <- c(1L, rec_new %% 10L)
      j <- j + 2L
    } else {
      rec[j + 1L] <- rec_new
      j <- j + 1L
    }
    pos <- (pos + rec[pos]) %% j + 1L
    # relpos1 <- relpos1 + 1L
  }
  #now pos1 is back to 24 and we can hop onto the trajectory
  res <- check_target(rec[-seq_len(check_idx - 4L)])
  
  if (length(res) > 0L) break
  
  check_idx <- length(rec)
  relpos1 <- which(traj == max(pos))
  dist <- relpos1 - 1L
  new_traj <- make_traj(max(traj), rec)[-1]
  traj[seq_along(new_traj) + length(traj)] <- new_traj
}

res + check_idx - 5L



#part2-----------
# one strategy could be to save the trajectories of pos across x:
#  e.g., every time pos[1] = 2 the same trajectory across x is produced
#     pos[1] = 2 then 10, 16, 23, 24 and so on
#  or pos[1] = 1 then 5, 7, 9, 14 etc
#  or pos[1] = 3 then 5, 7, 9, 14
#  or pos[1] = 4 then 5, 7, 9, 14
#  or pos[1] = 6 then    7, 9, 14
# a simple check shows that no matter what pos[1] %in% c(1,...,10) we choose we always end up at pos[1] = 24 in 5 steps or less 
# so once we now that pos1 == 24 we basically now the remaining trajectory.

