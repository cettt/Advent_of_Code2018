library(collections)
data15 <- unlist(strsplit(readLines("input/day15.txt"), ""))
N <- length(data15)
nr <- as.integer(sqrt(N))

grph <- seq_along(data15)[data15 != "#"]


find_adj <- function(k) {
  m <- k %% nr
  adj <- k + c(if (k <= N - nr) nr , if (k > nr) -nr, if (m != 1L) -1L, if (m != 0L) 1L)
  sort(adj[adj %in% grph])
}

adj_list <- lapply(seq_along(data15), find_adj)


#given the current position and a list of possible attack square what is the next step

find_next_square <- function(.from, .to, blk_pos) {
  .start <- adj_list[[.from]][match(adj_list[[.from]], blk_pos, 0L) == 0L]
  q <- priority_queue(as.list(.start), priorities = -N - .start)
  dist_vec <- rep.int(10000L, length(data15))
  parent_vec <- dist_vec

  dist_vec[.start] <- 1L
  parent_vec[.start] <- .start

  while (q$size() > 0) {
    cur <- q$pop()

    if (any(cur == .to))  {
      next_squares <- .Internal(which(dist_vec == dist_vec[cur] - 1L))
      next_square <- min(next_squares[match(unlist(adj_list[.to]), next_squares, 0L)])
      return(parent_vec[next_square])
    }

    adj <- adj_list[[cur]]
    adj <- adj[match(adj, blk_pos, 0L) == 0L]
    for (a in adj[dist_vec[cur] + 1L < dist_vec[adj]]) {
      parent_vec[a] <- parent_vec[cur]
      dist_vec[a] <- dist_vec[cur] + 1L
      q$push(a, priority = -dist_vec[a]*N - parent_vec[cur])
    }
  }

  return(.from) #if none of .to was reached we stay at .from

}



cd0 <- data.frame(
  id = seq_along(data15[data15 %in% c("E", "G")]),
  pos = .Internal(which(data15 %in% c("E", "G"))),
  eg = data15[data15 %in% c("E", "G")],
  hp = 200L
)


#simulate battle---------
simulate_battle <- function(ea = 3L, ga = 3L, cd = cd0, part1 = TRUE) {
  cd$ap <- ifelse(cd$eg == "G", ga, ea)
  rounds <- 0L
  while (TRUE) {
    for (j in seq_along(cd[, 1])) {
      if (cd$hp[j] > 0L) { #if the current party has positive hp
        eg_j <- cd$eg[j]
        all_e <- cd[cd$eg != eg_j & cd$hp > 0L, ] #all enemy positions

        if (all(match(all_e$pos, adj_list[[cd$pos[j]]], 0L) == 0L)) { #if current player is not adjacent to an enemy
          blk_pos <- cd[cd$hp > 0L & cd$eg == eg_j, "pos"] # positions that cannot be reached because it is blocked by a team-member
          cd$pos[j] <- find_next_square(cd$pos[j], .to = all_e$pos, blk_pos)
        }

        att_en <- all_e[match(all_e$pos, adj_list[[cd$pos[j]]], 0L) != 0L, ]
        if (nrow(att_en) > 0L) {
          att_id <- att_en[order(att_en$hp, att_en$pos), ][1, c("id", "hp")]
          cd$hp[cd$id == att_id$id] <- cd$hp[cd$id == att_id$id] - cd$ap[j]
          if (!part1 & eg_j == "G" & att_id$hp <= ga) return(-1)
          if (all(cd$hp[cd$eg != eg_j] <= 0L)) return(rounds * sum(pmax(cd$hp, 0L)))
        }
      }
    }

    rounds <- rounds + 1L
    cd <- cd[cd$hp > 0L, ]
    cd <- cd[order(cd$pos), ]
  }
}


#part1----
simulate_battle() #222831

#part2-----
res <- -1L
ea0 <- 12L #since one elf is attacked by four goblins this is the minimum ea it needs to survive

while (res == -1L) {
  res <- simulate_battle(ea = ea0, part1 = FALSE) #54096
  ea0 <- ea0 + 1L
}
res
