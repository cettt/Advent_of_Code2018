data15 <- do.call(rbind, strsplit(readLines("input/day15.txt"), ""))

co <- apply(which(data15 != "#", arr.ind = TRUE), 1, function(z) z[2] + z[1]*1i)
tile <- setNames(data15[data15 != "#"], co)

#maze functions----------
sort_co <- function(x) x[order(Im(x), Re(x))]
co <- sort_co(co)


find_next_square <- function(.from, .to, maze) { #given the current position and a list of possiible attack square what is the next step
  queue <- .from
  parent_vec <- NA_complex_
  step <- 0L
  j <- 1L
  
  while (TRUE) { #start bfs
    ne_vec <- complex()
    for (q in queue[step == max(step)]) {
      new_edge <- setdiff(maze[abs(q - maze) == 1], queue)
      ne_vec <- c(ne_vec, new_edge)
      queue <- c(queue, new_edge)
      parent_vec <- c(parent_vec, rep(q, length(new_edge)))
    }
    idx <- ne_vec %in% .to
    if (any(idx)) {# if we find one target
      path <- sort_co(ne_vec[idx])[1]
      while (path[1] != .from) path <- c(parent_vec[which(queue == path[1])], path)
      return(path[2]) #path[2] is the new square to move to target (= tail(path, 1))
    } else {
      if (length(ne_vec) == 0) break
      step <- c(step, rep(max(step) + 1L, length(ne_vec)))
    }
  }
  
  return(.from) #if none of .to was reached we stay at .from
}


cd0 <- data.frame(
  id = seq_along(tile[tile %in% c("E", "G")]),
  pos = as.complex(names(tile[tile %in% c("E", "G")])),
  eg = tile[tile %in% c("E", "G")],
  hp = 200L
)
cd0 <- cd0[order(Im(cd0$pos), Re(cd0$pos)), ]

#simulate battle---------
simulate_battle <- function(ea = 3L, ga = 3L, cd = cd0, part1 = TRUE) {
  cd$ap <- ifelse(cd$eg == "G", ga, ea)
  rounds <- 0
  while (TRUE) {
    
    for (j in seq_along(cd[, 1])) {
      
      if (cd$hp[j] > 0L) { #if the current party has positive hp 
        eg_j <- cd$eg[j]
        all_e <- cd[cd$eg != eg_j & cd$hp > 0L, ] #all enemy positions
        
        if (min(abs(cd$pos[j] - all_e$pos)) > 1) { #if current player is not adjacent to an enemy
          
          blk_pos <- cd[cd$hp > 0L, "pos"] # positions that cannot be reached because someone else is standing there
          maze <- setdiff(co, blk_pos)
          all_att_squares <- unlist(sapply(all_e$pos, function(z) maze[abs(maze - z) == 1]))#attacking squares
          cd$pos[j] <- find_next_square(cd$pos[j], unique(all_att_squares), maze)
        } 
        
        att_en <- all_e[abs(all_e$pos - cd$pos[j]) == 1, ]
        if (nrow(att_en) > 0) {
          att_id <- att_en[order(att_en$hp, Im(att_en$pos), Re(att_en$pos)), ][1, c("id", "hp")]
          cd$hp <- cd$hp - ifelse(cd$id == att_id$id, cd$ap[j], 0L)
          if (!part1 & eg_j == "G" & att_id$hp <= ga) return(-1)
          if (all(cd$hp[cd$eg != eg_j] <= 0)) return(rounds * sum(pmax(cd$hp, 0)))
        }
      }
    }
    
    rounds <- rounds + 1L
    cd <- cd[cd$hp > 0L, ]
    cd <- cd[order(Im(cd$pos), Re(cd$pos)), ]
  }
}


#part1----
simulate_battle() #222831

#part2-----
res <- -1L
ea0 <- 12L #since one elve is attacked by four goblins this is the minimum ea it needs to survive

while (res == -1L) { 
  res <- simulate_battle(ea = ea0, part1 = FALSE) #54096
  ea0 <- ea0 + 1L
}
res
