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
  
  while (j <= length(queue)) { #start bfs
    parent <- queue[j]
    new_edge <- setdiff(maze[abs(parent - maze) == 1], queue)
    queue <- c(queue, new_edge)
    parent_vec <- c(parent_vec, rep(parent, length(new_edge)))
    step <- c(step, rep(step[j] + 1L, length(new_edge)))
    if (any(new_edge %in% .to)) {# if we find one target
      # print(j)
      idx <- max(which(step == step[j]))
      while (j < idx) { #complete the bfs to include all edges with the same distances as the found target
        j <- j + 1
        parent <- queue[j]
        new_edge <- setdiff(maze[abs(parent - maze) == 1], queue)
        queue <- c(queue, new_edge)
        parent_vec <- c(parent_vec, rep(parent, length(new_edge)))
        step <- c(step, rep(step[j] + 1L, length(new_edge)))
      }
      target <- queue[step == step[j] + 1L]
      path <- sort_co(target[target %in% .to])[1] #this is the final attack destination
      while(path[1] != .from) path <- c(parent_vec[which(queue == path[1])], path)
      return(path[2]) #path[2] is the new square to move to target (= tail(path, 1))
    }
    j <- j + 1L
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
  while (length(unique(cd$eg)) > 1) {
    
    for (j in seq_along(cd[, 1])) {
      
      if (cd$hp[j] > 0L) { #if the current party has positive hp and there are enemies left
        eg_j <- cd$eg[j]
        all_e <- subset(cd, eg != eg_j & hp > 0L) #all enemy positions
        
        if (min(abs(cd$pos[j] - all_e$pos)) > 1) { #if current player is not adjacent to an enemy
          
          blk_pos <- subset(cd[-j, ], hp > 0)$pos # positions that cannot be reached because someone else is standing there
          maze <- setdiff(co, blk_pos)
          all_att_squares <- unlist(sapply(all_e$pos, function(z) maze[abs(maze - z) == 1]))#attacking squares
          cd$pos[j] <- find_next_square(cd$pos[j], unique(all_att_squares), maze)
        } 
        
        att_en <- subset(all_e, abs(pos - cd$pos[j]) == 1)
        if (nrow(att_en) > 0) {
          att_id <- att_en[order(att_en$hp, Im(att_en$pos), Re(att_en$pos)), ][1, c("id", "hp")]
          cd$hp <- cd$hp - ifelse(cd$id == att_id$id, cd$ap[j], 0L)
          if (!part1 & eg_j == "G" & att_id$hp <= ga) return(-1)
          if (nrow(subset(cd, hp > 0 & eg != eg_j)) == 0) break
        }
      }
    }
    
    if (j == nrow(cd)) rounds <- rounds + 1L
    cd <- subset(cd[order(Im(cd$pos), Re(cd$pos)), ], hp > 0)
  }
  
  return(rounds * sum(cd$hp))
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
