data15 <- do.call(rbind, strsplit(readLines("input/day15.txt"), ""))

co <- apply(which(data15 != "#", arr.ind = TRUE), 1, function(z) z[2] + z[1]*1i)
tile <- setNames(data15[data15 != "#"], co)
co <- co[order(Im(co), Re(co))] #bring co into reading order

#maze functions---------
bfs_old <- function(.from, .to, maze) { #breadth fist search
  queue <- .from
  parent_vec <- NA_complex_
  j <- 1L
  
  while (j <= length(queue)) {
    parent <- queue[j]
    new_edge <- setdiff(maze[abs(parent - maze) == 1], queue)
    queue <- c(queue, new_edge)
    parent_vec <- c(parent_vec, rep(parent, length(new_edge)))
    if (all(.to %in% queue)) break
    j <- j + 1L
  }
  return(data.frame(z = queue, parent = parent_vec))
}

bfs <- function(.from, .to, maze) { #breadth fist search
  queue <- .from
  parent_vec <- NA_complex_
  j <- 1L
  
  while (j <= length(queue)) {
    parent <- queue[j]
    new_edge <- setdiff(maze[abs(parent - maze) == 1], queue)
    queue <- c(queue, new_edge)
    parent_vec <- c(parent_vec, rep(parent, length(new_edge)))
    if (any(.to %in% queue)) break
    j <- j + 1L
  }
  return(data.frame(z = queue, parent = parent_vec))
}


path_length <- function(.from, .to, queue) { 
  path <- .to
  while (.from != path[1]) { #does not work if there is no path
    path <- c(queue$parent[which(queue$z == path[1])], path)
  }
  
  length(path) - 2L #number of steps needed from a to b
  
}

#create combat dataframe cd-----------
cd0 <- data.frame(
  id = seq_along(tile[tile %in% c("E", "G")]),
  pos = as.complex(names(tile[tile %in% c("E", "G")])),
  eg = tile[tile %in% c("E", "G")],
  hp = 200L
)
cd0 <- cd0[order(Im(cd0$pos), Re(cd0$pos)), ]

#simulate battle---------
simulate_battle <- function(ea = 3L, ga = 3L, cd = cd0, part1 = TRUE) {
  rounds <- 0
  while (length(unique(cd$eg)) > 1) {
    
    for (j in seq_along(cd[, 1])) {
    # for (j in 1:3) {
      # print(j)
      cur_pos <- cd$pos[j]
      all_e <- cd[cd$eg != cd$eg[j] & cd$hp > 0, ] #all enemy positions
      
      if (cd$hp[j] > 0 & nrow(all_e) > 0) {
        act_pos <- setdiff(subset(cd, hp > 0)$pos, cur_pos) #positions which are blocked by other players
        
        if (min(abs(cur_pos - all_e$pos)) > 1) { #if current player is not adjacent to an enemy
          all_att_squares <- setdiff(unlist(sapply(all_e$pos, function(z) co[abs(co - z) == 1])), act_pos)#attacking squares
          
          qu <- bfs(cur_pos, setdiff(all_att_squares, act_pos), setdiff(co, act_pos))
          
          all_att_squares <- all_att_squares[all_att_squares %in% qu$z] #all reachable attacking squares
          if (length(all_att_squares) > 0) {
            path_l <- sapply(all_att_squares, function(x) path_length(cur_pos, x, qu))
            # all_att_squares2 <- all_att_squares[path_l == min(path_l)]
            # t_square <- all_att_squares2[order(Im(all_att_squares2), Re(all_att_squares2))][1] #target square
            # t_square <- all_att_squares[1]
            
            if (min(path_l) > 0) { #if player cannot attack (yet)
              cur_p1 <- setdiff(co[abs(co - cur_pos) == 1], cd$pos) #squares that can be reached in one step
              
              qu_p1 <- bfs(t_square, cur_p1, setdiff(co, act_pos))
              # path_l_p1 <- sapply(cur_p1, function(x) path_length(t_square, x, qu_p1))
              new_pos <- cur_p1[cur_p1 %in% qu_p1$z][1]
              
              # new_pos <- cur_p1[path_l_p1 == min(path_l_p1)]
              # new_pos <- new_pos[order(Im(new_pos), Re(new_pos))][1]
              cd$pos[j] <- new_pos
              cur_pos <- new_pos
              attack <- FALSE
            } else { #if there is only on step to make before attack
              cd$pos[j] <- t_square
              cur_pos <- t_square
              attack <- TRUE
            }
          } else {
            attack <- FALSE
          }
        } else {
          attack <- TRUE
        }
        
        if (attack) { #if player can attack
          att_en <- subset(cd, eg != cd$eg[j] & abs(pos - cur_pos) == 1 & hp > 0)
          att_en <- subset(att_en, hp == min(att_en$hp))
          att_en <- att_en[order(Im(att_en$pos), Re(att_en$pos)), ][1, ]
          
          a <- if (cd$eg[j] == "E") ea else ga #attacking power
          cd$hp <- cd$hp - ifelse(cd$id == att_en$id, a, 0)
            
        }
      }
      if (length(unique(cd[cd$hp > 0, ]$eg)) == 1) break
    }
    
    if (j == nrow(cd)) rounds <- rounds + 1L
    cd <- cd[cd$hp > 0, ]
    cd <- cd[order(Im(cd$pos), Re(cd$pos)), ]
  }
  
  score <- rounds * sum(cd$hp)
  if (part1) return(score)

  return(if (cd$eg[1] == "E" & nrow(cd) == sum(cd0$eg == "E")) score else 0)
}

#part1----
simulate_battle() #222831
system.time(simulate_battle()) #50 seconds

#part2-----
simulate_battle(ea = 15L, part1 = FALSE) #found by manual search
