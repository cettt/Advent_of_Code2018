data20 <- gsub("[\\^\\$]", "", readLines("input/day20.txt"))
data20 <- gsub("\\(([NEWS]+)\\|\\)", "\\1", data20) #delete  unnecessary parentheses that end with |

mymap <- 0 + 0*1i
mydoors <- complex()

extend_map <- function(rx, cur_pos) {
  a <- strsplit(rx, "")[[1]]
  a[a == "|" & cumsum(ifelse(a == "(", 1, ifelse(a == ")", -1, 0))) == 0] <- " "
  rx2 <- strsplit(paste0(a, collapse = ""), " ")[[1]]
  
  if (length(rx2) == 1) {
    rx3 <- gsub("\\(.*", "", rx2)
    route <- c("N" = 1i, "E" = 1, "S" = -1i, "W" = -1)[strsplit(rx3, "")[[1]]]
    newrooms <- cur_pos + cumsum(route)
    mymap <<- c(mymap, cur_pos + cumsum(route))
    mydoors <<- c(mydoors, (c(cur_pos, head(newrooms, -1)) + newrooms) / 2)
    rem_route <- sub("^\\(", "", sub("\\)$", "", gsub(paste0("^", rx3), "", rx2)))
    return(if (grepl("[NEWS]", rem_route)) extend_map(rem_route, cur_pos + sum(route)) else  0)
  } else return(sum(sapply(rx2, extend_map, cur_pos = cur_pos)))
  
}
extend_map(data20, mymap[1])
mydoors <- unique(mydoors)
mymap <- unique(mymap)


#mazee---------
bfs <- function(.from = complex(1)) {
  queue <- .from
  parent_vec <- NA_complex_
  j <- 1L
  step <- 0L
  
  while (j <= length(queue)) {
    parent <- queue[j]
    new_edge <- setdiff((mydoors[abs(parent - mydoors) == 1/2] - parent)*2 + parent, queue)
    queue <- c(queue, new_edge)
    parent_vec <- c(parent_vec, rep(parent, length(new_edge)))
    step <- c(step, rep(step[j] + 1L, length(new_edge)))
    j <- j + 1L
  }
  return(list(z = queue, parent = parent_vec, step = step))
}

mybfs <- bfs()

#part1-----
max(mybfs$step)

#part2
sum(mybfs$step >= 1000)
