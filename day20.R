data20 <- gsub("[\\^\\$]", "", readLines("input/day20.txt"))

#function to draw the map of all rooms--------
extend_map <- function(rx, cur_pos, counter) {
  
  if (!grepl("^[NEWS\\|]+\\|\\)", rx)) {
    a <- strsplit(rx, "")[[1]]
    a[a == "|" & cumsum(ifelse(a == "(", 1, ifelse(a == ")", -1, 0))) == 0] <- " "
    rx2 <- strsplit(paste0(a, collapse = ""), " ")[[1]]
    
    if (length(rx2) == 1) {
      rx3 <- gsub("\\(.*", "", rx2)
      n <- nchar(gsub("[^NEWS]", "", rx3))
      route <- c("N" = 1i, "E" = 1, "S" = -1i, "W" = -1)[strsplit(rx3, "")[[1]]]
      count_vec <<- c(count_vec, counter + seq_len(n))
      mymap <<- c(mymap, cur_pos + cumsum(route))
      rem_route <- sub("^\\(", "", sub("\\)$", "", gsub(paste0("^", rx3), "", rx2)))
      return(if (grepl("[NEWS]", rem_route)) extend_map(rem_route, cur_pos + sum(route), counter + n) else  0)
    } else return(sum(sapply(rx2, extend_map, cur_pos = cur_pos, counter)))
    
  } else {
    b <- strsplit(sub("^([NEWS\\|]+\\|\\))", "\\1 ", rx), " ")[[1]]
    b1 <- strsplit(gsub("\\|", " ", gsub("\\|\\)", "", b[1])), " ")[[1]]
    for (x in b1) {
      route <- c("N" = 1i, "E" = 1, "S" = -1i, "W" = -1)[strsplit(x, "")[[1]]]
      count_vec <<- c(count_vec, counter + seq_along(route))
      mymap <<- c(mymap, cur_pos + cumsum(route))
    }
    return(if (grepl("[NEWS]", b[2])) extend_map(b[2], cur_pos, counter) else 0)
  }
  
}

#apply the function-------
mymap <- complex(1)
count_vec <- integer(1)

extend_map(data20, complex(1), 0L)

res <- aggregate(count_vec, by = list(pos = mymap), FUN = min)[,2]

#part1----------
max(res)

#part2-----------
sum(res >= 1000)

