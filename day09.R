data09 <- as.integer(gsub("\\D+", "", read.table("input/day09.txt", sep = ";")[1,]))

play_marble <- function(rounds, n_players) {
  mp1 <- 0 #mp1 is short for marble plus 1: mp[t] is the marble next to the marble with number t-1.
  cm <- 0L #current marble
  score <- integer(n_players)
  
  for (k in seq_len(rounds)) {
    if (k %% 23L != 0L) {
      hlp <- mp1[if (mp1[cm + 1L] + 1L > k) 1L else mp1[cm + 1L] + 1L]
      mp1[mp1[cm + 1] + 1L] <- k
      mp1[k + 1L] <- hlp
      cm <- k
      if (k %% 23L == 18L) point_marb <- mp1[k + 1L] # five rounds before a bonus round the bonus marble is next to the current marbl
    } else {
      mp1[k - 5L + 1L] <- mp1[point_marb + 1L] #the next marble after the marble before the bonus marble is set
      mp1[point_marb + 1L] <- NA_integer_ #the bonus marble has no successor
      cm <-  mp1[k - 5L + 1L] #the new current marble is the marble is the marble next to the marble from five rounds before
      idx <- (k - 1L) %% n_players + 1L
      score[idx] <- score[idx] + k + point_marb
    }
  }
  return(max(score))
}

#part1----------
play_marble(data09[2], data09[1])

#part2----------
play_marble(data09[2] * 100L, data09[1])
