data24 <- readLines("input/day24.txt")

is_in <- ifelse(cumsum(data24 == "Infection:") == 0, "is", "in")
x <- data24[grepl("^\\d", data24)]

im_vec <- rep("0", length(x))
we_vec <- rep("1", length(x))
im_vec[grepl("immune", x)] <- gsub(".*immune to ([^;\\)]+).*", "\\1", x[grepl("immune", x)])
we_vec[grepl("weak", x)] <- gsub(".*weak to ([^;\\)]+).*", "\\1", x[grepl("weak", x)])



stat_df <- data.frame(
  id = seq_along(x),
  is_in = is_in[grepl("^\\d", data24)],
  n_units = as.integer(gsub("(\\d+) units.*", "\\1", x)),
  hp = as.integer(gsub(".* (\\d+) hit.*", "\\1", x)),
  init = as.integer(gsub(".* initiative (\\d+)$", "\\1", x)),
  dp =  as.integer(gsub(".* does (\\d+) .*", "\\1", x)),
  im = im_vec,
  we = we_vec,
  dam = gsub(".* ([a-z]+) damage .*", "\\1", x)
)

# fight--------
fight_infection <- function(stats_df, boost = 0L, part1 = FALSE) {
  
  stat_df$dp <- stat_df$dp + ifelse(stat_df$is_in == "is", boost, 0L)
  while (TRUE) {
    stat_df$ep <- stat_df$n_units * stat_df$dp
    stat_df <- stat_df[order(-stat_df$ep, -stat_df$init), ]
    
    tar_vec <- integer()
    tar_dam <- integer()
    for (k in seq_along(stat_df[,1])) { #target selection
      poss_tar <- subset(stat_df, is_in != stat_df$is_in[k] & !id %in% tar_vec)
      if (nrow(poss_tar) == 0) 
      {
        tar_vec <- c(tar_vec, 0L)
        tar_dam <- c(tar_dam, 0L)
      } else {
        dam_vec <- stat_df$ep[k] * ifelse(grepl(stat_df$dam[k], poss_tar$we), 2, 1)
        dam_vec <- dam_vec *ifelse(grepl(stat_df$dam[k], poss_tar$im), 0, 1)
        if (max(dam_vec) == 0) {
          tar_vec <- c(tar_vec, 0L)
        } else {
          tar_vec <- c(tar_vec, poss_tar$id[order(-dam_vec, -poss_tar$ep, -poss_tar$init)][1])
        }
        tar_dam <- c(tar_dam, max(dam_vec))
      }
    }
    att_df <- stat_df
    att_df$tar <- tar_vec
    att_df$dam_num <- tar_dam
    att_df <- att_df[order(-att_df$init),]
    
    total_kill <- 0L
    for (j in seq_along(att_df[,1])) {
      if (att_df$n_units[j] > 0 & att_df$tar[j] != 0) {
        dam <- att_df$dam_num[j]
        target_row <- which(att_df$tar[j] == att_df$id)
        killed_units <- pmin(floor(dam / att_df[target_row, ]$hp), att_df$n_units[target_row])
        total_kill <- total_kill + killed_units
        att_df[target_row, ]$dam_num <- att_df[target_row, ]$dam_num / att_df[target_row, ]$n_units * (att_df[target_row, ]$n_units  - killed_units)
        att_df[target_row, ]$n_units <- att_df[target_row, ]$n_units - killed_units
        # cat(paste(
        #   "ID", att_df$id[j], "from",
        #   ifelse(att_df$is_in[j] == "is", "Immune System", "Infection"),
        #   "attacks id", att_df$tar[j],
        #   "dealing total damage of", dam, "and killing", killed_units, "units\n"))
      }
    }
    
    # cat("\n\n")
    
    stat_df <- subset(att_df, n_units > 0)
    if (length(unique(stat_df$is_in)) == 1) break
    if (total_kill == 0) break
  }
  
  part2_score <- if (any(stat_df$is_in == "in")) -1 else sum(stat_df$n_units) 
  return(if (part1) sum(stat_df$n_units) else part2_score)
}


#part1----
fight_infection(stat_df, part1 = TRUE)


#part2-------
fight_infection(stat_df, boost = 61L)
