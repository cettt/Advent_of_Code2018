data13 <- do.call(rbind, strsplit(readLines("input/day13.txt"), ""))

co <- data13[data13!= " "]
names(co) <- apply(which(data13 != " ", arr.ind = TRUE), 1, function(x) x[2] - 1 + (x[1] - 1)*1i)

carts0 <- data.frame(
  pos = as.complex(names(co[co %in% c("<", ">", "^", "v")])),
  dir = c("<" = -1, ">" = 1, "^"= -1i, "v" = 1i)[co[co %in% c("<", ">", "^", "v")]],
  turn_counter = 0
)

co[co %in% c("<", ">")] <- "-"
co[co %in% c("v", "^")] <- "|"

simulate_carts <- function(carts = carts0) {
  res <- character()
  
  while(nrow(carts) > 1) {
    carts$new_pos <- carts$pos + carts$dir
    carts$new_pos_type <- co[as.character(carts$new_pos)]
    
    carts$dir <- carts$dir * ifelse( #update cart direction
      carts$new_pos_type %in% c("-", "|"), 1, #new direction = old direction
      ifelse(
        carts$new_pos_type == "/", ifelse(Im(carts$dir) == 0, -1i, 1i),
        ifelse(
          carts$new_pos_type == "\\", ifelse(Im(carts$dir) == 0, 1i, -1i),
          c(-1i, 1, 1i)[carts$turn_counter %% 3 + 1] #intersections
        )
      )
    )
    
    carts$turn_counter = carts$turn_counter + (carts$new_pos_type == "+")
    
    if (anyDuplicated(c(carts$pos, carts$new_pos))) { #precheck for crashes (necessary but not sufficient)
      
      carts <- carts[order(Im(carts$pos), Re(carts$pos)), ] #order carts to check for real crashes
      crash <- sapply( #precise check for crashes but slower than the fast check
        seq_along(carts[, 1]),
        function(k) with(carts, new_pos[k] %in% c(new_pos[seq_len(k-1)], pos[-seq_len(k)]))
      )
      
      if (any(crash)) {
        crash_pos <- unique(carts$new_pos[which(crash)])
        res <- c(res, paste(Re(crash_pos), Im(crash_pos), sep = ","))
        carts <- carts[!((carts$pos %in% crash_pos) | (carts$new_pos %in% crash_pos)), ]
      }
    }
    carts$pos <- carts$new_pos
  }
  return(c(paste(Re(carts$pos), Im(carts$pos), sep = ","), res))
}

carts <- simulate_carts()
#part1--------
carts[2]

#part2--------
carts[1]

