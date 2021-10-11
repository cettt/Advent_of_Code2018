data21 <- readLines("input/day21.txt")
inst <- lapply(strsplit(gsub("^.{4} ", "", data21[-1]), " "), as.integer)

para1 <- inst[[8]][1]
para2 <- inst[[12]][2]

#similar to day 19, running the program is very slow.
# Instead we figure out what it does and boost the code.

x5 <- 0 #instruction #6
x5_vec <- numeric()

while (TRUE) {
  x4 <- x5 + ifelse(floor(x5 / 2^16) %% 2 == 0, 2^16, 0) #instruction #7
  x5 <- para1 #instruction #8
  while (x4 >= 1) { #instruction #14
    x5 <- (((x5 + x4 %% 2^8) %% 2^24) * para2) %% 2^24 #instruction #9-#13
    x4 <- floor(x4 / 256) #instruction #27
  }
  if (x5 %in% x5_vec) break;
  x5_vec <- c(x5, x5_vec)
}

#part1------
tail(x5_vec, 1)

#part2-----
x5_vec[1]


#explanation--------
#what does the algorithm do?

#1 set inp[5] <- 123
#2 set inp[5] <-  bitwAnd(inp[5], 456) = 72
#3 set inp[5] <- inp[5]==72  = 1
#4 set inp[6] <- inp[6] + inp[5] (and skip #5) otherwise we get an infinite loop
#6 set inp[5] <- 0
#outer loop start (outer while loop)________________________________________________
  #7 set inp[4] <- bitwOr(inp[5], 65536) 
  #   65536 = 2^16. In particular bitwor(x, 2^16) = x + 2^16 if x < 2^16
  #   bitwor(x, 2^16) = x + ifelse(floor(x / 2^16) %% 2 == 0, 2^16, 0) 
  #8 set inp[5] <- 14464005 # this para1 from above
  #middle loop start: (inner while loop)_______________________________________
   #9  set inp[3] <- bitwAnd(inp[4], 255) = 0
     # since 255 = 2^8 - 1 we have that bitwAnd(255, x) = x %% 2^8
   #10 set inp[5] <- inp[5] + inp[3] = 14464005
   #11 set inp[5] <- bitwAnd(inp[5], 16777215) = 14464005
            #167777215 is equal to 2^24 - 1. 
            #therefore its binary representation is 1111111...11 (24 times)
            #In particular bitwAnd(x, 167777215) = x %% 2^24
   #12 set inp[5] <- inp[5] * 65899 = 953163465495
            # 65536 is para2 from above
   #13 set inp[5] <- bitwAnd(inp[5], 16777215) 16270103
            # Same as in #11
   #14 set inp[3] <- 256 > inp[4] = 0
   #15 set inp[6] <- inp[6] + inp[3] = inp[6] # and do not skip 16
   #16 set inp[6] <- inp[6]+ 1 and skip 17 (17 would cause a jump to ip = 28)
   #18 set inp[3] <- 0
   #inner loop_start (not necessary since only inp3 is icremented by one)_____
      #19     inp[2] <- inp[3] + 1 = 1
      #20     inp[2] <- inp[2]* 256 = 256
      #21     inp[2] <- inp[2]> inp[4] = 0
      #22     inp[6] <- inp[6] + inp[2]  and not skip #23
      #23     inp[6] <- inp[6]+ 1 (and skip 24; 24 would cause a jump to ip = 26) 
      #25     inp[3] <- inp[3] + 1 
      #26     inp[6] <- 17 and we go back to #19 until  (inp[3] + 1)* 256 > inp[4]
   #inner loop end____________________________________________________
   #the loop finishes  (inp[3] + 1)* 256 > inp[4] in which case we have
   #19, #20, #21, #22, and then
   #24  inp[6] <- 25
  #27 inp[4] <- inp[3]. Here inp[3] is always equal to ceiling(inp[4] / 256 -1 + 1e-6)
  #28 inp[6] <- 7 causes a jump
  #middle loop end___________________________________________________________
#once the middle loop ends (i.e #13#14#15#17 inp[6] <- 27)
#29 inp[3] <- inp[1] == inp[5] (the first time this happens inp[5] = 5745418 )
#30 inp[6] <- inp[6] + inp[3] #if 29 was false we go to #31 otherwise we are finished
#31 inp[6] <- 6
#outer loop end------