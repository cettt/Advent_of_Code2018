data16 <- readLines("input/day16.txt")

x1 <- data16[cumsum(data16 == "") < sum(data16 == "")]
x1 <- x1[x1 != ""]

p1_d <- data.frame(
  be = sub("Before: \\[(.*)\\]", "\\1", x1[grepl("^B", x1)]),
  af = sub("After:  \\[(.*)\\]", "\\1", x1[grepl("^A", x1)]),
  inst = x1[!grepl("^[AB]", x1)]
)

run_prog <- function(inp, inst, opcode) {
  if (opcode == 1L) { #addr
    res <- inp[inst[2] + 1L] + inp[inst[3] + 1L]
  } else if (opcode == 2L) { #addi
    res <- inp[inst[2] + 1L] + inst[3]
  } else if (opcode == 3L) { #multr
    res <- inp[inst[2] + 1L] * inp[inst[3] + 1L]
  } else if (opcode == 4L) { #multi
    res <- inp[inst[2] + 1L] * inst[3]
  } else if (opcode == 5L) { # banr
    res <- bitwAnd(inp[inst[2] + 1L], inp[inst[3] + 1L])
  } else if (opcode == 6L) { #bani
    res <- bitwAnd(inp[inst[2] + 1L], inst[3])
  } else if (opcode == 7L) { # borr
    res <- bitwOr(inp[inst[2] + 1L], inp[inst[3] + 1L])
  } else if (opcode == 8L) { #bori
    res <- bitwOr(inp[inst[2] + 1L], inst[3])
  } else if (opcode == 9L) { #setr
    res <- inp[inst[2] + 1L]
  } else if (opcode == 10L) { #seti
    res <- inst[2]
  } else if (opcode == 11L) { #gtir
    res <- (inst[2] > inp[inst[3] + 1L])
  } else if (opcode == 12L) { #gtir
    res <- (inp[inst[2] + 1L] > inst[3])
  } else if (opcode == 13L) { #gtrr
    res <- (inp[inst[2] + 1L] > inp[inst[3] + 1L])
  } else if (opcode == 14L) { #gtir
    res <- inst[2] == inp[inst[3] + 1L]
  } else if (opcode == 15L) { #gtir
    res <- inp[inst[2] + 1L] == inst[3]
  } else if (opcode == 16L) { #gtrr
    res <- inp[inst[2] + 1L] == inp[inst[3] + 1L]
  }
  
  inp[inst[4L] + 1] <- res
  return(inp)
}

try_opcodes <- function(be, af, pr) {
  be <- as.integer(strsplit(be, ", ")[[1]])
  af <- as.integer(strsplit(af, ", ")[[1]])
  pr <- as.integer(strsplit(pr, " ")[[1]])
  which(sapply(1:16, function(opc)  all(run_prog(be, pr, opc) == af)))
  
}

#part1--------
opc_poss <- mapply(try_opcodes, p1_d$be, p1_d$af, p1_d$inst)
sum(sapply(opc_poss, function(x) length(x) >=3))

#part2----------

opc <- as.integer(sub("^(\\d+).*", "\\1", p1_d$inst))
possible <- sapply(0:15, function(k) Reduce(intersect, opc_poss[opc == k]))

opc_map <- integer(16L)
for (i in seq_len(16)) {
  x <- which(sapply(possible, function(x) length(setdiff(x, opc_map)) == 1))[1]
  opc_map[x] <- setdiff(possible[[x]], opc_map)
}

p2 <- data16[cumsum(data16 == "") == sum(data16 == "")][-1]
res2 <- integer(4)

for (i in seq_along(p2)) {
  inst <- as.integer(strsplit(p2[i], " ")[[1]])
  res2 <- run_prog(res2, inst, opc_map[inst[1] + 1L])
}
res2[1]
