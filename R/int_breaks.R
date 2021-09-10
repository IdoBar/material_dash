int_ceil <- function(x) {
  ceil <- ceiling(max(x))+1
  #ifelse(ceil > 1 & ceil %% 2 == 1, ceil + 1, ceil)
}

int_breaks <- function(x) { 
  ceil <- int_ceil(max(x))
  unique(ceiling(pretty(seq(0, ceil))))
} 

int_limits <- function(x) { 
  ceil <- int_ceil(x[2])
  c(x[1], ceil)
}

int_breaks2 <- function(x) unique(floor(pretty(seq(0, (max(x) + 2) * 1.1))))
