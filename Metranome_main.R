metronom <- function(n = 6, rate = 10, time = 8*pi, mode.random = T, phase.in = T){
  
  start = if(length(n)!= 1){
    n
  } else if(mode.random) {
    runif(n, 0, 2*pi)
  } else {
    seq(0, 2*pi, length.out = n+1)[-(n+1)]
  }
  
  given = ifelse(length(n) != 1, T, F)
  n = ifelse(length(n) == 1, n, length(n))
  k = 0.01/rate
  x = seq(1, time, 0.01)
  s = ifelse(phase.in, 1, -1)
  
  time = matrix(rep(start, each = length(x)), ncol = length(start), nrow = length(x))
  
  len = 0
  for (i in 2:length(x)) {
    for (l in 1:n) {
    sin_sum = 0
      for (j in 1:n) {
        current = sin(s*(time[i-1,j]-time[i-1,l]))
        sin_sum = sin_sum + current
      }
      time[i,l] = time[i-1,l] + k * sin_sum
    }
  }
  
  sindf = as.data.frame(time) %>% 
    gather(V, value = "value") %>%
    mutate(xx = rep(x, times = n),
           sinx = sin(xx + value),
           metranome = factor(rep(paste0("metranome ", 1:n), each = length(x)),
                              levels = rep(paste0("metranome ", 1:n))))
  
  ggplot(sindf, aes(xx, sinx, col = metranome)) +
    geom_line() +
    labs(y = "Oscillation",
         x = "Time",
         title = paste0("Simulation of ", n, " Metronomes",
                        ifelse(phase.in, " "," Phase out "), "Synchronization"),
         subtitle = paste0("Using ", ifelse(given, "given",
                                            ifelse(mode.random, "random", "equidistant")),
                           " starting points"),
         col = "")
}

metronom(c(0,0,0,pi,pi,pi*0.001))
metronom(n = 20, rate = 20, time = 30)
metronom(12, 50)


metronom(10)

set_time
