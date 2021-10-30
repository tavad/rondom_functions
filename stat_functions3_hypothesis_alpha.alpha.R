sample = c(11696, 13251, 12482, 11102, 8062, 10275, 6065, 5321, 10555, 10013, 8049, 9321, 7024, 6448, 10715,
           10854, 9368, 6017, 14177, 10326, 12576, 6251, 6995, 10682, 10477, 10943, 8483, 13148, 8954, 9210,
           7960, 9849, 9001, 12477, 8494, 3741, 8846, 8824, 11815, 13603, 10410, 8005, 7886, 9597, 9856,
           12472, 6851, 9488, 7359, 11092, 9383, 11402, 11531, 12802, 12078, 6545, 13161, 8659, 13203, 13937,
           9603, 7932, 12777, 7650, 10793, 15008, 6439, 11576, 14726, 10480, 7696, 11156, 8900, 10322, 9966)



## A report stated that the national mean price for a used cars is $9500. You want to dertermine
## if the Seattle area mean price of a used cars is differenet from the national mean.
## At an alpha of 0.01, is the mean price for a Seattle used cars different from the national average?


mu = 9500

# 1. make hypotesis
# H0 = 9500
# H1 != 9500

# 2. alpha
alpha = 0.01

# 3. calculations

mean.sample = mean(sample)
sd.sample = sd(sample)
n.sample = length(sample)
df.sample = n.sample - 1

SE.sample = sd.sample/sqrt(n.sample)

t = (mean.sample- mu)/SE.sample

# 4. compeare
# 4.1.
if (abs(t) <= qt(1-alpha/2,df.sample)) {
  print(paste0("At a ",alpha," significance level, our sample mean of $",
               formatC(mean.sample)," does not provide statistical evidences to show that the mean ",
               "Seattle used car price is different from the national mean."))
} else {
  print("H0 is rejected")
}

# 4.2.
if (1 - pt(t, df.sample) >= alpha/2) {
  print("H0 is accepted")
} else {
  print("H0 is rejected")
}

# 5 conclusion


########################### FUNCTION #################


hypothesis <- function(sample, type = c("RT", "LT", "2T"), alpha = 0.05, mu, sigma = NULL) {
  
  mean = mean(sample)
  sd = sd(sample)
  n = length(sample)
  df = n - 1
  
  print(mean)
  
  if (type == "2T") {
    a1 = alpha/2
    a2 = 1 - alpha/2
  } else if (type == "RT") {
    a1 = 0
    a2 = 1 - alpha
  } else {
    a1 = alpha
    a2 = 1
  }
  
  if (!is.null(sigma)) {
    SE = sigma/sqrt(n)
    Z1 = qnorm(a1)
    Z2 = qnorm(a2)
  } else {
    SE = sd/sqrt(n)
    Z1 = qt(a1, df)
    Z2 = qt(a2, df)
  }
  
  z = (mean - mu)/SE
  
  print(z)
  print(a1)
  print(a2)
  print(Z1)
  print(Z2)
  
  
  if (z <= Z2 & z >= Z1) {
    print("H0 is accepted")
  } else {
    print("H0 is rejected")
  }
  
  data <- data.frame(zet = seq(-4, 4, length.out = 500)) %>% 
    mutate(area_out = ifelse(zet <= Z1 | zet >= Z2, dnorm(zet), 0),      #if sigma is not known than dt
           area_in = ifelse(zet >= Z1 & zet <= Z2, dnorm(zet), 0),       #if sigma is not known than dt
           ) %>% 
    pivot_longer(c(area_out, area_in), names_to = "area", values_to = "fz")
  
  bounds = seq(-4, 4, length.out = 9)
  
  line_df <- data.frame(
    x = -3:3,
    y = 0,
    xend = -3:3,
    yend = ifelse(is.null(sigma),dt(-3:3, df.sample),dnorm(-3:3))  #if sigma is not known than dt
  )
  
  ggplot(data, aes(zet, fz, fill = area)) +
    scale_fill_manual(values = c(7,4)) +
    geom_area(show.legend = F) +
    geom_segment(
      data = line_df,
      mapping = aes(x=x, y=y, xend=xend, yend=yend),
      inherit.aes = FALSE, lty = 3) +
    geom_segment(aes(x = z, y=0, xend = z, yend = ifelse(is.null(sigma),dt(z, df.sample),dnorm(z))),
                 inherit.aes = FALSE, size = 1.2) + #if sigma is not known than dt

    scale_x_continuous(breaks = c(4.3, bounds, 4.3),
                       labels = paste0(c("X", formatC(bounds), "X"),
                                       "\n", "\n", c("Z",-4:4, "Z"))) +
    scale_y_continuous(position = "right") +
    labs(y = "Likelhood \n ",
         x = "",
         title = "Normal (Gaussian) Distribution",
         subtitle = "subtitle1")+
    theme(panel.background=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.ticks=element_blank())


  # data.frame(x = seq(-5,5,0.02)) %>%
  #   mutate(y = dnorm(x)) %>%
  #   ggplot(aes(x,y)) +
  #   geom_area(fill = "darkgrey")


  # if (abs(t) <= porog) {
  #   print(paste0("At a ",alpha," significance level, our sample mean of $",
  #                formatC(mean.sample)," does not provide statistical evidences to show that the mean ",
  #                "Seattle used car price is different from the national mean."))
  # } else {
  #   print("H0 is rejected")
  # }
  #

  
  
}

hypothesis(sample, type = "2T", alpha = 0.01, mu = 9900, sigma = 200)







