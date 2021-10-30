#############################################

roman.to.arabic <- function(vector, wrong_characters = NA){
  
  vector = trimws(vector)
  vec = rep(0, length(vector))
  matching = data.frame(character = c("I", "V", "X", "L", "C", "D", "M"),
                        values = c(1, 5, 10, 50, 100, 500, 1000))
  
  for(j in 1:length(vector)){
    if(is.na(vector[j])){
      vec[j] = wrong_characters
    } else {
      for(i in 1:nchar(vector[j])){
        
        char1 = str_sub(vector[j],i,i)
        char2 = str_sub(vector[j],i+1,i+1)
        
        number1 = subset(matching, character == char1)[[2]]
        number2 = subset(matching, character == char2)[[2]]
        
        if(length(number1) == 0 ){
          vec[j] = wrong_characters
        } else if(length(number2) == 0){
          vec[j] = vec[j] + number1
        } else if(number1 >= number2) {
          vec[j] = vec[j] + number1
        } else {
          vec[j] = vec[j] - number1
        }
      }
    }
  }
  vec
}

roman.to.arabic(c(NA,"MMMCMXCIX", "MMDXVI", "I"))

######################################################
arabic.to.roman <- function(vector){
  vec = rep("", length(vector))
  matching = tibble(number = as.character(1:9),
                    level1 = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"),
                    level2 = c("X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"),
                    level3 = c("C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"),
                    level4 = c("M", "MM", "MMM", rep(NA, 6)))
  
  for(j in 1:length(vector)){
    if(vector[j] > 0 & vector[j] < 4000 & vector[j]%%1 == 0){
      for(i in 1:nchar(as.character(vector[j]))){
        num = str_sub(vector[j],-i,-i)
        b = subset(matching, number == num)[[i+1]]
        vec[j] = paste(b, vec[j], sep = "")
      }
    } else{
      vec[j] = NA
    }
  }
  vec
}

######################################################

tibble(x = c(-6:100, 3980:4010, seq(1, 8, 0.25))) %>% 
  mutate(y = roman.numerals.2(x)) %>% 
  mutate(z = roman.numerals(y)) %>%
  # mutate(true = all.equal(x, z)) %>%
  view()

######################################################

envelope <- function(n, outer = " ", inner = "#", matrix = FALSE, sep = " "){
  MATRIX = matrix(outer, ncol = n, nrow = n)
  
  for(i in 1:n){
    for(j in 1:n){
      if(i == j | i == n - j + 1 | j == 1 | i == 1 | j == n | i == n){
        MATRIX[i,j] = inner
      }
    }
    if(!matrix) cat(MATRIX[i,],"\n", sep = sep)
  }
  if(matrix) MATRIX
}

envelope(30, sep = " ")

######################################################

compase_rose <- c("N", "NNE", "NE", "ENE",
                  "E", "ESE", "SE", "SSE",
                  "S", "SSW", "SW", "WSW",
                  "W", "WNW", "NW", "NNW")


