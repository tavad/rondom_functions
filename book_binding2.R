x = c("one", "two", "tree", "four", "five", "six", "one", "tree", "four")

xf = as.factor(x)


xf2 = factor(x, levels = c("one", "two", "tree", "four", "five", "six"))

x = gsub("two", "other", x)
x = gsub("one", "other", x)

x



book.binding <- function(n, blank = "blank", bindings = 1) {
  
  leftower.sheets = ceiling(n/4) - floor(ceiling(n/4)/bindings)*bindings
  
  vector = rep(c(1,0), times = c(leftower.sheets, bindings - leftower.sheets))
  print(vector)
  
  for (k in 1:bindings) {

    n1 = floor(ceiling(n/4)/bindings)*4*(k-1)+1
    n2 = floor(ceiling(n/4)/bindings)*4*k

    x = c(n1:n2, rep(blank, ifelse(n2%%4==0,0,4-n2%%4)))
    # nn = ceiling(n2/4)*4
    nn = n2/k

    odd = x[c(TRUE, FALSE)]
    even = x[c(FALSE, TRUE)]
    even = even[(nn/2):1]

    y = as.vector(t(data.frame(even, odd)))

    y1 = y[1:(nn/2)]
    y2 = y[(nn/2+1):nn]

    cat(paste0("Binding ", k, ", Number of sheets = ", (n2-n1+1)/4, collapse = ""))
    cat("\n")
    cat(paste0(y1, collapse=", "))
    cat("\n")
    cat(paste0(y2, collapse=", "))
    cat("\n")
  }
}


book.binding(28*3, bindings = 3)



book.b <- function(n,  bindings = 1) {
  
  leftower.sheets = ceiling(n/4) - floor(ceiling(n/4)/bindings)*bindings
  
  vector = rep(c(1,0), times = c(leftower.sheets, bindings - leftower.sheets))
  print(vector)

  
}


book.b(100,3)
