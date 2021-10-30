

book.binding <- function(n, blank = "NA", bindings = 1, combine = FALSE) {
  
  leftower.sheets = ceiling(n/4) - floor(ceiling(n/4)/bindings)*bindings
  
  add.sheets = c(0,rep(c(1,0), times = c(leftower.sheets, bindings - leftower.sheets)))
  add.sheets = cumsum(add.sheets)
  
  p1 = NULL; p2 = NULL; info.full =NULL
  
  for (k in 1:bindings) {

    n1 = floor(ceiling(n/4)/bindings)*4*(k-1)+1 + 4*add.sheets[k]
    n2 = floor(ceiling(n/4)/bindings)*4*k + 4*add.sheets[k+1]
    
    leftoverPages = ifelse(n%%4!=0 & k==bindings,4-n%%4,0)

    x = c(n1:(n2 - leftoverPages), rep(blank, leftoverPages))
    nn = (ceiling((n2-n1+1)/4)*4)

    odd = x[c(TRUE, FALSE)]
    even = x[c(FALSE, TRUE)]
    even = even[(nn/2):1]

    y = as.vector(t(data.frame(even, odd)))

    y1 = y[1:(nn/2)]
    y2 = y[(nn/2+1):nn]

    info = paste0("Binding ", k, ", Number of sheets = ", (n2-n1+1)/4,
                  " Pages: ", n1, "-", n2 - leftoverPages, "\n", collapse = "")
    
    p1 = c(p1, y1)
    p2 = c(p2, y2)
    info.full = c(info.full, info)
    
    if(!combine){
      cat(info)
      cat(paste0(y1, collapse=", "))
      cat("\n")
      cat(paste0(y2, collapse=", "))
      cat("\n\n")
    }
  }
  if(combine){
  cat(info.full)
  cat("\n\n")
  cat(paste0(p1, collapse=", "))
  cat("\n\n")
  cat(paste0(p2, collapse=", "))
  }
}


book.binding(201, bindings = ceiling(200/(4*10)), combine = FALSE)

