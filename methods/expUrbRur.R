expUrbRur <- function(data, exp) {

  urb <- data[data$urban == 1, ]
  rur <- data[data$urban == 0, ]
  
  urbV <- grepl(exp, urb$BALDESIG)
  rurV <- grepl(exp, rur$BALDESIG)
  
  urbP <- sum(urbV)/nrow(urb)
  rurP <- sum(rurV)/nrow(rur)
  
  df <- data.frame(c(urbP, rurP))
  rownames(df) <- c('urb', 'rur')
  colnames(df) <- exp
  
  return(df)
}