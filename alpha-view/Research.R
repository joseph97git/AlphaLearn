# source functions
source("Basics.R")

getIndicators <- function(symb, shift, splitsize, seed, traintest = TRUE) {
  # Gets indicator data, splits the data
  # into training and testing if desired.
  
  # ADX data
  adx <- ADX(symb, n = shift)
  adx_raw <- adx[complete.cases(adx),] 
  
  # ADX pos, neg, direction
  pnDX <- adx[,1:3]
  pnDX_raw <- pnDX[complete.cases(pnDX),]
  
  # ROC data
  roc <- ROC(symb, n = shift, na.pad = FALSE)
  
  # momentum data
  moment <- momentum(symb, n = shift, na.pad = FALSE)
  
  # combine ADX subset, roc, momentum
  pnDX_roc_moment <<- merge(pnDX_raw, roc[,4], moment[,4])
  
  # get equilibrium data
  equilPoints(symb, shift)
  
  # convert 2nd derivative to factor
  equil_dates_2deriv <- factor(equil_dates_2deriv)
  
  # combine and convert to xts
  dates_signal <- data.frame("signal" = equil_dates_2deriv)
  rownames(dates_signal) <- equil_dates
  dates_signal <- xts(dates_signal, order.by = as.Date(rownames(dates_signal), "%Y-%m-%d"))
  
  # subset indicators by equilibrium dates
  predictors_sub <- pnDX_roc_moment[equil_dates,]
  
  # subset signal 
  dates_signal_sub <- dates_signal[index(predictors_sub)]
  
  # combine predictors and response
  data_dx_roc <- merge(predictors_sub, dates_signal_sub)

  if (traintest) {
    # split into training and test sets
    smp_size <- floor(splitsize*nrow(data_dx_roc))
    set.seed(seed)
    train_idx <- sample(seq_len(nrow(data_dx_roc)), size = smp_size)
    trainDat <- data_dx_roc[train_idx,]
    test <- data_dx_roc[-train_idx,]
    names <- c("DIp", "DIn", "DX", "ROC", "Momentum", "signal")
    colnames(trainDat) <- names
    colnames(test) <- names
    
    output <- list(trainDat, test)
    
    return(output)
  }
  else {
    return(data_dx_roc)
  }
}