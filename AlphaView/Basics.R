discreteDeriv <- function(data, shift) {
  # calculates derivative of closing prices
  # applies specifically to moving averages
  discrete <- rep(NA, shift)
  for (i in 1:length(data)-1) {
    discrete <- append(discrete, data[i+1]-data[i])
  }
  return(discrete)
}

equilPoints <- function(symb, shift) {
  # gets the vertical lines corresponding
  # to the equilibrium points based on 
  # the specified shift
  
  # get sma data
  close <- data.frame(symb[,4])
  sma <- SMA(close, n = shift)
  sma_raw <- sma[complete.cases(sma),]

  # calculate derivative of sma, 10 day average
  sma_deriv <- discreteDeriv(sma_raw, shift)
  sma_deriv_raw <- sma_deriv[complete.cases(sma_deriv)]

  # calculate 2nd derivate of sma
  sma_2deriv <- discreteDeriv(sma_deriv_raw, shift + 1)

  # store sma and derivative of sma
  sma_func <- data.frame(cbind(sma, sma_deriv, sma_2deriv))

  # convert derivatve to binary
  bin_deriv <- sma_func$sma_deriv
  bin_deriv[bin_deriv > 0] <- 1
  bin_deriv[bin_deriv <= 0] <- 0

  # convert 2nd derivate to binary
  bin_2deriv <- sma_func$sma_2deriv
  bin_2deriv[bin_2deriv > 0] <- 1
  bin_2deriv[bin_2deriv <= 0] <- 0

  # find equilibrium points
  equil_pts <- c(which(diff(bin_deriv)!=0))
  equil_pts_deriv_shift <- equil_pts + 1
  equil_dates <<- as.Date(row.names(close)[equil_pts])
  
  # categorize buy, sell signal at equilibrium dates
  equil_dates_2deriv <- bin_2deriv[equil_pts_deriv_shift]
  
  # create xts objects for buy and sell dates
  buy_sell_dates <- xts(x = equil_dates_2deriv, order.by = as.Date(equil_dates))
  buy_dates <- rownames(data.frame(buy_sell_dates[buy_sell_dates == 1]))
  sell_dates <- rownames(data.frame(buy_sell_dates[buy_sell_dates == 0]))

  # get vertical lines for equil pts
  x_vline <- xts(rep(TRUE, length(equil_dates)), equil_dates)
  x_vline_buy <- xts(rep(TRUE, length(as.Date(buy_dates))), as.Date(buy_dates))
  x_vline_sell <- xts(rep(TRUE, length(as.Date(sell_dates))), as.Date(sell_dates))

  return(list(x_vline_buy, x_vline_sell))
}