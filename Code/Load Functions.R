#Load Libraries

#The share of proceeds to cover administrative expenses is:
  
#(a) USD 0.10 per certified emission reduction issued for the first 15,000 tonnes of CO2 equivalent for which issuance is requested in a given year.
#(b) USD 0.20 per certified emission reduction issued for any amount in excess of 15,000 tonnes of CO2 equivalent for which issuance is requested in a given year. (no more than 350,000 USD)
#(c) No share of proceeds shall be due for project activities hosted in least developed countries. The application of this exemption shall be based on the status of the country on the date of the publication of the request for issuance of certified emissions reductions.


# Define a function to calculate registration fee
calculate_registration_fee <- function(reductions) {
  registration_fee <- numeric(length(reductions))
  for (i in seq_along(reductions)) {
    if (reductions[i] <= 15000) {
      registration_fee[i] <- reductions[i] * 0.10
    } else {
      registration_fee[i] <- min(15000 * 0.10 + (reductions[i] - 15000) * 0.20, 350000)
    }
  }
  return(registration_fee)
}



# Define a function to calculate IRR for fixed annual cash flow
calculate_IRR <- function(initial_investment, annual_cash_flow, yrs) {
  
  initial_investment <- as.numeric(initial_investment)
  annual_cash_flow <- as.numeric(annual_cash_flow)
  yrs <- as.numeric(yrs)
  # Create a cash flow series with the initial investment followed by fixed annual cash flows
  cash_flow_series <- c(-initial_investment, rep(annual_cash_flow, yrs))
  
  # Calculate IRR
  irr_result <- irr(cash_flow_series)*100
  
  # Return the IRR value
  return(irr_result)
}



# shrink outliers
shrink_outliers <- function(x, lower_percentile = 0.005, upper_percentile = 0.995) {
  lower <- quantile(x, probs = lower_percentile, na.rm = TRUE)
  upper <- quantile(x, probs = upper_percentile, na.rm = TRUE)

  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

