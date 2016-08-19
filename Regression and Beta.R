library('quantmod')
library('XML')
 
#First I get the tickers of Dow Jones's components
url <- c("http://www.cnbc.com/dow-components/")
  symbols <- data.frame(readHTMLTable(url))
tickers <- c(as.vector(symbols$NULL.Symbol))
number_components = length(tickers)

#I am interested in daily returns since 5 July 2015 to 4 July 2016. There were not trading days between 2 July 2015 and 6 July 2015. Therefore I get adjusted close prices from 2 July 2015 to 4 July 2016 . 
#I need from 2 July 2015 not from 6 July 2015 for comparison reasons. I compare returns with adjusted close price of 2 July 2015.
#I use separate arrays for DJIA and its components
#First I download DJIA
getSymbols('^DJI',src="yahoo",from='2015-07-02',
to ='2016-07-04')
#Here I calculate the number of trading days
number_trade_days = length(Ad(DJI))-1
#I subtract -1, since I need the first close price of 2 July 2015 just for comparison.
#here I put the adjusted close  prices of DJIA in a matrix
DJIA_adj_close_prices <- matrix(nrow = number_trade_days+1, ncol = 1)
DJIA_adj_close_prices = as.numeric(Ad(DJI))

#Next I download the components
getSymbols(tickers,src="yahoo",from='2015-07-02',
to ='2016-07-04')
#here I put the adjusted close prices of the components from 2 July 2015 to 4 July 2016 in a matrix
comp_adj_close_prices <-matrix(nrow = number_trade_days+1, ncol = number_components)
for(i in 1:number_components)
comp_adj_close_prices[,i] = as.numeric(Ad(eval(parse(text = tickers[i]))))
comp_adj_close_prices

#here I calculate the daily  returns
#First I calculate them for DJIA
DJIA_daily_returns <- matrix(nrow = number_trade_days, ncol = 1)
for (i in 1:number_trade_days)
DJIA_daily_returns[i] = DJIA_adj_close_prices[i+1]/DJIA_adj_close_prices[i] -1


#Next I calculate them for the components
comp_daily_returns <- matrix(nrow = number_trade_days, ncol = number_components)
for (j in 1:number_components) for (i in 1:number_trade_days)
comp_daily_returns[i,j] = comp_adj_close_prices[i+1,j]/comp_adj_close_prices[i,j] -1

#I do linear regressions fits for all the components
#DJIA daily returns play role of explanatory variable
#Component daily returns play role of response variable
#First I initialize a vector for alpha and beta
alpha <- vector(mode = 'numeric',number_components)
beta <- vector(mode = 'numeric',number_components)

for (j in 1:number_components)
{
	t_line <- lm(comp_daily_returns[,j]~DJIA_daily_returns)
	coeffs <- coefficients(t_line)
	alpha[j] = coeffs[1]
	beta[j] = coeffs[2]
}

#Graphically represent the distribution of α(alpha) and β values for the constituents of DJIA
plot(x = alpha,y = beta, main = "Distribution of alphas and betas for constituents of DJIA",xlab = "alphas",ylab = "betas")


