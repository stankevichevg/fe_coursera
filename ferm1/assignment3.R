# Q2
# Suppose a 6-year swap with a notional principal of $10 million is being
# 
# configured. What is the fixed rate of interest that will make the value
# 
# of the swap equal to zero. (You should use the term structure of interest rates from Question 1.)

irs = c(0.07, 0.073, 0.077, 0.081, 0.084, 0.088)
ds = c(irs)
for(i in 1:6) 
{
  ds[i] = 1 / (1 + irs[i])^i
}
X = (1 - 1 / (1 + irs[6])^6) / sum(ds)

# Q4.
# Suppose a farmer is expecting that her crop of grapefruit will be ready for
# 
# harvest and sale as 150,000 pounds of grapefruit juice in 3
# months time. She would like to use futures to hedge her risk but unfortunately there
# 
# are no futures contracts on grapefruit juice. Instead she will use orange juice futures.
# 
# Suppose each orange juice futures contract is for 15,000
# pounds of orange juice and the current futures price is F0=118.65 cents-per-pound.
# 
# The volatility, i.e. the standard deviation, of the prices of
# 
# orange juice and grape fruit juice is 20% and 25%, respectively,
# 
# and the correlation coefficient is 0.7. What is the approximate number
# 
# of contracts she should purchase to minimize the variance of her payoff?

# correlation
P = 0.7;
# Standard deviation of the spot
S = 0.25;
# Standard deviation of the futures
F = 0.20;
# Size of position one wants to hedge
QA = 150000.00;
# Size of futures
QF = 150000.00;
# per contract amount
C = 15000.00;
h = ( QA / C ) * ( P * ( S / F ) )
N = ( (h * QA) / QF )

# Q5
# Consider a 1-period binomial model with R=1.02, S0=100,
# 
# u=1/d=1.05. Compute the value of a European call option on the stock
# 
# with strike K=102. The stock does not pay dividends.

R   = 1.02;
S0  = 100.00;
D   = 1.05;
U   = 1.00;
K   = 102.00;
uS0 = U * S0;
dS0 = D * S0;
Cu = uS0 - K;
Cd = 0;
H = ( ( Cu - Cd ) / ( uS0 - dS0 ) );
HdS0 = H * dS0;
PV = ( ( HdS0 / ( 1 + R ) ) );
HS0 = H * S0;
C = HS0 - PV;

# Q6
# When you construct the replicating portfolio for the option in the previous 
# question how many dollars do you need to invest in the cash account?

A<-matrix(c(105,100/1.05,1.02,1.02),2,2)
b<-c(3,0)
solve(A,b)
