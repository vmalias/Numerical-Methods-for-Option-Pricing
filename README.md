# Numerical Methods for Option Pricing

•	Introduced the binomial model of Cox, Ross and Rubinstein (CRR) and the trinomial model of Kamrad and Ritchken (KR) of multiple periods. 

•	Developed the pricing algorithms based on these models for European call and put options and American Put options.

•	Separated three cases: when the option expires in the money (ITM), out the money (OTM) and at the money (ATM) and conducted sensitivity analysis of the common parameters. Observed that using the KR model yields better approximations of the Black-Scholes value when the strike price, interest rate, volatility, and expiration time vary. 

•	Confirmed that KR converges, when lambda takes values near 1 or 2.

•	Employed the minimum convergence step metric to test the convergence speed and simulated 2000 contracts to test the accuracy of the models, using root mean squared error. The trinomial model seemed to converge faster and smoother.

•	Applied the explicit finite difference method to approximate the Black-Scholes solution for European call and put options and then adjusted the method to value American puts. Observed that the explicit finite difference method can be seen as a lattice extension of the trinomial model and employs a very similar pricing algorithm. However, this method does not work reliably for all parameter sets. It can work under certain assumptions, regarding the partition of time and space.

•	Applied the implicit finite difference method for the same instances and solved the linear system that occurred using LU-Decomposition, Jacobi and Gauss-Seidel methods. In comparison to the explicit method, the implicit is consistent across all parameter sets, though its computational cost is slightly higher.

•	Concluded that the trinomial model performed better in terms of accuracy and convergence speed compared to binomial and finite difference methods.
