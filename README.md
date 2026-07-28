A financial derivative is nothing more than a contract between two counterparties
that stipulates the purchase or sale of a certain quantity (and
quality) of a good or security (called the underlying product) at one (or up
to one) predetermined future moment and price. Underlying product in such
an agreement can be practically anything. For example, it can be a nan-
cial instrument (e.g. bond, share), a commodity (e.g. gold, corn, cotton),
a stock index (e.g. FTSE100, NASDAQ), a reference rate (e.g. LIBOR),
some exchange rate (e.g. euro/yen), the weather (weather derivatives) or
even some other derivative. Such agreements are traded either on regulated 
stock markets or over-the-counter and
may be binding equally on both parties or only on one party. Nowadays,
the use of such agreements is imperative by investors, both for reasons of
insecurity against the various nancial risks to which they are exposed, and
for making a prot.
As it is understood, there are many dierent types of derivative products,
the most well-known of which are: Forwards, Futures, Swaps and nally
Options, which are the subject of this work. The main dierence between
the options and the other derivatives is that they give their buyer the right
and not the obligation to exercise the contract in his possession (i.e. to
buy or sell the underlying product in accordance with the predetermined
terms). It is therefore obvious that the buyer of an option is in a more
advantageous position vis-a-vis the seller of the right, who, in the event that
the buyer decides to exercise, is obliged to comply with the terms of the
contract between them. Because of this advantage, the buyer pays the seller
a fee, at the maturity, to take in his possession the right, which is known as
the premium option or option price.
Finding this price is a particularly demanding problem, mainly due to the
randomness that characterizes the evolution of the prices of the underlying
securities/commodities, on which the option is structured. Various pricing
models have been proposed from time to time to solve this problem, the
most famous of all being the Black-Scholes model [10]. This model is a
model in real time and is essentially a model in a closed form for the pricing
of European type of options (i.e. options that can only be exercised at
maturity). Despite its usefulness, the Black-Scholes model cannot be used
to price rights that have a more complex structure than European ones,
such as American options (these can be exercised at any time until their
expiration). A classic way to address this problem is with the help of the
binomial or even the trinomial pricing model.
The binomial pricing model, in its best-known form, is based on an
original concept of Sharpe, which was expanded by the Cox, Ross and
Rubinstein. The philosophy of this model is very simple and is based
on the assumption that at the next point in time the underlying title can
get only two possible values, one upward or one downward. The trinomial
pricing model (Boyle, Kamrad and Ritchken) is a direct extension
of the binomial model whose basic assumption is that at the next point in
time the underlying title can get three possible values instead of the two
assumed by the binomial model, one upward, one downward or its price
may not change. Despite their simple nature, these models can be used
quite satisfactorily to price exotic rights (e.g. American options, options
with barriers, etc.). Of course, these templates are not the only ones that
exist for pricing options. Many other models/numerical procedures for the
pricing of options have been proposed from time to time (e.g. models based
on Monte-Carlo simulation techniques, machine learning techniques, finite
difference method, etc.).
This paper is divided into two parts. In the first part, after a detailed
introduction to the issue of option pricing and some key elements have been
presented, the binomial pricing model (Cox, Ross and Rubinstein), and
the trinomial pricing model (Kamrad and Ritchken) are presented in
detail. These models will then be compared with each other (both in terms
of the accuracy of the approach and in terms of the speed of convergence
at the (theoretical) reference price) in the case of: (a) a European right to
buy, (b) the European put option, and (c) the US right to sell. For this
purpose, and for each of the above cases, various useful statistical criteria
will be used, e.g. minimum convergence step, root mean square error, etc.,
which will help us to draw useful conclusions. In the second part, we return
to the trinomial pricing model and see it from a different perspective. More
specifically, we examine the connection of the trinomial model with the finite
difference method for the numerical solution of the Black-Scholes equation.
