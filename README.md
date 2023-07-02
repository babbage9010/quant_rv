# quant_rv
quant_rv is a quantitative ETF trading strategy based on realized volatility, written in R. it's released under the MIT license.

the goal (as of June 2023) is to create a strategy that: 
1. develops a signal based on market close values and trades on the next-day Open (easy to trade)
2. makes sense logically (not based on magic)
3. can beat market benchmarks on a risk-adjusted basis, and hopefully on a CAGR basis

i'll be blogging along with this at [https://babbage9010.wordpress.com](https://babbage9010.wordpress.com)

Note 1 (July 2 2023): these are goals. The first two are already met, I believe, although I plan to document why it isn't magic.  The last one (beating benchmarks) is not met yet, except barely perhaps, if you only look at the Sharpe ratio.  It's definitely not a great strategy yet, or even more than a proof of concept. But I have high hopes.
