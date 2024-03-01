# quant_rv
quant_rv is a quantitative ETF trading strategy based on realized volatility, written in R. it's released under the MIT license.

HowTo: the code is written in R, and explained at the blog. In short, install R Studio, download/save these R files to your computer and load them into R Studio and "source" them. The first time you may need to "install" a couple libraries (the error messages should help with that). More explicit beginner instructions someday, but it's pretty easy.

i am blogging along with this at [https://babbage9010.wordpress.com](https://babbage9010.wordpress.com) Code releases are separate R scripts that tie into the blog posts. so far i've kept it up for about 17 posts in eight months, and starting to get somewhere interesting

====== Feb 2024 versions ======

quant_rv_2.0.0 met my goals for beating SPY total return in real and risk-adjusted returns by investing in SPY or SH depending on the daily signal. I stopped further dev on quant_rv there for now. My latest contribution, qrvx, takes two long and one short legs from quant_rv and combines them into a strategy with a negative correlation to SPY, making it suitable for combining in 60/40 fashion with SPY to get even better risk-adjusted returns than before.

qrvx_2.0.0 is a subset of quant_rv specifically crafted to have a very low to negative correlation with SPY. You can read [the blog post about it here](https://babbage9010.wordpress.com/2024/02/26/replacing-the-40-with-qrvx-in-r/).

====== November 2 2023 version ======

quant_rv_1.3.3 consistently beats SPY total return by investing in SPY or SH depending on the daily signal. It meets all the goals set out for the July 3 version (below). It has not been shown to meet the stretch goal (#7 below) but that may show up in future posts as it is further explored. Also to be explored... testing with market data from before and after the in-sampe test period (July 2006 to December 2019).

====== July 3 2023 version ======

the goal (as of July 3 2023) is to create a strategy that: 
1. trades popular, liquid ETFs (to allow it to scale meaningfully) with no extra leverage (no 2x or 3x ETFs)
2. develops signals based on sensible, logical, statistically meaningful market observations (like realized volatility)
3. trades at the next-day Open with signals based on the previous day’s market data (allowing plenty of time for followers to generate signals and place trades)
4. unequivacably beats a benchmark of buy-and-hold SPY (including dividends compounded, ie., calculated using Adjusted Close) on all these metrics: Annual Return, Annualized Standard Deviation, Sharpe Ratio, Max Drawdown
5. uses the same instruments as the benchmark in order to meet these goals (ie, SPY or derivatives/equivalents, not QQQ or some specific market sector ETF)
6. accomplishes its goals without consideration of dividends collected by the strategy (quant_rv gets one hand tied behind its back)
7. stretch goal: also performs reasonably well across several different market/ETF areas, to show that it really meets the #2 (sensible) goal above

Note 2 (July 3 2023): the earlier goals were met in the quant_rv 1.1.0 release, please read details on the blog for that release and a bit more about setting these new goals: 
 [https://babbage9010.wordpress.com/2023/07/03/meeting-goals-setting-higher-goals](https://babbage9010.wordpress.com/2023/07/03/meeting-goals-setting-higher-goals)

====== June 30 2023 version ======

the goal (as of June 2023) is to create a strategy that: 
1. develops a signal based on market close values and trades on the next-day Open (easy to trade)
2. makes sense logically (not based on magic)
3. can beat market benchmarks on a risk-adjusted basis, and hopefully on a CAGR basis

Note 1 (July 2 2023): these are goals. The first two are already met, I believe, although I plan to document why it isn't magic.  The last one (beating benchmarks) is not met yet, except barely perhaps, if you only look at the Sharpe ratio.  It's definitely not a great strategy yet, or even more than a proof of concept. But I have high hopes.
