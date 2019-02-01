# Microcolony Landscape Resource Simulation Experiments
## Daily working notes and ideas

#### Summary Figures To Make: 
- [x] Step plot for drone production over time
- [x] Mass-gain of MC over experiment period
- [x] Diverging z-score plot of drone production
- [x] Diverging z-score plot of avg. daily mass gain
- [x] Nectar consumption over time
- [x] Pollen consumption over time

#### Data Checking
- [x] Data shape/spread/normality checks 

#### Data Cleanup
- [x] Food consumption calculations (averaging out left-over pollen, nectar diff.)


#### Notes
**8/10:** Starting to think about how to analyze these data.  Need to first calculate more accurate measure of colony mass.  So far, I have this in mind as an example:

* `day1.mass = mc.mass - mass.box + p.mass.cons(days 1-9) / 4` 
* `day3.mass = mc.mass - mass.box - mass.pol.fd(day 1) + p.mass.cons(days 1-9) / 4`
* `day6.mass = mc.mass - mass.box - mass.pol.fd(day 1 + 3) + p.mass.cons(days 1-9) / 4`
* `day9.mass = mc.mass - mass.box - mass.pol.fd(day 1 + 3 + 6) + p.mass.cons(days 1-9) / 4`
* `day12.mass = mc.mass - mass.box + p.mass.cons(days 12-21) / 4`

**12/13:** Welp, been a minute.  Most all summary plots are made, in addition to a few basic analytical frameworks to model the exp. as a repeated measures anova (see `D2018_MicroCol_SummaryPlots` and `D2018_MicroCol_Models`).  Need an assortment of more specific models to test original hypotheses.  

Data summary/cleanup from Rounds 1/2 are similar, but Round 1 had a few issues with data structure that necessitated some customization/hard coding of feed interval elements.  Documentation on that code will be important.  

**12/14:** Working on MC analyses today for R1/R2 mass, drone production, etc.  Going to include models for:

- [x] Drone production throughout experiment (repeated measures ANOVA - LMM - need GLMM)
- [x] Drone production overall experiment (ANOVA) 
- [x] Mass gain throughout experiment (repeated measures ANOVA - LMM) 
- [ ] End mass by treatment (ANOVA)
- [ ] Drone fitness metrics (ANOVA)
- [ ] Drone fitness w/food availability (LM)
- [ ] Growth rates (repeated measures? ANOVA?) 
- [ ] Pollen/nectar consumption rates? Highly linear/autocorrelated - not sure how to go about this

Issues with Round 1 drone metrics - need to figure out a way to get a fd.day variable in there...

**12/18:** Round 1 mass may be best modeled with a sqrt transformation to normalize data.  Model fit suggests there aren't issues, but response histogram looks much better when sqrt transformed. 

Moving on to modeling drone production as a repeated measures design.  Need to do as GLMM due to non-continuous response (cumulative drone production).  How to implement AR1 autocorrelation structure? Addn'l random effect of date? I'll first try log-transforming (per Ives 2015) to see how model diagnostics are - this way I can easily account for autocorrelation structure using lme.  

**12/20:** Round 1 initial analyses done.  Some autocorrelation issues still in a few of the models that needs to be looked at/asked about.  Now need to work on Round 2, plus round 1 secondary metrics (e.g., growth rate models for different experimental periods), and variance tests for drone fitness metrics.  

Also need to clean up `D2018_MicroCol.R` code so that there is minimal overlap in data and such that all necessary data are cleaned/created and no code is missing.

For Round 1, 2.4 being removed due to lack of establishment/overaggressive worker
For Round 2, 1.4 being removed due to lack of establishment.

Going to need to explore GAMs for the Round 2 drone - dianostic plots look pretty poor (or try log-transforming)

**12/21:** Played with GAMMs this morning.  From basic diagnostic plots (resid v fitted), terrible fit with AR1 temporal autocorrelation structure.  Probably more my ignorance than anything else.  

**1/14:** Time to maturity (per colony and treatment average), worker mortality (per colony and averaged per treatment).  Calc ASAP. Also inter-period slope calculations

**1/17:** Calculated time to first drones and ran stats on that.  Also calculated worker mortality and ran stats. Need to do a RM ANOVA though to see if worker mortality spikes during dirth periods. 

**1/31:** Calculate amount of pollen to produce a drone (on avg) as well as proportion of food consumed per treatment for each pollen and nectar - NEED TO RE-RUN `mc3.feed.df` AS I FOUND RECORDING ERROR.  DO AFTER MEETING AND RE-CHECK ALL RECORDINGS.
