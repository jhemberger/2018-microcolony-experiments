# Microcolony Landscape Resource Simulation Experiments
## Daily working notes and ideas

#### Summary Figures To Make: 
- [ ] Step plot for drone production over time
- [x] Mass-gain of MC over experiment period
- [x] Diverging z-score plot of drone production
- [x] Diverging z-score plot of avg. daily mass gain
- [x] Nectar consumption over time
- [x] Pollen consumption over time

#### Data Checking
- [ ] Data shape/spread/normality checks 

#### Data Cleanup
- [x] Food consumption calculations (averaging out left-over pollen, nectar diff.)
- [ ] 

#### Notes
__8/10:__  Starting to think about how to analyze these data.  Need to first calculate more accurate measure of colony mass.  So far, I have this in mind as an example:
`day1.mass = mc.mass - mass.box + p.mass.cons(days 1-9) / 4` 
`day3.mass = mc.mass - mass.box - mass.pol.fd(day 1) + p.mass.cons(days 1-9) / 4`
`day6.mass = mc.mass - mass.box - mass.pol.fd(day 1 + 3) + p.mass.cons(days 1-9) / 4`
`day9.mass = mc.mass - mass.box - mass.pol.fd(day 1 + 3 + 6) + p.mass.cons(days 1-9) / 4`
`day12.mass = mc.mass - mass.box + p.mass.cons(days 12-21) / 4`

