OzDASL

Sleep in Mammals

Keywords: multiple regression, transformation, regression tree,
compositional data

------------------------------------------------------------------------

    Description

Includes brain and body weight, life span, gestation time, time
sleeping, and predation and danger indices for 62 species of mammals. Of
interest is to predict the time spent sleeping and the proportion of
sleep time in dream sleep.


------------------------------------------------------------------------
	Variable 		Description

------------------------------------------------------------------------
	BodyWt 		body weight (kg)
	BrainWt 		brain weight (g)
	NonDreaming 		slow wave ("nondreaming") sleep (hrs/day)
	Dreaming 		paradoxical ("dreaming") sleep (hrs/day)
	TotalSleep 		total sleep, sum of slow wave and paradoxical sleep (hrs/day)
	LifeSpan 		maximum life span (years)
	Gestation 		gestation time (days)
	Predation 		predation index (1-5)
1 = minimum (least likely to be preyed upon); 5 = maximum (most likely
to be preyed upon)
	Exposure 		sleep exposure index (1-5)
1 = least exposed (e.g. animal sleeps in a well-protected den); 5 = most
exposed
	Danger 		overall danger index (1-5) (based on the above two indices and
other information)
1 = least danger (from other animals); 5 = most danger (from other animals)

------------------------------------------------------------------------

    Source

Allison, T., and Cicchetti, D. V. (1976). Sleep in mammals: ecological
and constitutional correlates. /Science/ *194* (November 12), 732-734.
The electronic data file was obtained from the Statlib database
<http://lib.stat.cmu.edu>.


    Analysis

One can use multiple regression to predict the three sleep variables
after suitably transforming the predictors and response. The response
variable could be hours of sleep or the proportion of sleep spent
dreaming. There are quite a few missing values. I have also used this
data to illustrate regression trees for predicting duration of dream-sleep.


