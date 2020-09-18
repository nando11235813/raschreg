# raschreg

The library _raschreg_ was created an mantained by a group of statisticians from Universidad de la República, Uruguay. It's primary pourpose is to provide R users with a framework for Item Response Theory (IRT) models, allowing for the possibility to include regression effects for the person parameters.   
***
## The models
Having a set of _p_ items (Y<sub>1</sub>, Y<sub>2</sub>, ..., Y<sub>p</sub>) the probabilistic structure for the models is:   
P(Y<sub>j</sub>=1|&theta;<sub>i</sub>) = logit(&alpha;<sub>j</sub>(&theta;<sub>i</sub> - &delta;<sub>j</sub>))   
&theta;<sub>i</sub> ~ N(0, 1)   
Where &theta;<sub>i</sub>, &delta;<sub>j</sub> and &alpha;<sub>j</sub> represent person, difficulty and discrimination parameters respectively.

_raschreg_ provides a set of functions to fit, analyse and plot IRT models intended to analyse binary items. Currently supported models are:   
* Rasch model (&alpha;<sub>1</sub>=&alpha;<sub>2</sub>=...=&alpha;<sub>p</sub>=1)
* Rasch model with free discrimination parameter (&alpha;<sub>1</sub>=&alpha;<sub>2</sub>=...=&alpha;<sub>p</sub>)
* Two parameter logistic model   
All previous models allow the posibility to incorporate explanatory variables (either qualitative or quantitative) for person parameters. This is:   
&theta;<sub>i</sub> ~ N(X'&beta;, 1)   
Finally, the comply with different model constraints, _raschreg_ allow the posibility to restrict difficulty parameters so that:  &delta;<sub>1</sub>+...+ &delta;<sub>p</sub>=0
***
## Features
1. Models are by maximum marginal likelihood, estimated integrating person parameters via Laplace approximation. Estimated models ar stored in _rasch_ objects similar to _lm_ or _glm_ objects.   
2. Usual R mehods for (generalized) linear models are implemented, these include:   
    * coef
    * summary
    * plot
    * anova
    * update
    * and more
3. Stored _rasch_ objects con be visualized using:
    * Item characteristic curves and information curves
    * Person item maps
    * Forest plots
***
## Installation
Esta parte si que no se como sería...
