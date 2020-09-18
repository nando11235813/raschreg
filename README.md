# raschreg

The library _raschreg_ was created (an curently mantained) by a small team of statisticians from Universidad de la República, Uruguay. It's primary pourpose is to to provide R users with a framework of item response theory (IRT) models allowing for the possibility to include regression effects for the person parameters.   
***
## The models
Probabilistic structure for the models is:   
<p align="center">
   P(Y<sub>j</sub>=1|&theta;<sub>i</sub>) = logit(&alpha;<sub>j</sub>(&theta;<sub>i</sub> - &delta;<sub>j</sub>))   
   &theta;<sub>i</sub> ~ N(0, 1)   
</p>
Where &theta;<sub>i</sub>, &delta;<sub>j</sub> and &alpha;<sub>j</sub> represent person, difficulty and discrimination parameters respectively.
***
_raschreg_ provides a set of functions to fit, analyse and plot IRT models intended to analyse binary items. Currnet supported models are:   
* Rasch model (with discrimination paramter set to 1)
* Rasch model with free discrimination parameter (common to all items)
* Two parameter logistic model
All previous models allow the posibility to incorporate explanatory variables (either qualitative or quantitative) for person parameters. This is:   
<p align="center">
   &theta;<sub>i</sub> ~ N(X'&beta;, 1)
</p>
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
