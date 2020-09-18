# raschreg

The library _raschreg_ was created (an curently mantained) by a small team of statisticians from Universidad de la República, Uruguay. It's primary pourpose is to to provide R users with a framework of item response theory (IRT) models allowing for the possibility to include regression effects for the person parameters.   
***
## The models
Probabilistic structure for the models is:
$P(Y_j = 1|\theta_i) = \logit(\alpha_j(\theta_i - \delta_j))$
$\theta_i \sim N(0, 1)$
Where $\theta_i$, $\delta_j$ and $\alpha_j$ represent person, difficulty and discrimination parameters respectively.
***
_raschreg_ provides a set of functions to fit, analyse and plot IRT models intended to analyse binary items. Currnet supported models are:   
* Rasch model (with discrimination paramter set to 1)
* Rasch model with free discrimination parameter (common to all items)
* Two parameter logistic model
All previous models allow the posibility to incorporate explanatory variables (either qualitative or quantitative) for person parameters. This is:   
$\theta_i \sim N(X'\beta, 1)$
***
## Features
P(Y<sub>j<\sub>=1|&theta<\sub>i<\sub>) = logit(&alpha<sub>j<\sub>(&theta<sub>i<\sub> - &delta<sub>j<\sub>))
***
## Installation
