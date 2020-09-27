\name{odont_ncd}
\alias{odont_ncd}
\docType{data}
\title{
 Oral and Non-Communicable Diseases
}
\description{
  A data frame comprising 602 observations and 13 variables regarding sex, age, four common oral conditions, three behavioral factors and four metabolic factors.
}
\usage{data("odont_ncd")}
\format{
  A data frame with 602 observations on the following 13 variables.
  \describe{
    \item{\code{sex}}{Sex, categories are M and F.}
    \item{\code{age}}{Age}
    \item{\code{education}}{Highest educational level achieved, categories are elemmentary school, high school and dollege.}
    \item{\code{phy_act}}{Physical Activity, categories are adequate and inadequate according to WHO criteria.}
    \item{\code{hypert}}{Hypertension, categories are no and yes.}
    \item{\code{diabetes}}{Diabetes, categories are no and yes.}
    \item{\code{b_drink}}{Binge drinking, categories are yes and no.}
    \item{\code{tobacco}}{Tobacco consumption, categories are current, former and never.}
    \item{\code{bmi}}{Body Mass Index, categories are <25 and >25.}
    \item{\code{pocket}}{Periodontal Pockets (0 = No, 1 = Yes).}
    \item{\code{loa}}{Loss of Attachment (0 = No, 1 = Yes).}
    \item{\code{decay}}{Decay (0 = Absence of decayed theet, 1= At least one decayed tooth).}
    \item{\code{lesions}}{Mucosal lesions (0 = No, 1 = Yes).}
    \item{\code{less_10}}{Less than 10 theet (0 = No, 1 = Yes).}
    \item{\code{bleed}}{Bleeding (0 = No, 1 = Yes).}
  }
}
\details{
The data were obtained from a cross-sectional study executed in the Uruguayan Faculty of Dentistry between 2015 and 2016 collecting information of people whe required dental care. The study was founded by the i+d programe of the sectoral committee for scientific research.
}

\source{
  \insertRef{lorenzo2020}{raschreg}
}

\examples{
data(odont_ncd)
items <- odont_ncd[,c('pocket', 'loa', 'decay', 'less_10', 'bleed', 'lesions')]

mod <- raschreg(items, ~ rcs(age, 3) + sex, odont_ncd)

summary(mod)
forest(mod)
}
\keyword{datasets}
