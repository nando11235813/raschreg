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
    \item{\code{d_smoke}}{Daily smoking (0 = No, 1 = Yes).}
    \item{\code{drink_2w}}{Drinking  (0 = Less than 2 times per week, 1 = 2 times per week, or more).}
    \item{\code{phy_act}}{Physical Activity (0 = Does not meet WHO requirements, 1 = Metts WHO requirements).}
    \item{\code{pocket}}{Periodontal Pockets (0 = No, 1 = Yes).}
    \item{\code{f_dentition}}{Functional Dentition (0 = 20 theet or more, 1 = Less than 20 theet).}
    \item{\code{decay}}{Decay (0 = Absence of decayed theet, 1= At least one decayed tooth).}
    \item{\code{loa}}{Loss of Attachment (0 = No, 1 = Yes).}
    \item{\code{bmi}}{Body Mass Index}
    \item{\code{wth_r}}{Waist to Hip Ratio}
    \item{\code{hypert}}{Hypertension (0 = No, 1 = Yes)}
    \item{\code{diabetes}}{Diabetes (0 = No, 1 = Yes)}
    \item{\code{age}}{Age}
    \item{\code{sex_f}}{Sex (0 = Male, 1 = Female)}
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
items <- odont_ncd[,c('pocket', 'loa', 'decay', 'f_dentition')]

mod <- raschreg(items, ~ age + sex_f, odont_ncd)

summary(mod)
forest(mod)
}
\keyword{datasets}
