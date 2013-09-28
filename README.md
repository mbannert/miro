miro -- Meta Information for R Objects
====

miro is an R package that helps useRs to store meta information without affecting
the original objects (except for a key attribute). Miro stores all meta information in 
a separate environment. miro uses reference classes to store the actual meta information. 
Currently the following uses cases are targeted: 

1. Univariate Time series (one variable over time)
2. Cross sectional data (multiple observations, multiple variables. The standard data.frame case)
3. Panels (multiple individuals, multiple variables over time)




