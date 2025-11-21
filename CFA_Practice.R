library(lavaan)
library(semPlot)


# Load & Check the Structure 
data(HolzingerSwineford1939)
head(HolzingerSwineford1939)

# Specify the CFA model
model <- '
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9
'

#'visual', 'textual', and 'speed' are latent factors.
#'x1' to 'x9' are observed variables representing the cognitive test scores.

# Run CFA
cfa_result <- cfa(model, data = HolzingerSwineford1939)

# Interpret the results
summary(cfa_result)
semPaths(cfa_result, "std")

?semPaths
