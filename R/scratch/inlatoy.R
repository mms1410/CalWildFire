library(INLA)
library(sp)
data("SPDEtoy")
#-------------------------------------------------------------------------------
SPDEtoy.sp <- SPDEtoy
coordinates(SPDEtoy.sp) <- ~ s1 + s2
bubble(SPDEtoy.sp, "y", key.entries = c(5, 7.5, 10, 12.5, 15),
       maxsize = 2, xlab = "s1", ylab = "s2")

# INLA uses default primer on coefficient and intercept
# $y \sim \mathcal{N}(\mu_i, \tau^{-1]})$ for $i \in [200]$
# $\mu_i = \alpha + \beta_1 s_{1i} + \beta_2 s_{2i}$
# $\alpha  \sim \Unif$
# $\beta_j \sim \mathcal{N}(0, 0.001^{-1})$ for $j \in [2]$
# $\tau \sim \Ga{1, 0.00005}$
m0 <- inla(y ~ s1 + s2, data = SPDEtoy)
summary(m0)
