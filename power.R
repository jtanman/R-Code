p_load(pwr)

# Gaussian

M1  = .05                      # Mean for sample 1
M2  = .04                    # Mean for sample 2
# M2 = 1.05*M1

S1  =  5                      # Std dev for sample 1
S2  =  2                     # Std dev for sample 2

Cohen.d = (M1 - M2)/sqrt(((S1^2) + (S2^2))/2)

pwr.t.test(n = NULL, d = Cohen.d, sig.level = .05, power = .95, type = 'two.sample', alternative = 'two.sided')

pwr.t.test(n = NULL, d = Cohen.d, sig.level = .05, power = .95, type = 'two.sample', alternative = 'greater')

# Proportions

p1 = .05
p2 = .04

Cohen.h = ES.h(p1, p2)

pwr.2p.test(h = Cohen.h, n = NULL, sig.level = .05, power = .95, alternative = 'greater')

n <- 1000
n1 <- n * .9/.95
n2 <- n * .05/.95

n1 <- n * .9
n2 <- n * .1

pwr.2p2n.test(h = Cohen.h, n1 = n1, n2 = n2, sig.level = .05, power = NULL, alternative='greater')

getNRatio <- function(n, p=.8){
  
  n1 <- n * .9/.95
  n2 <- n * .05/.95
  
  power <- pwr.2p2n.test(h = Cohen.h, n1 = n1, n2 = n2, sig.level = .05, power = NULL, alternative='greater')$power
  power
  (p - power)^2
  return((p - power)^2)
}
plot(getNRatio,from=100,to=1e6, n=100)
optim(10000, getNRatio, lower=0, upper=1e6, method='Brent')

optimize(getNRatio, interval=c(0, 1e5), maximum=FALSE, p=.8)


     
