## Installation
Install this package via `remotes::install_github("pchiroque/PermuteTest")`.

## Usage 
The response `y` must be vector with $y\in[0,1]$.

The covariates must be a ´data.frame` and have the same numbers of the rows as the response.

### Example 

```R
set.seed(4)

set.seed(4)

y <- c(yt,yc)

x <- c(xt,xc)

treat <- c(rep(1,length(yt)),rep(0,length(yc)))

# PITE 

data <- data.frame(cbind(y,x,treat))

datat <-data[treat==1,]
datac <-data[treat==0,]

pite_mc <- pite.ind(datac,datat)
sd(apply(pite_mc,2,mean))

# Permutation test

# set up a matrix with all permutations of treatment assignment.
# each column corresponds to one set of permutated labels.

nperm = 2
full_trtperms <- replicate(n=nperm, sample(treat, replace = F))

perm.test.pite <- lapply(1:nperm,function(p)perm.test(datac,datat,treat,full_trtperms[,p]))
lapply(lapply(perm.test.pite, function(x){apply(x,2,mean)}),sd)

```


