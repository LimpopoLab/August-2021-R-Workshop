# Parallelization Example
# Source:
# https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html

q <- 1:1e8
x <- cbind(q,q,q)

# Series
y <- array(NA, dim = ncol(x))
s1 <- system.time({
for (i in 1:ncol(x)) {
      y[i] <- sum(x[,i])
}
})
print(s1)
#  user  system elapsed
# 1.311   0.603   1.944
# 1.315   0.600   1.951
# 1.291   0.603   1.915
# 1.295   0.600   1.917
# 1.312   0.619   1.968

# Parallel
library(doParallel) # loads parallel and foreach
registerDoParallel(detectCores())
rm(z)
p2 <- system.time({
z <- foreach (i=1:ncol(x), .combine = cbind) %dopar% {
      sum(x[,i])
}
})
print(p2)
#  user  system elapsed
# 1.334   1.134   1.295
# 0.009   0.033   1.333
# 0.009   0.015   1.237
# 0.009   0.065   1.307
# 0.010   0.065   1.293

