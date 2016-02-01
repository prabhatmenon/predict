require(fpp)

fit <- stl(elecequip, t.window=15, s.window="periodic", robust=TRUE)
plot(fit)
