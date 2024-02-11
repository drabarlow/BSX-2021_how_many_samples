# script for carrying out power analysis on the lizard experiment

# declare variables
nsamp = 30
meanA = 10
meanB = 10
stdev = 1
x = 0
nreps = 100

# here I am just making an empty plot
plot(1, type="n", xlim=c(0,3), ylim=c(8,11),
     xlab = "population", ylab = "snout-vent length (cm)",
     axes=FALSE)

axis(1, at=c(1, 2), labels=c("A", "B"), lwd=-1, lwd.ticks=1)
axis(2)

# set up loop to repreat experiement
for (i in 1:nreps) {
  # take nsamp lizard measurments from each population
  lizA <- rnorm(nsamp, meanA, stdev)
  lizB <- rnorm(nsamp, meanB, stdev)

  # add sample means and add to plot
  points(jitter(1), mean(lizA))
  points(jitter(2), mean(lizB))

  # run t test, increment x if significant
  my_t <- t.test(lizA, lizB)
  if(my_t$p.value < 0.05) {x = x + 1}
}

# title to report outcome of t test
title(main=paste("p < 0.05 in", x, "out of", nreps, "tests"))
