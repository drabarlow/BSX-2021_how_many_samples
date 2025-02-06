# script for carrying out power analysis on the lizard experiment

# declare variables
nsamp = 30
meanA = 10
meanB = 9.5
stdev = 1
x = 0
nreps = 100

# here I am making an empty plot with axes and labels
plot(1, type="n", xlim=c(0,3), ylim=c(8, 11),
     xlab="population", ylab="snout-vent length (cm)",
     axes=FALSE
)
axis(1, at=c(1, 2), labels=c("A", "B"), lwd=-1, lwd.ticks=1)
axis(2)

for (i in 1:nreps) {
  # take nsamps from each population
  lizA <- rnorm(nsamp, meanA, stdev)
  lizB <- rnorm(nsamp, meanB, stdev)

  # add sample means to the plot
  points(jitter(1), mean(lizA))
  points(jitter(2), mean(lizB))

  # run t test, increment x if significant
  my_t <- t.test(lizA, lizB)
  if(my_t$p.value < 0.05) {x = x + 1}
}

# add a ttile reporting outcome of t tests
title(main=paste("p values < 0.05 in", x, "out of", nreps, "tests"))
