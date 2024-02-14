# script for carrying out power analysis for the lizard experiment

nsamp = 30
meanA = 10
meanB = 10
stdev = 1
x = 0
nreps = 100

# here I am making a blank plot to add data to later
plot(1, type="n", xlim=c(0, 3), ylim=c(8, 11),
     xlab="population", ylab="snout-vent length (cm)",
     axes=FALSE
)

axis(1, at=c(1,2), labels=c("A", "B"), lwd=-1, lwd.ticks=1)
axis(2)

# setting up loop
for (i in 1:nreps) {

  # take nsamp samples from lizard populations
  lizA <- rnorm(nsamp, meanA, stdev)
  lizB <- rnorm(nsamp, meanB, stdev)

  # add sample means to plot
  points(jitter(1), mean(lizA))
  points(jitter(2), mean(lizB))

  # carry out t test, increment x if significant
  my_t <- t.test(lizA, lizB)
  if(my_t$p.value < 0.05) {x = x+1}
}

title(main=paste("p < 0.05 in", x, "out of", nreps, "tests"))
