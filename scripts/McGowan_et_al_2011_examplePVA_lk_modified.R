# From Appendix A of McGowen et al 2011 paper at
# http://digitalcommons.unl.edu/cgi/viewcontent.cgi?article=1563&context=usgsstaffpub

# Orig code: Declare variables
it = 2#10000 # No. iterations in simulation.
yrs = 50 # No. years in simulation.
aSait = 62.5 # Alpha and beta shape parameters derived from
bSait = 22.3 # estimated mean adult survival and
# estimated sampling variance (Larson et al., 2000).
aSjit = 42.5 # Alpha and beta shape parameters derived from estimated mean
bSjit = 56.9 #juvenile survival
#and estimated temporal variance (Larson et al., 2000).
ilnFm = 0.80 # Shape parameters for the log-normal distribution for
ilnFsd = 0.3 # generating iteration-level fecundity values.
SaVar<- matrix(0, it,1) # Vector for adult survival variance values.
SaVarm = 0.00198 # Estimated temporal variance of adult survival
SaVars = 0.0001 # (Larson et al., 2000).
SjVar<- matrix(0,it,1) # Vector for Juvenile survival variance values.
SjVarm= 0.00198 # Estimated temporal variance on juvenile
SjVars = 0.0001 # survival (Larson et al., 2000).
Fsd<- matrix(0,it,1) # Vector for annual fecundity variance values.
Fsdm = 0.21 # mean temporal variance for fecundity.
Fsds = 0.01 # variance on mean variance
Sai<- matrix(0, it, 1) # Vector for the iteration-level adult Survival values
aSayr<- matrix(0, it, 1) # Vector for the iteration-level alpha shape
# parameter for each iteration for adult survival.
bSayr<- matrix(0, it, 1) # Vector for the iteration-level beta shape
# parameter for each iteration for adult survival.
Sji<- matrix(0, it, 1) # Vector for the iteration-level juvenile
# survival values
aSjyr<- matrix(0, it, 1) # Vector for the iteration-level alpha shape.
# parameter for each iteration for juvenile survival.
bSjyr<- matrix(0, it, 1) # Vector for the iteration-level beta shape.
# parameter for each iteration for juvenile survival.
Fi <- matrix(0, it, 1) # Vector for the iteration-level Fecundity
# values.
lnFm<- matrix(0, it, 1) # Vector for the iteration-level shape
# parameters for each iteration for fecundity.
lnFsd<- matrix(0, it, 1) # Vector for the iteration-level shape
# parameter for each iteration for fecundity.

Sa <- matrix(0, it, yrs) # Vector for the annual adult survival values
# for each year.
Sj<- matrix(0, it, yrs) # Vector for the annual juvenile survival
# values for each year.
F <- matrix(0, it, yrs) # Vector for the annual fecundity values for
# each year.
yy<-matrix(0,it,yrs) # Vector for young of the year produced.
# yy[i,1]=500 # initial young of the year.
yy[,1]=500 # initial young of the year.
P <- matrix(0, it, yrs) # Vector for population size.
P[1:it,0] = 2300 # sets initial population size.
e <- matrix(0,it,yrs) # Vectors to track extinction.
ep<- matrix(0,1,yrs)
  
# LK add: info to calculate and track probability of persistence
n_runs_per_it <- 10000
pe <- matrix(0,it, yrs) # Vector to track probability of extinction.


system.time({ # turn on the timer
# Orig code: initiates iteration loop; here we are trating this as the loop where we do the parameter draws
  for(i in 1:it)
  {
    
    print(paste("Running iteration #", i, "of", it))
    # select adult survival value and variance for each iteration from a beta
    # distribution, replace with 0.737 to eliminate sampling variation
    Sai[i] <- rbeta(1, aSait, bSait)
    SaVar[i] <- rnorm(1,SaVarm, SaVars)
    #??????Here we used a normal distribution because the rinvgauss function is not a
    # standard function in R, but must be installed in a separate package.
    # calculate alpha shape parameter for each iteration
    # aSayr[i] = Sai[i]???((Sai[i]???(1-Sai[i])/SaVar[i])-1)
    aSayr[i] = Sai[i]*((Sai[i]*(1-Sai[i])/SaVar[i])-1) # NOT SURE HOW THESE??? GOT IN THERE, REMOVING, DOUBLE CHECK CORRECT, SAME FURTHER DOWN IN SEVEFRAL SPOTS
    # Calculate beta shape parameter for each iteration
    # bSayr[i] = (1-Sai[i])???((Sai[i]???(1-Sai[i])/SaVar[i])-1)
    bSayr[i] = (1-Sai[i])*((Sai[i]*(1-Sai[i])/SaVar[i])-1)
    # select juvenile survival value for each iteration from a beta distribution, replace
    # with 0.45 to eliminate sampling variance
    Sji[i] <- rbeta(1, aSjit, bSjit)
    SjVar[i] <-rnorm(1, SjVarm, SjVars)
    # calculate alpha shape parameter for each iteration
    # aSjyr[i] = Sji[i]???((Sji[i]???(1-Sji[i])/SjVar[i])-1)
    aSjyr[i] = Sji[i]*((Sji[i]*(1-Sji[i])/SjVar[i])-1)
    # Calculate beta shape parameter for each iteration
    # bSjyr[i] = (1-Sji[i])???((Sji[i]???(1-Sji[i])/SjVar[i])-1)
    bSjyr[i] = (1-Sji[i])*((Sji[i]*(1-Sji[i])/SjVar[i])-1)
    # select fecundity value for each iteration from a log-normal distribution, replace
    # with 0.42 to eliminate sampling variance
    Fi[i] <- rlnorm(1,ilnFm, ilnFsd)
    Fsd[i] <- rnorm(1, Fsdm, Fsds)
    # Calculate the log-normal shape parameters for the annual selection of fecundity values
    lnFsd[i] = log((Fsd[i]^2)/(Fi[i]^2) + 1)
    # lnFm[i] = log(Fi[i])-1/2???lnFsd[i]
    lnFm[i] = log(Fi[i])-1/2*lnFsd[i]

    

    
    # system.time({ # LK add: turn on the timer for the annual loop
    for(k in 1:n_runs_per_it){# LK add: runs per iteration loop, in here calculate prob of persistence
      
      
    
      # initiate annual loop
      for(j in 1:(yrs))
      {
        # select adult survival value for each year in each iteration from a beta distribution
        Sa[i,j] <- rbeta(1, aSayr[i], bSayr[i])
        # select juvenile survival value for each year in each iterationfrom a beta distribution
        Sj[i,j] <- rbeta(1, aSjyr[i], bSjyr[i])
        #selects annual fecundity value from a log-normal distribution
        F[i,j] <- rlnorm(1,lnFm[i],lnFsd[i])
        # Demographic Stochasticity for fecundity, Poison distributed no. of female chicks produced per female
        yy[i,j-1]=sum(rpois(P[i,j-1],F[i,j-1]))
        #### Combining Temporal variance and Demographic Stochasticity in Pop.
        # dynamics. Binomially distributed survival of adults (P[i,j-1]) with
        # probability Sa[i,j-1] plus the binomially distributed survival of young of
        # the year (yy[i,j-1]) with probability Sj[i,j-1]
        if (j == 1) P[i,j] = 2300
        # else P[i,j] = (rbinom(1,P[i,j-1],Sa[i,j-1])) + (rbinom(1,yy[i,j-1],Sj[i,j-1]))
        if (j != 1){
          P[i,j] = (rbinom(1,P[i,j-1],Sa[i,j-1])) + (rbinom(1,yy[i,j-1],Sj[i,j-1]))
        }
        # Set density-dependent population ceiling
        if (P[i,j] >= 8000) F[i,j] = 0.0
        # count the number of replicates that go extinct
      if (P[i,j] < 1) e[i,j]=1
    } # Close the annual loop
    }  # LK add: close the runs per iteration loop
    # }) # LK add: turn off sys.time - annual loop
    
    # Orig code: Summarize simulation data and create plots of population trajectories
    # Calculate the proportion of simulations that went extinct
    se = apply(e,2,sum)
    # pe = se/it
    pe[i,] = se/it # LK add: just made this a vector instead of a scalar so we can track it across iterations
    pe
    
    # LK: commented out below since we don't need to calculate this info for our purpose; adapted below to use the same approach to graph prob of extinction
    # # calculate median population size
    # mP = apply(P,2,median)
    # mP
    # # Calculate the upper and lower 2.5 percentiles
    # lb = apply(P, 2, quantile, probs = c(0.025))
    # ub = apply(P,2,quantile, probs = c(0.975))
    # # Create plots of abundance
    # plot(mP,main = "", xlab = "years", ylab = "abundance", ylim=c(0,9000))
    # lines(mP)
    # lines(lb)
    # lines(ub)
    # # Create an output file to store simulation data.
    # data<-data.frame(mP,lb,ub,pe)
    # write.table(data,file="samp-var.csv",sep="","")
          
  } # Close the iteration loop
}) # turn off sys.time - iteration loop

# Doing just one run of the annual loop (i.e. within an iteration) took this much time:
# user  system elapsed 
# 0.06    0.00    0.06 
# So say we have 10,000 runs per iteration = 0.06*10000 = 600 seconds = 10 min

# In reality, the computer is finding some efficiencies as it was faster, 
# even with adding a bit of time to do the parameter draw at the iteration level:
# One iteration with 10000 runs per iteration took this much time on my work laptop:
# user  system elapsed 
# 129.33    0.05  129.41
# This is approximately 2.1 minutes.
# If we were to do this 10,000 times it would take 21,000 minutes = 350 hours = 14.6 days

# 10 iterations with 10000 runs per iteration took
# user  system elapsed 
# 1267.13    0.12 1267.43 
# 1267 seconds = 21 minutes

# For our real models, I have been using parallel computing with the iterations spread across cores to speed it up. 
# However, our models are more involved than this one and so take longer per iteration.
# Therefore I have also had to compromise on the number of run per iteration and 
# the number of iterations overall for some alternatives (not for those where we do sensitivity analyses).

  

# Orig code for abundance adapted here for probability of extinction
# calculate median probability of extinction 
mPe = apply(pe,2,median)
mPe
# Calculate the upper and lower 2.5 percentiles
lb = apply(pe, 2, quantile, probs = c(0.025))
ub = apply(pe,2,quantile, probs = c(0.975))
# Create plots of probability of extinction
plot(mPe,main = "", xlab = "years", ylab = "probability of extinction", ylim=c(0,1))
lines(mP)
lines(lb)
lines(ub)
# Create an output file to store simulation data.
data<-data.frame(mP,lb,ub,pe)
# write.table(data,file="samp-var.csv",sep="","")



  