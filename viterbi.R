##### Viterbi in R
# JHendry
# 2015/10/20

# Now I am editing this file inside of GitHub.

### Conceptually:
# Imagine a system with X states which are unobservable.
# There is an "initial" probablity to be in a given state.
# There is also a probability to transition between any two given unoberservable states.
# The system evolves through time, moving invisibly from state to state.
# As the system evolves, you can observe Y "outputs".
# Each system state has a probability profile for the Y "outputs".
# Now, imagine you observed a sequence of outputs, Y1, Y2, ... YN.
# Could you infer how the system was transition between the states X?
# Yes, you can calculate the most probable path the system took through the states.
# You need to know:
# (1) The initial probabilites of each states X. 
#		= a vector: probIn
# (2) The probabilities of transitioning between any two states.
#		= a matrix: probTrans
# (3) The probabilities of observing a given output given a certain system state.
#		= a matrix: probEmit

# Now, to compute to most probable path:
# V1, K = probEmit(Y1 | k)*probIn(k) # i.e., probability of starting at K and observing Y1, here is the root, after becomes hazy
# Vt, K = max[x%in%Xstates](probEmit(Yt | K)*Vt-1,X*probTrans(X, K)) # i.e., probIn becomes prob of previous state times transition, over all possible previous states



# Practice Data:
probIn <- c(0.1, 0.2, 0.1, 0.4, 0.15, 0.05)
names(probIn) <- letters[1:length(probIn)]
obsOut <- sample(LETTERS, 6)

# Build Transition Probabilities
probTrans <- matrix(0, nrow = length(probIn), ncol = length(probIn), dimnames = list("From" = names(probIn), "To" = names(probIn)))

# Build Emission Probabilities
probEmit <- matrix(0, nrow = length(probIn), ncol = length(obsOut), dimnames = list("From" = names(probIn), "To" = obsOut))
for (i in 1:nrow(probEmit)) {	
	probs <- runif(5)
		while (sum(probs) > 1) { probs <- runif(5) }
		probs[6] <- 1 - sum(probs)
		probEmit[i, ] <- probs
		}




viterbi <- function(probIn, probTrans, probEmit, obsOut) {
	VmatProbs <- matrix(NA, nrow = length(obsOut), ncol = length(probIn), dimnames = list("Trial" = 1:length(obsOut), "hState" = names(probIn)))
	VmatPaths <- VmatProbs
	VmatProbs[1, ] <- probIn*probEmit[ , obsOut[1]]
	VmatPaths[1, ] <- names(probIn)
	for(t in 2:nrow(VmatProbs)) {
		for(k in 1:ncol(VmatProbs)) {
				probToEnd <- VmatProbs[t - 1, ]*probTrans[ , k]*probEmit[k, obsOut[t]]
				VmatProbs[t, k] <- max(probToEnd)
				VmatPaths[t, k] <- names(probIn)[which.max(probToEnd)]
		}
	}
	finalState <- which.max(VmatProbs[nrow(VmatProbs), ])
	Vresult <- list(VmatProbs, VmatPaths)
	return(Vresult)
}


### A function that sums two previous numbers in vector to produce next
sumSeries <- function(n) {	
		sumVal <- rep(NA, n)
		for (i in 1:n) {
			if (i < 3) {
				sumVal[i] <- 1
			} else {
				sumVal[i] <- sumVal[i - 1] + sumVal[i - 2]
			}
		} 
		return(sumVal[n])
}


### Generating Transition Probability Matrices


for (i in 1:nrow(probEmit)) {	
	probs <- runif(5)
		while (sum(probs) > 1) { probs <- runif(5) }
		probs[6] <- 1 - sum(probs)
		probEmit[i, ] <- probs
		}
		

for (k in 1:ncol(probTrans)) {
	for (i in 2:nrow(probTrans)) {	
		probs <- runif(5)
			while (sum(probs) > 1) { probs <- runif(5) }
		probs[6] <- 1 - sum(probs)
		probTrans[i, ] <- probs
		}
	while(sum(probTrans[ , k]) > 1) {
		for (i in 2:nrow(probTrans)) {	
		probs <- runif(5)
			while (sum(probs) > 1) { probs <- runif(5) }
		probs[6] <- 1 - sum(probs)
		probTrans[i, ] <- probs
		}
	}
	probTrans[1, k] <- 1 - sum(probTrans[ , k])
}

# One Dimension Matrix Population Constraining Sum
fixSumVec <- rep(0, 5)
for (i in 1:length(fixSumVec)) {
	fixSumVec[i] <- runif(1, 0, 1 - sum(fixSumVec))
}
fixSumVec[1] <- 1 - sum(fixSumVec[2:length(fixSumVec)])

# Two Dimension
	

