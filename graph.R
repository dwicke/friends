set.seed(12345)
require(igraph) 
require(MASS) # to write matrix
require(kknn) # for spectral clustering
setwd("~/Documents/R/pattern rec hw/project")
par(ask=TRUE)				#wait for permission before showing new graph
clusterThem <- function(mydata, lMethod, nneigh) {
	adjMatrix <- matrix(ncol=length(mydata[,1]), nrow=length(mydata[,1]))
	print(length(mydata))
	
	getElOnes <- function(x, indOns) append(indOns, which(x %in% c(1)))
	denseData <- list()
	denseData  <- apply(mydata, 1, getElOnes, denseData)
	
	
	
	for (i in 1:length(mydata[,1])) { ## for each person
		
		#curPer <- which(mydata[i,] %in% c(1))
		for (j in c(1:length(mydata[,1]))[-i]) {
			## must look at every other person besides myself
			#weight <- sum((c(mydata[i,] & mydata[j,])== TRUE)) 
			#sim <- which(c(mydata[i,]==mydata[j,]),arr.ind=TRUE)
			#meAndYou <- which(curPer %in% which(mydata[j,] %in% c(1)))
			#weight <- length(meAndYou)
			#sim <- curPer[meAndYou]
			common <- which(denseData[[i]] %in% denseData[[j]])
			
			
			
			weight <- length(common)
			
			if( weight > 0 ) {
				## then there exists some common values between i and j
				adjMatrix[i,j] <- weight
			}else {
				adjMatrix[i,j] <- 0
			}
		}
		adjMatrix[i,i] <- 0
		adjMatrix[i,i] <- sum(adjMatrix[i,])
		if(adjMatrix[i,i] == 0) {
			adjMatrix[i,i] <- 1
		}
		
		#print(i)
	}
	
	#write.matrix(adjMatrix, "friendNet.matrix")
	
	friendNet <- graph.adjacency(adjMatrix,mode=c("undirected"),weighted=TRUE)
	

	
	# read in the circles
	x <- scan("0.circles", what="", sep="\n")
	y <- strsplit(x, "[[:space:]]+")
	names(y) <- sapply(y, `[[`,1)
	y <- lapply(y, `[`, -1)
	y <- lapply(y, as.numeric)
	
	#print(clusters(friendNet))
	#plot(friendNet)
	
	
	#sc <- specClust(adjMatrix, method="random-walk", nn=25)
	
	
	succRates <- c()
	
	if(TRUE) {
		## five clusters did well
		for (k in 2:30) {
			sc <- specClust(adjMatrix, centers=k, method= lMethod, nn=nneigh) ## try nn=12
			#sc <- kmeans(adjMatrix, centers=k, iter.max=100)
			
			print(k)
			#print(sc$betweenss / sc$totss) ## want this to be big
			maxSumDifs <- 0
			for(i in 1:k) {
				clus <- which(sc$cluster %in% c(i), arr.ind=TRUE)
				lapply(y, function(x) length(which(x %in% clus)))
				#print(i)
		
				#print(unlist(lapply(y, function(x) length(which(x %in% clus))), use.names=FALSE))
				print("k")
				print(k)
				print("ith ")
				print(i)
				dalist <- unlist(lapply(y, function(x) length(which(x %in% clus))/max(c(length(clus), length(x)))),use.names=FALSE)
				whichInd <- which(dalist %in% max(dalist), arr.ind=FALSE)
				print(whichInd)
				print(max(unlist(lapply(y, function(x) length(which(x %in% clus))/max(c(length(clus), length(x)))),use.names=FALSE)))
				maxSumDifs <- maxSumDifs + max(unlist(lapply(y, function(x) length(which(x %in% clus))/max(c(length(clus), length(x)))),use.names=FALSE))
				
			}
			succRates[k-1] <- maxSumDifs / k
			#print(maxSumDifs/ k)
		}
	}
	return(succRates)

}


clusterem <- function(mydata, lMethod, nneigh, adjMatrix) {

	

	#friendNet <- graph.adjacency(adjMatrix,mode=c("undirected"),weighted=TRUE)

		
		
		
	# read in the circles
	x <- scan("0.circles", what="", sep="\n")
	y <- strsplit(x, "[[:space:]]+")
	names(y) <- sapply(y, `[[`,1)
	y <- lapply(y, `[`, -1)
	y <- lapply(y, as.numeric)
	
	#print(clusters(friendNet))
	#plot(friendNet)
	
	
	#sc <- specClust(adjMatrix, method="random-walk", nn=25)
	
	
	succRates <- c()
	
	if(TRUE) {
		## five clusters did well
		for (k in 2:30) {
			sc <- specClust(adjMatrix, centers=k, method= lMethod, nn=nneigh,gmax=4000) ## try nn=12
			print(k)
			#print(sc$betweenss / sc$totss) ## want this to be big
			maxSumDifs <- 0
			for(i in 1:k) {
				clus <- which(sc$cluster %in% c(i), arr.ind=TRUE)
				lapply(y, function(x) length(which(x %in% clus)))
				#print(i)
		
				#print(unlist(lapply(y, function(x) length(which(x %in% clus))), use.names=FALSE))
				
				maxSumDifs <- maxSumDifs + max(unlist(lapply(y, function(x) length(which(x %in% clus))/max(c(length(clus), length(x)))),use.names=FALSE))
				
			}
			succRates[k-1] <- maxSumDifs / k
			#print(maxSumDifs/ k)
		}
	}
	return(succRates)
}



MAGThem2 <- function(mydata, feats, affMatrix) {
	
	## The idea is that I want to to take the whole data matrix and
	## add each row if length()
	featID <- unlist(feats, use.names=FALSE)
	adjMatrix <- matrix(ncol=length(mydata[,1]), nrow=length(mydata[,1]))
	for (i in 1:length(mydata[,1])) { ## for each person
		
			adjMatrix[i,] <- apply(mydata,1,function(x) length(which(c(mydata[i,featID] + x[featID]) %in% 2)) / length(featID))	
		print(i)
	}
	
	return(adjMatrix)
}



MAGThem <- function(mydata, feats, affMatrix) {


	adjMatrix <- matrix(ncol=length(mydata[,1]), nrow=length(mydata[,1]))
	conMatrix <- matrix(ncol=length(mydata[,1]), nrow=length(mydata[,1]))
	for (i in 1:length(mydata[,1])) { ## for each person
		
		curPer <- mydata[i,]
		for (j in c(1:length(mydata[,1]))[-i]) {

			otherPer <- mydata[j,]
			probConnected <- 1.0
			## loop over each of the features and 
			
			for (k in 1:length(names(feats)))
			{
				#print(mydata[i,k])
				
				# so now i need to get the 
				theLis <- unlist(c(mydata[i,feats[[names(feats)[k]]]]+mydata[j,feats[[names(feats)[k]]]]))
				thelisSum <- sum(theLis)
				
				if(length(which(theLis %in% 2)) > 0)
				{
					# This means that they both have a one in the same column
					# for this feature!
					#probConnected <- probConnected * affMatrix[2,2]
					probConnected <- probConnected + 1
				}
				else if(thelisSum > 0)
				{
					# then that means that one of them had a one but the other didn't
					## affMatrix[1,0] == affMatrix[0,1] so all good
					#probConnected <- probConnected * affMatrix[2,1]
					
				}
				else
				{
					# neither had a one so all zeros!
					#probConnected <- probConnected * affMatrix[1,1]
					probConnected <- probConnected + .25
				}
				
				
			}
			
			probConnected <- probConnected * (1 / length(names(feats)))
			adjMatrix[i,j] <- probConnected
			#print(probConnected)
			if(TRUE)
			{
				## Now check to see if they are connected
				if(runif(1) < probConnected)
				{
					conMatrix[i,j] = 1
					#print("connected")
				}else
				{
					conMatrix[i,j] = 0
					
				}
			}
		}
		
		adjMatrix[i,i] <- 0
		adjMatrix[i,i] <- sum(adjMatrix[i,])
		if(adjMatrix[i,i] == 0) {
			adjMatrix[i,i] <- 1
		}
		#adjMatrix[i,] <- adjMatrix[i,] / sum(adjMatrix[i,])
		#print(adjMatrix[i,])
	}
	
	friendNet <- graph.adjacency(conMatrix,mode=c("undirected"),weighted=TRUE)
	write.graph(friendNet, "conGraph.txt", format="edgelist")
	
	
	return(adjMatrix)
	
	
	

}



mydata <- read.csv("data.csv")
mydata <- mydata[-1] # first col is id
## thinking about getting rid of column 125-127 
## since they are local which would be sort of useless...
##mydata <- mydata[c(-1*126:140, -1*202:224, -1*161:176, -1*91:104, -1*54:79, -1*1:8 )]


## now go ahead and cluster
d <- scan("0.featnames", what="", sep="\n")
sp <- strsplit(d, "[[:space:]]+")
ad <- sapply(sp, `[[`,2)
names(sp) <- ad
sp <- lapply(sp, `[`, -2)

b <- unique(ad)
feats <- apply(as.matrix(b), 1, function(x) which(ad %in% x))

names(feats) <- b
#if(F){
## remove these since they seem like they would just be noise
feats$`work;with;id;anonymized` <- NULL
feats$`education;with;id;anonymized` <- NULL
feats$"work;start_date;anonymized" <- NULL
feats$"first_name;anonymized" <- NULL
feats$"work;end_date;anonymized" <- NULL
feats$"work;location;id;anonymized" <- NULL
feats$"locale;anonymized" <- NULL
feats$"gender;anonymized" <- NULL
feats$"education;type;anonymized" <- NULL
feats$"education;degree;id;anonymized"  <- NULL
feats$"education;classes;id;anonymized" <- NULL
feats$"birthday;anonymized" <- NULL
#}

mydata <- mydata[,unlist(feats,use.names=FALSE)]

if(TRUE) {
succRes <- clusterThem(mydata, "symmetric", 12)
srMatrix <- matrix(nrow=length(succRes), ncol=5)
srMatrix[,1] <- c(2:30)
srMatrix[,2] <- succRes
succRes <- clusterThem(mydata, "symmetric", 25)
srMatrix[,3] <- succRes
succRes <- clusterThem(mydata, "random-walk", 12)
srMatrix[,4] <- succRes
succRes <- clusterThem(mydata, "random-walk", 25)
srMatrix[,5] <- succRes
}
if(FALSE) {
	
	afinMatrix <- read.csv("afinMatrix.csv")
	rmatr <- matrix(runif(length(probM[1,])*length(probM[,1]),min=.3), nrow=length(probM[1,]), ncol=length(probM[1,]))
	afinMatrix[afinMatrix > rmatr] <- 1
	afinMatrix[afinMatrix != 1] <- 0
	
#afinMatrix <- MAGThem(mydata, feats, matrix(ncol=2,nrow=2, data=c(c(.2,.5), c(.5,.9))))
#write.matrix(afinMatrix, file="afinMatrix.csv", sep=",")

	succRes <- clusterem(mydata, "symmetric", 12, afinMatrix)
	srMatrix <- matrix(nrow=length(succRes), ncol=5)
	srMatrix[,1] <- c(2:30)
	srMatrix[,2] <- succRes
	
	succRes <- clusterem(mydata, "symmetric", 25, afinMatrix)
	srMatrix[,3] <- succRes
	succRes <- clusterem(mydata, "random-walk", 12, afinMatrix)
	srMatrix[,4] <- succRes
	succRes <- clusterem(mydata, "random-walk", 25, afinMatrix)
	srMatrix[,5] <- succRes
}


if(T)
write.matrix(srMatrix, file="succrate.csv", sep=",")

#plot(srMatrix[,1], srMatrix[,2])

# doesn't work yet... try looking into matlines instead...
if (FALSE) {
xrange <- range(c(2:30))
yrange <- range(srMatrix[,-1])
plot(xrange, yrange, type="n", xlab="k (num clusters)", ylab="Success Rate" ) 
colors <- rainbow(5) 
linetype <- c(2:5) 
plotchar <- seq(18,18+4,1)


for (i in 2:5) { 
  
  lines(srMatrix[,1], srMatrix[,i], type="b", lwd=1.5,
    lty=linetype[i], col=colors[i], pch=plotchar[i]) 
} 
}
#for(i in 1:length(feats)) {
	
#	clusterThem(mydata[unlist(feats[i], use.names=FALSE)])
	
	
#}
