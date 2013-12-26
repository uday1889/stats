is.prime <- function(n) n == 2L || n ==3L || all(n %% 2L:floor(sqrt(n)) != 0)
getPrimeSets <- function(n) {
  perms <- permutations(n,n,1:n)
  perms.sum <- (perms + cbind(matrix(0,nrow=length(perms[,1])), perms[,1:n-1]))[,2:n]
  perms[which(apply(apply(perms.sum,1:2,function(x) is.prime(x)), 1, function(x) all(x == TRUE))),]
}


library(gregmisc)

pt <- NULL
results <- NULL
i <- 0
is.prime <- function(n) n == 2L || n ==3L || all(n %% 2L:floor(sqrt(n)) != 0)

getPrimeTuples <- function(n) {
  perms2 <- permutations(n,2,repeats.allowed=F)
  perms2.sum <- apply(perms2,1,sum)
  perms2[which(apply(matrix(perms2.sum),1,function(x) is.prime(x))),]
}

ptm <- proc.time()

getChain <- function(startChain) {
  #cat("\ncalling getChain for...", startChain)
  #Wow...Jackpot!!
  if (length(startChain) == n) {
    i <<- i+1
    
    cat(paste("\n", i, ")",sep=""), startChain, "<-", (proc.time()-ptm)[3])
#     ptm <<- proc.time()
#     if (all(is.na(results))) {
#       results[1,] <<- startChain
#     } else {
#       results <<- rbind(results,startChain)
#     }
#     return(results)
  }
  nextLinks <- findNextLinks(startChain[length(startChain)])  
  #Filter away existing links
  #cat("\nstartChain .... ", startChain)
  nextLinks <- nextLinks[!(nextLinks %in% startChain)]
  #cat("\nnext links .... ", nextLinks)
  if (length(nextLinks) == 0) {
    #We've hit a wall..no point pursuing further
    return(NULL)
  } else {
    for (i in 1:length(nextLinks)) {
      newChain <- append(startChain,nextLinks[i])
      #cat("\nnew chain after append ", newChain)
      getChain(newChain)
    }
  }
  return(startChain)  
}

findNextLinks <- function(lookupNum) {
  return(pt[which(sapply(pt[,1], function(x) x==lookupNum)),2])
}

getPrimeSetsOpt <- function(n) {
  ptm <<- proc.time()
  
  i <<- 0
  n <<- n
  pt <<- getPrimeTuples(n)
  results <<- matrix(NA, ncol=n)
  for (i in 1:length(pt[,1])) {
    startChain <- pt[i,]
    getChain(startChain)
  }  
  rownames(results) <- paste(c(1:nrow(results)), 
                             ")", sep="")
#   cat("\nResults:\n")
#   print(results)
# 
#   proc.time() - ptm
}




