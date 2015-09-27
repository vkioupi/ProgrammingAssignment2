## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL ## initialize inverse
  
  set <- function(userValue = matrix()) {
    x <<- userValue 
    cachedInv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(invVal) {
    cachedInv <<- invVal 
    return(cachedInv)
  }
  
  getInverse  <- function() cachedInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { 
  
  
  calculatedInverse <- x$getInverse() 
  
  
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
    message("getting cached data")
    return(calculatedInverse)
  }
  
  
  matrixToSolve <- x$get()  
  
  calculatedInverse <- tryCatch({ 
    solve(matrixToSolve)
  }, warning=function(w) {
    message("This may not be the result you're looking for")
    message(w)
  }, error=function(e) {
    message("Something went wrong solving your matrix")
    message(e)
    message("\n")
  })
  
  
  message("Setting the value of inverse to:") 
  x$setInverse(calculatedInverse)
}