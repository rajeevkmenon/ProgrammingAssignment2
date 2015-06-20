## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        retinverse <- NULL
        set <- function(y) {
          x <<- y
          retinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) retinverse <<- inverse
        getinverse <- function() retinverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # check if a previous calculation exists in cache..
        invVal <- x$getinverse()
        if(!is.null(invVal)) {
          
          # cache exists.. return the existing invrse result
          message("no recalculation needed. returning cached values.")
          return(invVal)
        }
        
        message("first time calculation. no cache exists..")
        # getting the input matrix..
        matrixVal <- x$get()
        
        # computing the inverse
        invVal <- solve(matrixVal)
        
        #caching the inverse calculation result..
        x$setinverse(invVal)
        
        # returning result
        invVal  
}
