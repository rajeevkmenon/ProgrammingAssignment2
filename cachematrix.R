## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates and stores a list with functions for
# 1. set the the matrix
# 2. get the the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

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
# This function returns the matrix inverse. First, it checks if
# the inverse has already been calculated. If yes, it returns the already calculated 
# result instead of re-calculating. If no, it calculates the inverse and stores the result
# in the cache using setinverse function.

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
