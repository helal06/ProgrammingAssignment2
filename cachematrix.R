

## Put comments here that give an overall description of what your
## functions that cache the inverse of a matrix


## This function creates a special kind of matrix object that can cache its inverse

makeCacheMatrix <- function(x=matrix()){
      ## Initializing the inverse property
      invrs <- NULL
      
      ## Method to set the matrix
      set <- function(y){
            x <<- y
            invrs <<- NULL
      }
      
      ## Method to get the matrix
      get <- function(){
            ## returning matrix
            x
      }
      
      ## Method to set the inverse of the matrix
      setInverse <- function(solve) {
            ## storing inverse 
            invrs <<- solve
      }
      
      ## Method to get the inverse of the matrix
      getInverse <- function() {
            ## returns the inverse
            invrs
      }
      
      ## Returns the list of methods
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by the "makeCacheMatrix" function. 
## - If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## getting a matrix that is the inverse of 'x'
      invrs <- x$getInverse()
      
      ## returns if the inverse has already been calculated (i.e. if !is.null(m)==TRUE)
      if(!is.null(invrs)) {
            message("getting cached data")
            return(invrs)
      }
      
      ################################################
      ## If the inverse wasn't yet been calculated ##
      ################################################
      
      ## getting the matrix from our object
      data <- x$get()
      
      ## calculating the inverse by using matrix multiplication
      invrs <- solve(data) 
      
      ## storing the inverse to the object to future usage
      x$setInverse(invrs)
      
      ## returning a matrix that is the inverse of 'x'
      invrs
}

