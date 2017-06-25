# Caching the Inverse of a Matrix
# This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  makeCacheMatrix <- function(x = matrix()) { # define the argument considering the default mode of matrix
    inver <- NULL                             # initialize inver variable which stores value of matrix inverse 
    set <- function(y) {                    # define the set function which assigns new value of matrix 
    x <<- y                                 
    inver <<- NULL                          # resent inver to NULL in case of having a new matrix
      }
        get <- function() x                 # define the get fucntion to return value of the matrix
        
          setinverse <- function(inverse) inver <<- inverse  # assigns value of inver
          getinverse <- function() inver                     # gets the value of inver
          list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  
}


# This function computes the inverse of the special matrix returned by makeCacheMatrix. 
# If the inverse has already been calculated and the matrix has not changed, 
# then the cachesolve retrieve the inverse from the cache.
  
cacheSolve <- function(x, ...) {  # return the inverse of matrix
  inver <- x$getinverse()
      if(!is.null(inver)) {           # get the inverse from cache if the inverse has already been calculated  
           message("getting cached data")
           return(inver)
        }
      data <- x$get()
      inver <- solve(data, ...)  # compute the inverse
      x$setinverse(inver)        # set the value of the inverse in the cache
      inver
}
}