# Caching the Inverse of a Matrix
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  makeCacheMatrix <- function(x = matrix()) { # define the argument with default mode of "matrix"
    inver <- NULL                             # initialize inver as NULL; will hold value of matrix inverse 
    set <- function(y) {                    # define the set function to assign new 
    x <<- y                                 # value of matrix in parent environment
    inver <<- NULL                          # if there is a new matrix, reset inver to NULL
      }
        get <- function() x                 # define the get fucntion - returns value of the matrix argument
        
          setinverse <- function(inverse) inver <<- inverse  # assigns value of inver in parent environment
          getinverse <- function() inver                     # gets the value of inver where called
          list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## in order to refer 
  
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve retrieve the inverse from the cache.
  
cacheSolve <- function(x, ...) {  # return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
      if(!is.null(inver)) {           # if the inverse has already been calculated, get the inverse from the cache  
           message("getting cached data")
           return(inver)
        }
      data <- x$get()
      inver <- solve(data, ...)  # compute the inverse of a square matrix
      x$setinverse(inver)        # set the value of the inverse in the cache using the setinverse function
      inver
}
}