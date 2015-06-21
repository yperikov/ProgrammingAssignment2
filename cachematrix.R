## Functions provide the ability to return inverse matrix of the argument matrix.
## In case when matrix has not changed and result is asked second time, fucntions return cached result.


## makeCacheMatrix returns closure, that consist of 4 functions to get, set matrix and get inverse result
## closure contains cached result of inverse matrix calculation - inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL  # variable used to store cached result
  set <- function(y) {  # functions to store new matrix
    x <<- y
    inverse <<- NULL
  }
  
  get <- function () x # function to return stored matrix
  setInverse <- function (i) inverse <<- i  #function to store inverse matrix in "inverse" variable
  getInverse <- function () inverse  #function to return stored result
  
  return ( list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)) # return list of functions

}


## use this function to calculate inverse matrix. 
cacheSolve <- function(x, ...) {
    
    i <- x$getInverse() # get result stored in closure
    if(!is.null(i)) {  # if cached result is present
        message("getting cached data")
        return(i)
    }
    data <- x$get()   # if result is not cached, get matrix, 
    i <- solve(data, ...)  # solve 
    x$setInverse(i)  #and store the result using setInverse
    return (i)  # return the result
  
}
