## R programming Exercise 2: Cache an inverse matrix


## makeCacheMatrix creates a 'list' from a matrix to get and set a matrix 
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Check matrix is square and therefore might be invertible
  if (nrow(x) != ncol(x)) {
    warning("Must be square matrix")
  }
  # Define 4 functions for list
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  # Create list of functoins to be returned
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cache solve will calculate the inverse of a matrix if it has not already 
## been calculated

cacheSolve <- function(x, ...) {
  # Retrieve the value in the cache      
  inv <- x$getinv()
  # If the value reuturned is not null, the inverse has been previously calculated
  # so we just need to return it. Print helpful message to say we have found it.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # The value returned is null, so we need to calculate the inverse of x
  data <- x$get()
  inv <- solve(data, ...)
  # Save it for the next time cacheSolve is called
  x$setinv(inv)
  # Return the inverse
  inv
  
}
