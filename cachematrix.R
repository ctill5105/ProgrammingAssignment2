## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #store the inverse version of the matrix  
  inv <- NULL  
  
  #set for matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
    
  #get for matrix
  get <- function() x 
  
  
  #set for inverse of matrix
  setinv <- function(inverse) inv <<- inverse
  
  #get for inverse of matrix
  getinv <- function() inv
  
  
  #return the matrix    
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## compute the inverse of the matrix if it's not already calculated otherwise return the already calculated inverse matrix
  
  inv <- x$getinv()
  
  # If the inverse is already calculated then just return the already calculated inverse
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not calculated then calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return it
  inv
        
}
