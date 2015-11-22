# makeCacheMatrix: return a list of functions to:
# Set the value of the matrix
# Get the value of the matrix
# Set the value of the inverse
# Get the value of the inverse

# creating the function
makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    # getting the matrix 
    get <- function() x
    
    # setting the inverse
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
   
     # return the matrix with our newly defined functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}

# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  #cache the inverse
  x$setInverse(inv)
  
  # return the result
  inv
}
