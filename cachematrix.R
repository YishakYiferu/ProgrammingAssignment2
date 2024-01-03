# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   
  set <- function(y) {
    x <<- y       
    inv <<- NULL  
  }
  get <- function() x   # Matrix from cache matrix object
  set.inv <- function(inverse) inv <<- inverse  
  get.inv <- function() inv   
  list(set = set, get = get,     
       set.inv = set.inv,
       get.inv = get.inv)
}

# Function to compute the inverse of the special "matrix" and use caching
cacheinverse <- function(x, ...) {
  inv <- x$get.inv()    
  if(!is.null(inv)) {
    message("get cached data")
    return(inv)
  }
  matrix_to_inverse <- x$get()   
  inv <- solve(matrix_to_inverse, ...)
  x$set.inv(inv)
  inv                         
}
