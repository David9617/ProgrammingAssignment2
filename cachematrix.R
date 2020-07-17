## This functions allows us to cache the inverse of a given matrix


## This function gets a matrix and it returns a list of function to be provided to the next one

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  set_inv <- function(inverse) inv <<- inverse
  
  get_inv <- function() inv
  
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)   

}


## This function gets the last function applied to a given vector. In return it calculates the inverse matrix
## or gives the stored inverse matrix if it is already in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv  
}

# You can confirm the function works running the following:

m <- matrix(rnorm(25), nrow = 5, ncol = 5) # 1. create a matrix
matriz <- makeCacheMatrix(m) # 2. Apply the first function to that matrix

cacheSolve(matriz) # 3. Run the 2nd function on the previous variable to get the inverse

cacheSolve(matriz) # 4. Run it again to confirm that it's now stored in the cache

