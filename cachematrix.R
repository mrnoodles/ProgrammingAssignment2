## These are functions for creating and manipulating
## "matrices" that are capable of caching their inverses



## Returns a list of functions for manipulating original, 
## which is now a matrix capable of chaching its inverse

makeCacheMatrix <- function(original = matrix()) {
  inv <- NULL
  
  
  #sets new_value to be the new value of the matrix
  set <- function(new_value) {
    original <<- new_value
    inv <- NULL
  }
  
  #returns the value of the matrix
  get <- function() { 
    return(original)
  }
  
  #sets new_inv to be the new inverse of the matrix
  setinverse <- function(new_inv) {
    inv <- new_inv
  }
  
  #returns the inverse of the matrix
  getinverse <- function() {
    return(inv)
  }
  
  return(list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  ))
}



## Updates the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## Checking to see if the inverse if cached
  if(!is.null(inv)) {
    return(inv) ##If it is cached then we are done and return
  }
  
  ## If the test resulted in negative then we calculate the inverse again
  
  ## Fetching the data
  original <- x$get()
  
  
  inv <- solve(x, ...) ## Calculating the inverse
  x$setinverse(inv) ## Updating the cache
  
  return(inv) ## We are done so we return
}
