## Our aim in this experiment is to write a pair of functions, namely, "makeCacheMatrix" and "cacheSolve" that caches the inverse of a matrix
## makeCacheMatrix is a function which creates a special matrix object that can cache its inverse for the input. 
makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL                                            ## Initialize inv as NULL; will hold value of matrix inv                          
  set <- function(y) {                    
    x <<- y                                              ## Value of matrix in parent environment                         
    inv <<- NULL                        
  }
  get <- function() x  ## Returns value of the matrix argument                   
  
  setinverse <- function(inverse) inv <<- inverse        ## Assigns value of inverse in parent environment
  getinverse <- function() inv                           ## Gets the value of inv when called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}

## cacheSolve is a function which compute the inverse of the special matrix returned by makeCacheMatrix above. 
## If the inverse is already been calculated and the matrix has not changed, then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


