

## I have written two functions to cache inverse of a matrix
## The first function makeCacheMatrix creates a special type of matrix ( which can be cached)
## It takes a matrix as an argument and returns a list objects which has 4 functions 
## look at the last line where list is returns with 4 objects ( all are functions )



makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y      
    inv <<- NULL
  }
  
  get <- function() x  
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function cacheSolve first checks if we already have inverse of matrix in cache
## if inv variable is not Null i.e. cache is found it returns the data from cache ( value of inv)
## if inv is null it calculates the inverse using solve function and stores it in cache by calling 
## setinverse function ( look at the call, its calling the list elements by name (using $ sign) and 
## that element happens to be a function ). setinverse stores output in inv and returns the value to console.
## next time if cacheSolve is called without changing the matrix, inv wont be NULL so again data
## gets returned from cache, if we change the matrix we need to convert it to a format that can be
## cached, so we need to call makeCacheMatrix which again sets inv to NULL so now when you call cacheSolve 
## on this new matrix, it will skip the if clause because inv is NULL and it will have to calculate inverse 
## using solve method which will then get stored in inv using setinverse method

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
