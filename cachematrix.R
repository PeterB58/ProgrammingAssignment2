## Put comments here that give an overall description of what your
## functions do
## The functions calculation of an inverse matrix, so that the result is cached
## and then re-used if a call to calculate the inverse is repeated

## Write a short comment describing this function
## makeCacheMatrix function initiates an object that stores the initial matrix and 
## some functions ("methods") that will be used to process that matrix
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL  # initial value in case inverse was not yet calculated
   set <- function(y) {
      # this "method" is used to update the matrix (x)
      x <<- y  # need <<- to make assignment inside the main function
      inv <<- NULL
   }
   get <- function() x  # this "method" is used to return the matrix
   setInverse <- function(sol) inv <<- sol # used "sol" to show that this is just a variable 
   getInverse <- function() inv  # this "method" is used to return the inverse
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve function computes, caches, and returns the inverse of the original matrix 
cacheSolve <- function(obj, ...) {
   ## Note that I re-named the function argument to obj (object defined by makeCacheMatrix)
   ## so that it is not confused (by humans, not computers) with x used in makeCacheMatrix 
   ## Return a matrix that is the inverse of 'x' (entry matrix into makeCacheMatrix)
   inv <- obj$getInverse()  # get cached inverse
   if(!is.null(inv)) {
      # can simply use old (cached) inverse
      message("getting cached data")
      return(inv)
   }
   # do the following if the cached inverse is NULL (that is, not previously calculated):
   data <- obj$get() # get the matrix
   inv <- solve(data, ...) # find inverse; could also solve equations if ... used. 
   obj$setInverse(inv)  # inv object in the envirnment created originally by makeCacheMatrix 
                        # will be the inverse matrix
   inv
}
