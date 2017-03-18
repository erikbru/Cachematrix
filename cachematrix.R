##Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly (there are also 
## alternatives to matrix inversion that we will not discuss here).
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

##Write the following functions:
  
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

## My solution is:

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## Default x is an empty matrix. The inverse (i) is initialized to NULL (as an object)
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
    ##assigns input parameter to x, and clears any cached values of i
    ##x en i are stored in the environment created by makeCacheMatrix
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  ## i is updated (from NULL to inverse), when calling x$setinverse(i) in cachesolve.
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ## storing functions in a named list which can be called using x$name().
  ##This is the object that cacheSolve works with.
  }

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  ## retrieving the inverse from the environment created by calling makeCacheMatrix
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
    ## doing nothing if the inverse is not NULL
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  ## calculating the inverse with the solve function if the inverse was NULL, and storing it
}

##Testing the code with suggested matrices in the discussion forum

m1 <- makeCacheMatrix(matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2))
m1$get()
m1$getinverse()
cacheSolve(m1)
## This proves that this first time, there was no inverse stored that could be retrieved.
## If I now run cachesolve again, you will see that result is accompanied by the "getting cached data" message,
## and hence retrieved instead of calculated
cacheSolve(m1)

## In order to show that the inverse is correct, I am doing a true matrix multiplication.
## The result is indeed the 2x2 identity matrix
m1$get() %*% m1$getinverse()

##I also want to check if the other one mentioned works out. 
n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
m2 <- makeCacheMatrix(n2)
m2$get()
m2$getinverse()
cacheSolve(m2)
## This proves that this first time, there was no inverse stored that could be retrieved.
## If I now run cachesolve again, you will see that result is accompanied by the "getting cached data" message,
## and hence retrieved instead of calculated
cacheSolve(m2)

## In order to show that the inverse is correct, I am doing a true matrix multiplication.
## The result is indeed the 2x2 identity matrix.
##(although I don't understand where the (extremely small) rounding error comes from ;-))
n2 %*% m2$getinverse()