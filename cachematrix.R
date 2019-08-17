## makeCacheMatrix - Stores the matrix and cache its inverse.   
##

## 
## Usage example:

## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(2, 0, 0, 2),nrow = 2,ncol = 2))
## > cacheSolve(m)
## [,1] [,2]
## [1,] 0.5 0.0
## [2,] 0.0 0.5

## Create a special "matrix", which is a list containing
## a funtion to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
  set <- function(y) {
   x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##  cacheSolve-computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)###Main Logic of Inverse
  x$setinverse(i)
  i
}
