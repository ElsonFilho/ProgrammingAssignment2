# Programming Assignment 2:  Lexical Scoping 
#
# Elson Felix Mendes Filho - pricotu@outlook.com
# 12.06.2015


#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric(), nrow, ncol) {
  i <- NULL
  set <- function(y, nrow, ncol) {
    x <<- matrix(y, nrow, ncol)
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.
# NOTE: For this assignment, assume that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}


#Usage Examples

# Example 1
M <- matrix(c(1,-0.25, -0.25, 1), nrow = 2, ncol = 2, byrow=TRUE)
M1 <- makeCacheMatrix(M)
M1$get()
cacheSolve(M1)


# Example 2
M1 <- makeCacheMatrix(matrix(c(1,0,4, 1,3,4, 4,1,0), nrow = 3, ncol = 3, byrow=TRUE))
M1$get()
cacheSolve(M1)
