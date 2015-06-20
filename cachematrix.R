## R programming assignment2

## The functions below cache and calculate the inverse of a matrix.

## makeCacheMatrix function creates special "matrix", which is a list containing function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
  get <- function ()x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function ()i
  list (
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)

}


## cacheSolve function computes the inverse of the special "matrix" 
## created by makeCacheMatrix function.
## if inverse of the matrix is calculated, and the matrix remains the same,
## cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message ("getting cache data")
    return (i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
