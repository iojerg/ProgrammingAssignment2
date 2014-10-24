## The function makeCacheMatrix creates a special "Matrix"
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the Matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## The next function checks if the inverse of the matrix has already been calculated
## If yes, it returns the value of the inverse computed previously
## If not, it calculates the inverse of the matrix through the function solve() and saves it

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

## See an example how does it work
m<-makeCacheMatrix(matrix(rnorm(9),3,3))
## variable m contains a matrix, defined as matrix(rnorm(9),3,3)
m$get()
## m$get() shows the already created matrix m

## call cacheSolve to compute the inverse. First it checks whether it has already been calculated
cacheSolve(m)
## it is the first time that we ask for the inverse of this matrix, so it will compute the inverse with the function solve()

## if we call again the function cacheSolve(m), it will check if the inverse of the matrix has already benn calculated
cacheSolve(m)
## as we asked for the inverse of this matrix before, it will take the vale from the cache
## you should be able to read on the scren "getting cached data"
