## This function will save any inversion of a matrix
## so that the computation done need not be repeated
## when the same computation is inputted again.

## The main purpose of this function is to condition
## and allow the matrix to be a valid input for
## the second function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function is the function that a
## programmer will use to compute a matrix as well
## as cache the resulting output for future use.

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m    
}

## I advice my fellow student who will evaluate my work
## to first run the code I submitted above and then
## input this other code in order to test my work:
## (remove the double hashes please)
## mat <- matrix(sample(1:40, 9), nrow = 3, ncol = 3)
## matinput <- makeCacheMatrix(mat)
## cacheSolve(matinput)
## cacheSolve(matinput)

## The last command will show the stored answer