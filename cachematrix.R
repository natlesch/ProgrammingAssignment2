## Calculate the inverse of a square matrix:
##  -Check if matrix inverse already exists and retrieve from cache if exists
##  -Otherwise calculate inverse and cache the result for future reference

## Function to get and set cached matrix inverse
## - Initialize with a (square) matrix
makeCacheMatrix <- function(x = matrix()) {
  ##Initialization
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }  
  get <- function() x
  
  ##Caching
  setMatrixInverse <- function(inverse) m <<- inverse
  getMatrixInverse <- function() m
  
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## Calculate inverse of square matrix.
##  - Utilize instance of makeCacheMatrix for getting/setting cached inverse value
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse
  m <- x$getMatrixInverse()
  if(!is.null(m)) {
    message("getting cached matrix inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrixInverse(m)
  m
}