## The following two functions cache the inverse of a matrix


## A special matrix object to cache its inverse:
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Inversion is initialised
  i <- NULL
  
  ## The matrix is set
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## The matrix is extrated
  get <- function()
    
  {
    ## The matrix is returned
    m
  }
  
  ## The inverse of the matrix is set
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## A function to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  
  }
  
  ## List of methods is returned
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The inverse of the special matrix returned by "makeCacheMatrix"
## above is computed. If the inverse has already been computed (and the matrix has not
## changed), the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## The inverse of matrix of 'x' is returned
  m <- x$getInverse()
  
  ## If already set, the result is returned
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Matrix is retreived from the object
  data <- x$get()
  
  ## Using matrix multiplication, the inverse is computed
  m <- solve(data) %*% data
  
  ## The inverse is set to the object
  x$setInverse(m)
  
  ## The matrix is returned
  m
}