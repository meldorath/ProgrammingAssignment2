## This function stores the values of a matrix within the function
## including the mean and inverse of a matrix that is input.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) { ## Stores cached values for the matrix
      x <<- y 
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## This function solves for the inverse of the matrix that is input
## but rather than calculating the inverse from scratch the function
## uses the cached values to solve for the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    matrix <- x$get() ## Solves for the inverse of the matrix and returns the values
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
