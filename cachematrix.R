## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # Overwrite the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # Return the matrix
    get <- function() x
    # Set the inverse
    setInv <- function(Inv) m <<- Inv
    # Return the inverse
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    # Check if the inverse has already been calculated
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # If the inverse has not yet been calculated, use the solve(a)
    # function to find the inverse
    data <- x$get()
    m <- solve(data)
    x$setInv(m)
    # Return the inverse
    m
}
