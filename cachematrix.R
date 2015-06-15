## Author: Jeremy Lerner
## These functions make and save a matrix 
## and its inverse in a list

## Create a list that holds a matrix and its inverse
## input:
##          x: an invertible matrix
## output:
##          a list that contains, on which the 
##          following functions can be run:
##          get(): returns the matrix
##          set(x2): sets the matrix in the list to be x2
##          getInv(): returns the inverse of the matrix
##          setInv(num): records the inverse of the matrix to be num
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # Overwrite the matrix, setting it to y
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # Return the matrix
    get <- function() x
    # Set the inverse to Inv
    setInv <- function(Inv) m <<- Inv
    # Return the inverse
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## If the cache matrix has an inverse already, 
## return it, otherwise calculate it, then return it
## input:
##          x: the matrix to find the inverse of
## output:
##          m: the inverse of matrix x
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
