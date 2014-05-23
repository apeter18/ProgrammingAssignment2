## These two functions will compute and cache the inverse of a matrix, which 
## eliminates the need to repeatedly compute the inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=numeric()){
    m <- NULL # creates the cache
    set <- function(y){ 
        x <<- y # stores matrix y as a variable named x
        m <<- NULL # clears cache
    }
    get <- function() (x) # gets the matrix stored in x
    setinv <- function(solve) (m <<- solve) # caches the matrix
    getinv <- function() (m)  # returns the cached matrix
    list(set = set, get = get, setinv = setinv, getinv = getinv)
        # list of factors in x
}

## This function computes the inverse of the  special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv() # checks to see if inverse has already been calculated
    if(!is.null(m)){ 
        message("getting cached data")
        return(m) # returns the inverse if previously cached
    }
    data <- x$get()
    m <- solve(data,...) # solves for the inverse of the matrix
    x$setinv(m) # caches the inverse
    m # Return a matrix that is the inverse of 'x'
}