#'This module contains two functions which aid in caching the inverse of a matrix.
#' \code makeCacheMatrix returns an object which can hold a cache.
#' \code cachesolve is used to compute or retrieve the inverse.

#' Create an object that contains a matrix and possibly a cached value of the inverse
#' of the matrix.
#' 
#' @param x the matrix to be contained and inverted
#' @return a list containg the given matrix with functions to get, set the matrix and its inverse
#' @seealso \code cacheSolve which actually computes the inverse and caches it
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<-inv
    getinverse <- function() inverse
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


#' Compute or retrieve the inverse of a matrix
#' 
#' @param x an object created by \code makeCacheMatrix
#' @return the inverse of x
#' Assumes that x is invertible, otherwise will cause an error. The matrix
#' inverse is retrieved from x if a cached value is available, otherwise
#' the inverse is computed and cached. 
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i

}
