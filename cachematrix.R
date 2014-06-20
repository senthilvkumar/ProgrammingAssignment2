## This file contains two functions that are used to store a matrix
## and cache's its inverse.

## Sample Run
## v <- makeCacheMatrix()
## v$set(matrix(runif(16),4,4))
## v$get()
## cacheSolve(v)
## cacheSolve(v)

## The first function makeCacheMatrix creates a vector of four functions:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)}


## The second function calculates the inverse of the matrix using the above 
## function. It checks if the inverse is already computed. If so, it gets
## inverse from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the inverse in the cache via the setinv
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}


