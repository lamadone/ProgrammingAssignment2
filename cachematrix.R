##Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than computing it repeatedly
##These functions are created to compute the inverse of a square invertible matrix and 
##cache it so if we need it again, it is not recomputed but taken from the cache


## This function creates a special "matrix" object that can cache 
## its inverse
##it takes as argument a matrix
## it stores four function (get, set, setInvMatr,getInvMatr)
##The get function simply return the matrix stored in the main function (makeCacheMatrix)
##The set function change the matrix that have been stored in the main function
## by the one in input
##the setInvMatr function store the input inverse matrix in the main function and 
##the getInvMatr function return this matrix 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInvMatr <- function(inverse) inv <<- inverse
    getInvMatr <- function() inv
    list(set = set, get = get,
         setInvMatr = setInvMatr ,
         getInvMatr = getInvMatr )
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
## First, this function checks if the inverse, stored with `
## getInvMatr exists and is not NULL. If it is the case, it returns 
## a message and the inverse. Otherwise, it computes the inverse
## of the matrix, stores the inverse with the setInvMatr  and 
## returns his value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInvMatr()
    if(!is.null(inv)) {
      message("getting cached inverse matrix")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInvMatr (inv)
    inv
  }
