################################################################################
## Below is functions that allows to compute the inverse of a matrix
## and keep the result in cache in case of a new computation.
##
## This is very useful to save time while using inverse function many
## time for the same function
##
## Author : 6RiLM
## Date   : 10/06/2016
##
################################################################################


#####
## Create a kind of Matrix object allowing to store inverse result in cache
##
## params :
##   -  in : a matrix
##   - out : a list of function allowing to get/set the value of the matrix
##           and get/set the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function () i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


#####
## Compute the inverse of a matrix at the first call of the function and store
## the result in cache to avoid a new calculation at the following call of the 
## function.
##
## params :
##   -  in : a special matrix (represented by the outcome of makeCacheMatrix)
##   - out : the inverse of the matrix
##
## NB: line 51 is used for debugging, it could be removed

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setInverse(i)
    i
}

