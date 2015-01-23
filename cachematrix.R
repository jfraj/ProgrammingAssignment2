## Put comments here that give an overall description of what your
## functions do


## This function creates a "matrix" which is really a list of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) xinv <<- solve
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" 
## created with the function makeCacheMatrix
## It first checks to see if the inverse has already been calculated.
## If so, it gets the mean from the cache and skips computation
## Otherwise, it calculates the inverse of the matrix and sets it
## to the cache via the setinv function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xinv<-x$getinv()
    if(!is.null(xinv)){
        message("getting cached inverse matrix")
        return(xinv)
    }
    message("Calculating the inverse matrix")
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
}
