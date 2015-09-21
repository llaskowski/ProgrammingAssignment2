## Put comments here that give an overall description of what your
## functions do
##
## Write a short comment describing this function
## makeCacheMatrix is a function returning a list containing a function to
## set the value of the Matrix
## get the value of the Matrix
## set the value of the inverse of the Matrix
## get the value of the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
                m<-NULL
                set <- function(y) {
                  x <<- y
                  m <<- NULL
                }
                get <- function() x
                setinv <- function(solve) m <<- solve
                getinv <- function() m
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
}


## Write a short comment describing this function
## This function calculates the inverse of a Matrix defined by the function
## makeCacheMatrix. If the Inverse has already been calculated it returns the cached results rather than doing the computation again

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
