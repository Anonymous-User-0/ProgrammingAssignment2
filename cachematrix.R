## Put comments here that give an overall description of what your
## functions do

##Every call to the function makeCacheMatrix returns a list that can be##
##used to compute and cache the inverse of a matrix ##


makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(mean) m <<- mean
        getInverse <- function() m
        list(set=set,get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function returns the inverse of a matrix stored in x$get(). If a matrix Y is provided as a second argument, then
## the function determines whether it differs from x$get(). If so, it changes the matriz stored in x$get() and calculates it's inverse. If not (...) argument is provided it first checks whether the inverse of x$get() is cached.
## When no (...) argument is provided it checks whether the inverse of x$get() has been computed and cached. If not, it computes
## the inverse and caches it for future use.

cacheSolve <- function(x, ...) {
        ## The (...) argument, if provided, is used to change the old matrix by a new one
	## without the need to call makeCachematrix again.                 
	  dots <-list(...)
	## The next line changes the matrix stored in output of makeCacheMatrix for the matrix in the argument (...)
        ## in case it is provided. Nothing happens, if provided matrix is the same as the originally provided.
        if (length(dots)==1 && !identical(dots[[1]],x$get())) {x$set(dots[[1]])}
	  m <- x$getInverse()
        if(!is.null(m)) {
                message("Getting Inverse of unchanged matrix:")
                return(m)
        }
	  message("Original matrix has changed. Computing inverse of new matrix:")
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m

}
