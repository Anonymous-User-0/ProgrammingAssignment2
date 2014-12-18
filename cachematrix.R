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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## The (...), if provided, argument is used to change the old matrix by a new one##
	## without the need to call makeCachematrix again.                  ##
	  dots <-list(...)
	## The next line changes the matrix stored in output of makeCacheMatrix for the matrix in ##
        ## the argument (...) in case it is provided. Nothing happens, if provided matrix is the same as ##
        ## the one the first time makeCacheMatrix is called. ##
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
