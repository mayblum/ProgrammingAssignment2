## The following two functions, when combined, enable to easily obtain the inverse of a given matrix
## using a cache mechanism. Since the inverse operation may be costly for large matrices,
## it is useful to compute it once for a given matrix and then store the result in a cache

## makeCacheMatrix creates a special "matrix" which is a list of functions for
## getting and setting the matrix data, as well as getting and setting the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # init the inverse results to NULL
    s <- NULL
    
    # set the data of the special "matrix"
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    # get the data of the special "matrix"
    get <- function() x
    
    # set a cached result for the inverse of the matrix
    setSolve <- function(inv) s <<- inv
    
    # set a cached result for the inverse of the matrix
    getSolve <- function() s
    
    # return a list object containing all 4 functions
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
    
}


## cacheSolve returns the inverse of a given matrix, first checking for a cached result
## to see if the inverse matrix was already computed earlier

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # check for a cached result
    s <- x$getSolve()
    
    # if result exists in cache - return it     
    if (!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    ## otherwise, compute inverse of x and save it
    # first - get the matrix
    data <- x$get()
    # then solve it
    s <- solve(data)
    # and set it
    x$setSolve(s)
    
    # return the result
    s
    
}
