## Functions for wrapping a matrix with inverse caching functionality as specified by
## programming assignment 2 of the R Programming Coursera course.
##
## makeCacheMatrix  wraps a regular matrix with cache goodness
## cacheSolve       wrapper for solve() that leverages caching functionality
##
## Note: Google commenting style followed (function comments below first line).
## See http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml. 

makeCacheMatrix <- function(x = matrix()) {
    # Wraps a matrix with capabilities to cache its inverse.
    #
    # Args:
    #   x: An matrix (assumed to be invertible). Default is the empty matrix.
    #
    # Returns:
    #   A list with accessors for the matrix as follows. 
    #
    #   v.set(x)            sets the underlying matrix (clears the cache)
    #   v.get()             retrieves the underlying matrix
    #   v.setinverse(xp)    sets the inverse cache
    #   v.getinverse()      gets the the cached inverse or NULL if not set

    # inv will be the variable for storing the cached inverse
    inv <- NULL
    set <- function(omx) {
        # the matrix has changed, wipe out the inverse as well
        x <<- omx
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

cacheSolve <- function(x, ...) {
    # Computers or fetches the cached inverse of x. 
    #
    # Args:
    #   x:   A wrapped matrix with caching functionality created with 
    #        createCacheMatrix.
    #   ...: Extra arguments for solve. Note that these arguments will not get recheceked
    #        when accessing the cache. In other words two calls with different "..." args
    #        will get you the same matrix. This is consistent with the assignment vector
    #        example.
    #
    # Returns:
    #   The inverse of x's underlying matrix. 
    inv <- x$getinverse()
    if (!is.null(inv)) {
        # like the vector example, show a message when we get from cache
        # inv already points to the cached inverse
        message('getting cached inverse')
    } else {
        # compute inv and cache it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
    }
    inv
}

