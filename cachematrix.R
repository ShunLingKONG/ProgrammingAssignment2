## ****makeCacheMatrix: This function creates a special"matrix" that can cache its inverse****
## **<<-operator can be used to assign a value to an object in an environment that is different from the current environment**
##**The first function,make Vector creates a special "vector", which is really a list containing a funtion to:1.set the value of the vector;2.get the value of the vector;3.set the value of the inverse;4.get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


##***** CacheSolve computes the inverse of the special "matrix". If the inverse has already been calculated and the matrix has not changed, then the cachesolve should retrieve the inverse from the cache
##****Assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
