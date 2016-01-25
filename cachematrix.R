## The first function, `makeCacheMatrix` creates a special "CacheMatrix", 
## which is really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix 
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function calculates the inverse of the special "CacheMatrix"
## created with the above function. However, it first checks to see if the
## inverse of the matrix has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get() 
        
        ## if X is a square invertible matrix, then solve(X) returns its inverse.
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## Below is an example test
## m1 <- matrix(data = c(1,3,3,4,2,7,6,2,4), nrow = 3, ncol = 3)
## m2 <- makeCacheMatrix(m1)
## cacheSolve(m2)
