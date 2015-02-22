## My approach:
## Steps:
## 1. Test if input matrix is in the cache
## 2. If so, return cache
## 3. If not, invert, return and add to cache

## Cache management. Will create a list for the cache functions..

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## creates list of the four new functions. 
}


## First checks the cache, and retrieves result if it exists, otherwise calculates 

cacheSolve <- function(x, ...) {
        
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) { ## testing the cache
                message("getting cached data")
                return(m)
        }
        originalmatrix <- x$get() ## if no cachce, retrieves the original matrix
        m <- solve(originalmatrix, ...) ## inverts matrix
        x$setinverse(m) ## adds inverted matrix to cache
        m
}
