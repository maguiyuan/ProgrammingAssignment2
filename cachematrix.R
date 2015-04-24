## there are two fucntions: makeCachematrix() and cacheSolve(), chich can cache the inverse of a matrix.

##  This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL               ## set a flag to mark whether the inverse of this matrix has been computed.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x               ## store the original matrix in the special list
        setinverse <- function(solve) inv <<- solve          ## when the inverse hase been computed, this function can keep the result.
        getinverse <- function() inv      ## store the inverse and change the flag.
        list(set = set, get = get,        ## update the list
             setinverse = setinverse,
             getinverse = getinverse)
}
##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##   If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()              ## extract this element to check whether the inverse has been computed
        if(!is.null(inv)) {                     
                message("getting cached data")
                return(inv)             ## if the inverse has been computed, just retrieve the result from cache.
        }
        data <- x$get()                
        inv <- solve(data)               ## if not, get the matrix and inverse it
        x$setinverse(inv)                ## update result store in the cache
        inv
}
