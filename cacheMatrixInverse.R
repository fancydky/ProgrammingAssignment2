## Usage:           > x <- makeCachedMatrix( MatrixToBeInversed[n,n] )
##                  > solve <- cacheSolve( x )

## Sample Usage:    > x <- makeCachedMatrix(matrix(c(1,3,5,7),2,2))
##                  > solve <- cacheSolve( x )  # Return calculated solve
##                  > solve <- cacheSolve( x )  # Return cached solve


## makeCacheMatrix returns a list of 4 functions, set(),get() and setinverse()/getinverse(),
## set() sets the matrix and clear cache.
## get() returns the matrix
## setinverse() sets the resolved cache
## getinverse() returns cached solve value
makeCacheMatrix  <- function(x = matrix()){
        inv  <- NULL
        set  <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get  <- function() x
        
        setinverse <- function(solve) inv <<- solve
        
        getinverse <- function() inv
        
        list (mat=x, inv=inv, set=set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)       
}

## cacheSolve() checks whether the matrix x had been solved before,
## if not, it calculates the inverse and save the result to cache.
## if already solved, it simply returns the cached value.
cacheSolve  <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached inverse matrix")
                return (inv)
        }
        data  <- x$get()
        inv  <- solve(data,...)
        x$setinverse(inv)
        inv
}