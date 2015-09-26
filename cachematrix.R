## The two functions are used to get the inverse of a square matrix, and cache 
## the results so it can be used in future. 


##makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        matrixInverse <- NULL # Initialization 
        
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setinverse<- function(x) {
                matrixInverse <<- solve(x)
        } # apply the inverse for x
        
        getinverse <- function() {
                matrixInverse
        }
        
        list( set = set,
              get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


##cacheSolve: This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been
##calculated (and the matrix has not changed), then the cachesolve should
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        matrixInverse <- x$getinverse()
        # The inverse of x is returned for verification
        if (!is.null(matrixInverse)) {
                print("getting cached data")
                
                matrixInverse
        } # Returns the inverse of x if calculated and cached before 
        
        else { 
                #print("This is else")
                x1 <- x$get()
                matrixInverse = solve(x1) 
                x$setinverse(matrixInverse)
                matrixInverse
                
        } # Calculate the inverse of x, cache the relult by setinverse
}
