## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    #initialize with null inverse
    inverse <- NULL
    
    #make a new matrix...
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    #return our matrix...
    get <- function() x
    
    #assign or return our inverse
    set_Inverse <- function(inv) inverse <<- inv
    get_Inverse <- function() inverse
    
    list(set = set, get = get,
         set_Inverse = set_Inverse,
         get_Inverse = get_Inverse)

}


## Computes the inverse of the makeCacheMatrix matrix. 
## If the inverse has already been calculated, then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    #get our inverse from the matrix object
    inv <- x$get_Inverse()
    #if we already calculated (inv is not null), just return the inverse
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #else we need to get the matrix...
    data <- x$get()
    #calculate the inverse...
    inv <- solve(data)
    x$set_Inverse(inv)
    #and return
    inv
}
