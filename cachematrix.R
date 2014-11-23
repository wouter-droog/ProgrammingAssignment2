## These functions take a matrix and calculate and return the inverse matrix
## With every call the inverse matrix is cached so that subsequent calls with same
## input will return cached inverse matrix instead of calculating it again

## makeCacheMatrix function creates an object from an input matrix and store/fetch 
## the inverse by use of the defined methods. The inverse is initialized to be "NULL"

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) #list of methods
}


## checks if input matrix's inverse is already stored in the object and retrieves it from 
## cache by calling makeCacheMatrix"s getinverse method. If not then the inverse will be 
## calculated and added to the object by calling the makeCacheMatrix methods.

cacheSolve <- function(x, ...) {        
    inv <- x$getinverse() 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
