## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #empty vector for the inverse 
    set <- function(y) {
        x <<- matrix
        inv <<- NULL
    } #method to set the matrix
    get <- function() x #method to get the matrix
    setInverse <- function(inverse) inv <<- inverse #method to set the inverse property
    getInverse <- function() inv #method to et the inverse property
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse) #return a list of the methods
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse() #return a matrix that is the inverse of x
    if(!is.null(inv)) { #if the inverse is already calculated, return it from the cache
        message("getting cached data")
        return(inv)
    }
    data <- x$get() #input of the matrix
    inv <- solve(data, ) #solve for the inverse matrix, with x as input and b as the identity matrix
    inv #return the inverse matrix
}

