## Creates the special matrix object
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL #inverse matrix
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inverseMatrix) im <<- inverseMatrix
    getInverseMatrix <- function() im
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Caches the inverse of a matrix
cacheSolve <- function(x, ...) {
    im <- x$getInverseMatrix()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setInverseMatrix(im)
    im
}

#m<-makeCacheMatrix(matrix(c(2,5,7,10), 2))
#print(cacheSolve(m))
#print(cacheSolve(m))
