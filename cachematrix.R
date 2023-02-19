makeCacheMatrix <- function(x = matrix) {
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) {
        invMat <<- inverse
    }
    getInv <- function() invMat
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

cacheSolve <- function(x, ...) {
    invMat <- x$getInv()
    if(!is.null(invMat)) {
        message("getting cached data")
        invMat
    }

    mat <- x$get()
    invMat <- solve(mat, ...)
    x$setInv(invMat)
    invMat
}
