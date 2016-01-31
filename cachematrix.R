## The two following functions, used together, compute the
## inverse of an invertible matrix and store the result in
## chace, providing it without a new computation if asked for
## again. When the cached result is displayed, a message is
## print to advice the user.


## function makeCacheMatrix: given a matrix, return a list of
## four functions, which respectively
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse (NULL if not yet computed)
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
          x <<- y
          inv <<- NULL }
    get <- function() {
          x }
    setinv <- function(inverse) {
          inv <<- inverse }
    getinv <- function() {
          inv }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## function cacheSolve: control if the value of the inverse
## is already stored in cache. If so, returns its value (printing)
## a message) and exit, otherwise compute it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
