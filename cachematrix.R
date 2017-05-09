## This function define set, get, getinv, setinv

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
        set <- function(y){
          x <<- y
          inv <<- NULL 
        }
        get <- function() x
        getinv <- function() inv
        setinv <- function(inverse) inv <<- inverse
        list(set = set, get = get, getinv = getinv, setinv = setinv)
        }


#This function will see if inverse of the matrix is already stored in cache otherwise it'll
# and store.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting data from cache")
    inv
  }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
