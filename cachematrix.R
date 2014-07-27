## The function creates a matrix object that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(i) {
    m <<- i
  }
  getinv <- function() {
    m
  }

  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)  
}

## cacheSolve either computes the inverse of the matrix created by makeCacheMatrix
#  or retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()  
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinv(m)
  m
}
