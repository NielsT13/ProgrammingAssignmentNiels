## Create 'special' matrix that caches matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(solve) inv <<- solve
  getinverse = function () inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## compute the inverse of a matrix or retrieve chached inverse if it is already calculated

cacheSolve <- function(x, ...) {
  inv = x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data = x$get()
  inv = solve(data)
  x$setinverse(inv)
  inv
}
