## this function create 'matrix' that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
  set <- function(y) {
    x<<-y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver<<- inverse
  getinverse <- function() inver
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## it return inverse matrix of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver
}
