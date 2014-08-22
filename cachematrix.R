## A cached class is defined to keep the inverse of a matrix if it is ever calculated
## The second or subsequent times it is required, the inverse matrix will not be calculated but the cached value will be used.
## testcacheSolve is written for testing. 

## inv will keep the inverse of the input matrix x 
## it will be null until setinv function is called
## getinv will return inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## x is oftype created by makeCacheMatrix
## instead of calculating the inverse of x, it checks whether it has been calculated before
## if it is, it returns the cached value
## if not, it calculates it and caches it for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

testcacheSolve <- function(n=3){
  if (n<2) n=2
  M <- matrix(runif(n*n),n,n)
  message("Original Matrix")
  print(M)
  Minv <- solve(M)
  message("Inverse Matrix")
  print(Minv)
  CM <- makeCacheMatrix(M)
  message("First Call")
  print(cacheSolve(CM))
  message("Second Call")
  cacheSolve(CM)
}
