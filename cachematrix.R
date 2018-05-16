## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# MakeCacheMatrix takes a matrix argument 
# passing the argument and the inverse variable into global space
# it creates several accessor variables to set and get the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv2 <- NULL
  set <- function(y) {
    x <<- y
    inv2 <<- NULL
  }
  get <- function()
    x
  setinverse <- function(inverse)
    inv2 <<- inverse
  getinverse <- function()
    inv2
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}



## Write a short comment describing this function
# cacheSolve takes the variables defined in makeCacheMatrix
# if the inverse is already defined by makeCacheMatrix it will
# take that variable from cache or solve the inverse, save the result to cache 
# and display the result. 

cacheSolve <- function(x, ...) {
  inv2 <- x$getinverse()
  if (!is.null(inv2)) {
    message("getting cached data.")
    return(inv2)
  }
  data <- x$get()
  inv2 <- solve(data)
  x$setinverse(inv2, ...)
  inv2
}
