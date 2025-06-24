## Put comments here that give an overall description of what your
## functions do

## set a matrix with inverse as NULL. also set inv from cachesolve

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv 
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## this function first check whether the det of the returned matrix is 0 or not.Then check whether there is already an inverse calculated.
#if not return the inverse and assign new inverse to the setinv()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if (det(x$get())== 0) print("det is zero")
  if (!is.null(x$getinv())){
    message("getting cached data")
    return(x$getinv())
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  inverse
}
