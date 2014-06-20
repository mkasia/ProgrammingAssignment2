## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inversion
## 4. get the value of the matrix inversion


makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y){
    x<<- y
    m<<- NULL
}
  get<- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m<- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<- x$get()
  m<- solve(data, ...)
  x$setinv(m)
  m
}
