makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  #method to set the matrix
  get <- function()x
  #meth to set the inverse of the matrix
  setsolve <- function (solve) m <<- solve
  getsolve <- function () m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


cacheSolve <- function(x = matrix()) {
  m <- x$getsolve()
  #return the inverse if it s already calculated
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  #get the matrix from the object
  data <- x$get()
  m <- solve(data, ...)
  #set the inverse to our object
  x$setsolve(m)
  m
}
