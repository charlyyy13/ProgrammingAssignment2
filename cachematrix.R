#Set and get the value of the vector 
#Matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <- y
    m <<- NULL
  }
  get <- function() x 
  setinverse <- function(solveMatrix) m <<- solveMatrix
  getinverse <- function() m
  list (set=set,get=get,
        setinverse=setinverse,
        getinverse=getinverse)
  
}

##Function computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse 
  if(!is.null(m))  {
    message( "getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
