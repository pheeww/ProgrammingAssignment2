## Put comments here that give an overall description of what your
## functions do

## Makes a list of functions

makeCacheMatrix <- function(x=matrix()){
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get=get, setinverse=setinverse,getinverse=getinverse)
}


##Checks if the inverse of the matice exists in cache and accordingly re-calculates 

cacheSolve<-function(x,...){
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse<-solve(data,...)
  x$setinverse(inverse)
  inverse
}
