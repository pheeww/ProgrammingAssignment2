## Both the functions combined are capable of judging wether an inverse of a matrix has already been computed or not and accordignly decide wether to calculate or not


## Makes a list of functions and returns it

makeCacheMatrix <- function(x=matrix()){
  #inverse holds the inverse of the matrix 'x'
  inverse <- NULL
  #this function is used to set a new value to the matrix 'x'
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  #this function is used for the retrival of the matrix 'x'
  get <- function() x
  #this function is for the giving the value to inverse of x
  setinverse <- function(inv) inverse <<- inv
  #this function is for retrival of the value of the inverse of 'x'
  getinverse <- function() inverse
  list(set = set, get=get, setinverse=setinverse,getinverse=getinverse)
}


##Checks if the inverse of the matice exists in cache and accordingly re-calculates 
cacheSolve<-function(x,...){
  inverse <- x$getinverse()
  #chekcs wether an inverse of x has already been calculated or not
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  #if not already cached then proceeds to calculate it
  data <- x$get()
  inverse<-solve(data,...)
  x$setinverse(inverse)
  inverse
}
