## Functions that create a special matrix and cache its inverse

## Creates special matrix object

makeCacheMatrix <- function(x = matrix()) {
  ##Initialize inverse property
  i<-NULL
  
  ## Set the matirix
  set<-function(matrix){
    m<<-matrix
    i<<-NULL
  }
  ## Get the matrix
  get<-function(){
    m
  }
  ## Set inverse of matrix
  setInverse<-function(inverse){
    i<<-inverse
  }
  ## Get inverse of matrix
  getInverse<-function(){
    i
  }
  ## Return list of methods
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Compute inverse of special matrix from previous function, "makeCacheMatrix",
##If inverse was calculated, then the initial matix will be the final output.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()

  ## Return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get matrix from object
  data <- x$get()
  
  ## Calculate inverse
  m <- solve(data) %*% data
  
  ## Set inverse 
  x$setInverse(m)
  
  ## Return matrix
  m
}