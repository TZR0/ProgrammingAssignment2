## makeCacheMatrix is a functiong that creates other functions and then returns them (and two objects) as a list to parent environment.
## The set function assigs x to the parent environment and makes sure to assigns NULL to any previously cached inverse values. 
## The get function retrieves the value of x from the parent environment. Setinverse calculates and assigns the inverse matrix
## to the variable "inverse" in the parent environment. The getinverse returns the variable inverse.
## cacheSolve completes the makeCacheMatrix. It is used to populate and/or reatrive values form makeCacheMatrix. It first checks to see
## if there is a non NULL inverse, if it is, it calculates one and assigns it "inverse". 

## makeCacheMatrix is a function that creates a matrix, calculates it inverse matrix and then caches both in memory
## to avoid repeatedly calculating notoriously expensive calculations.

makeCacheMatrix <- function(x = matrix()){
  inverse<-NULL
  set<-function(c){
    x<<-c
    inverse<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) inverse<<-solve(x)
  getinverse<-function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve uses as an argumento thaat is returned by makeCacheMatrix. It retrieves the chached inverse matrix fom
## the makeCacheMatrix's environment. 

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()
  if(!is.null(inverse)){
    message("Getting cached data!")
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(data,...)
  x$setinverse(inverse)
        ## Return a matrix that is the inverse of 'x'
}
