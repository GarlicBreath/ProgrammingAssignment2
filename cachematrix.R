## A function to cache the inverse of a matrix and recall the cached inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  setinverse<-function(inverse) {inv<<-inverse}
  getinverse<-function() {inv}
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Calling previously cached inverse of a matrix

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
