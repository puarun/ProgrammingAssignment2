## This is soln to assignment 2 by puarun
## makeCacehmatrix will make an opbject containing get, set, 
## get_inverse and set_inverse functions as a list
## call em using a $ sign

makeCacheMatrix <- function(x = matrix()) {
  xinv<-NULL
  set<-function(y){
    x<<-y
    xinv<<-NULL
  }
  get <- function(){return(x)}
  setinv <- function(xinv1){    
    xinv <<- xinv1    
  }
  getinv <- function(){
    return(xinv)  
  }  
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## cacheSolve will first check if you have already solved the inverse of the matrix
## if not it will solve it, if yes it will returned the cached value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinv<-x$getinv()
  if (!is.null(xinv)){
    return(xinv)
  }
  matdata<-x$get()
  xinv<-solve(matdata)
  x$setinv(xinv)
  return(xinv) 
}
