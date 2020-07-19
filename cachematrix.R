## Put comments here that give an overall description of what your
## functions do
## it is found that there are 2 functions makecachematrix and cashesolve
## makecashematrix have set,get,setinv,getinv
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL               #initialising inverse as NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}       # this helps in getting the matrix x
  setinverse<-function(solve_Matrix) {inv<<-solve_Matrix}      
  getinverse<-function(){inv}           # this helps in getting the inverse of the matrix
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## this helps in getting the cache data
cacheSolve <- function(x, ...) {    #helps in getting the cache data
  inv<-x$getinverse()
  if(!is.null(inv)){             #checks whether the inverse is null
    message("getting cashed data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)         #calculate inverse of value
  x$setinverse(inv)
  inv              ## Return a matrix that is the inverse of 'x'
}
