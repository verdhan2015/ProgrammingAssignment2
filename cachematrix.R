## This is Assigment 2 for R programming where we will be calculating inverse of matrix from 
## cache if required

## First make the makeCacheMatrix function which takes matrix as an input parameter

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL #initializing the value with NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve #solve gives the inverse
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Make the cacheSolve function which takes matrix as an input parameter, it will first check
##if the value is present in cache and if not will calculate it.if the value is present it will return that

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)) #if the vale of m is not null
  {
    message("getting cached data")
    return(m) #returm m if the vale of m is not null else we need to calcualte it
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
