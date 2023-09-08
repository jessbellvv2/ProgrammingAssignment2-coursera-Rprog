## library(MASS) is used to calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL         #initializing inverse as NULL
  set <- function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function()x          #function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getsolve<-function(){
                      inver<-ginv(x)
                      inver%*%x
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## this is used to get the cache data

cacheSolve <- function(x, ...) ##gets chache data
  {
  inv<-x$getinv()
  if(!is.null(inv)) {     ##checking whether inverse is null
    message("getting cached data!")
    return(inv)
  }         #returns inverse value
  data<-x$get()
  inv<-solve(data, ...)         #calclates inverse value
  x$setinv(inv)
  inv  ##return a matrix that is the inverse of 'x'
}
