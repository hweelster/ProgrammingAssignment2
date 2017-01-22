## The first function makeCacheMatrix  creates an special invertible "matrix" x, which is really a list to:
##1. set the matrix
##2. get the matrix
##3. set the inverse of matrix
##4. get the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  inv <- NULL
  set<- function(y) {
    x<<-y
    inv <<-NULL
  }
  
get<-function()x
setinv <-function(inverse)inv <<-inverse
getinv<-function()inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getinv()
        
        # if the inverse has already been calculated
        if(!is.null(inv)) {  
        message("getting cached data")
        return(inv)
        }
        ## otherwise, inverse is calculated        
        data <- x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        return(inv)
        }
         
  
