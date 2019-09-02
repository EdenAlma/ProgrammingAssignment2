## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function mimics a constructor from a OOP perspective holding/tracking 2 variables (iMatrix and x)
##and 4 functions which are used to access and set the two variable (functions are returned in list)
makeCacheMatrix <- function(x = matrix()) {
  
  iMatrix <- NULL     #inverse matrix var
  get <- function() x 
  set <- function(y) {x <<- y} 
  #basically sets the input matrix by refrencing using <<- operator
  
  getInv <- function() iMatrix #get iMatrix
  setInv <- function(inpt) iMatrix <<- inpt #set '''' by refrencing upper scope iMatrix
  
  z<-list(get = get, set = set, setInv = setInv, getInv = getInv) #create list to be returned
  z

}


## Write a short comment describing this function
##This functiom will be used to access/create the "cacheMatrix" objects inverse matrix vaiable
##x is a cacheMatrix list/object and this function will use the four functions to access and set
##the iMatrix variable 

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  iM <- x$getInv() ##get inverted matrix value 
   
  if (is.null(iM)){ ##if not set >> calculate and cash
    
    m <-  x$get() ##get matrix
    i <- solve(m, ...) ##calculate inverse matrix
    x$setInv(i) ##cache the calculated inverse matrix
    
  }
  
  r <- x$getInv() ##get cached matrix
  r  ##return inverse matrix
  
  
}


