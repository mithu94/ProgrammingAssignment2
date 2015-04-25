## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. Following pair of functions 
## are written to cache inverse of a matrix and retrive the cahced inverse when appropriate 



## makeCacheMatrix function have 4 functions within this. Those functions are:  

## 1. set : Assign original matrix to a floating variable outside current environment for caching
## 2. get : Retrive original matrix that is in cache 
## 3. setinv: set inverse of the matrix in cache 
## 4. getinv: Retrive inverted matrix from cache


makeCacheMatrix <- function(x = matrix()) {
  
  InvMtx = NULL ## InvMtx initialized with a defaul value of null
  
  set = function(y) {
    ## this function used to assign matrix value to floating variable outside current env.
    x <<- y 
    ## <<- used to assign value to x. 
    ## x is a floting variable outside current environment
    ## x is used to hold original matrix 
    
    InvMtx <<- NULL ## initialize invert matrix variable to NULL 
  }
  
  get = function() x
  
  setinv = function(invt) InvMtx <<- invt 
  
  getinv = function() InvMtx
  
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## cacheSolve function do following actions:
## Check if the inverse is available in the cache
## if the original matrix is changed
## if not changed then return the cached inverse
## if not chahed or original matrix changed then it will calculate inverse and store the inverse and return it.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  
  invt = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(invt)){
    
    ## get cached matrix for change verification
    mtx=x$get() 
    
    ## calcmtx is matrix which needs to be inverted. calcmtx is defined in calling environment  
    if (dim(calcmtx) == dim(mtx) && all(calcmtx == mtx)){
      
      # alrady in the cache and matrix not changed so retrun cache value 
      message("getting cached data")
      return(invt)
    } 
      
    }
  
  
  # otherwise, calculates the inverse 
    
  inv = solve(calcmtx) ## calculate inverse

  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)

  return(inv)
  
  }


## To test the functions:
## First execute the functions in R
## then execute follwing command to create a matrix
## calcmtx<-matrix(1:4,2,2)
## then execute:
## x<-makeCacheMatrix(calcmtx)
## then execute
## cacheSolve(x) this will show the inverse of the matrix and cache it.
## if you again execute the cacheSolve(x), you will see the text "getting cached data"
## and then inverse of the matrix. 