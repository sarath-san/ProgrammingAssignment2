makeCacheMatrix <- function(x = matrix()) {
  #initialise the inverse matrix with null
  invMatrix<-NULL
  
  #function to set the value of data
  set <- function(y){
    x<<-y
    invMatrix<-NULL
  }
  
  #function to get the value of data
  get <-function() x                             
  
  #function to set value of inverse matrix
  setInv<-function(inverse) invMatrix<<-inverse
 
  #function to get the value of inverse matrix
  getInv<-function() invMatrix
  
  #return the list containing all the functions
  list(set=set,get=get,setInv=setInv,getInv=getInv)
  
}


cacheSolve <- function(x, ...) {
  
  #get the inverse matrix in the cache memory
  invMat<-x$getInv()
  
  #Check if it is a null
  if(!is.null(invMat)){
   
    #if it is not a null return the cached inverse
    message("getting cached data")
    return(invMat)
  
  }
  
  #if it a null calculate inverse and cache and return the result
  data<-x$get()
  
  #calculating inverse
  invMat<-ginv(data)
  
  x$setInv(invMat)
  invMat
}
