
##makeCacheMatrix function
##@param x : invertible matrix
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL  #create an empty matrix
  
  
  set <- function(y) { # set x
    x <<- y
    m <<- NULL
  }
  
  get <- function()  # get x
    x 
  
  #set inverse
  setinverse <- function(value) m <<- value  
  

  #get inversed matrix
  getinverse <- function() 
    m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##makeCacheMatrix function
##Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {        
  
  m <- x$getinverse()  #calling getinverse()
  

  if(!is.null(m)) {  #check if the returned matrix is NULL
    message("getting cached data")  #show message
    return(m)  #return the matrix that's not NULL
  }
  
  ##the codes below will be executed when m is NULL
  data <- x$get()   #get input matrix
  m <- solve(data, ...)  #inverse
  x$setinverse(m)  #set the inverse
  m #display and return the inverse
}
