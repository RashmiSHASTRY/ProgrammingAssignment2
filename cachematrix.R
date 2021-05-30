## Put comments here that give an overall description of what your
## functions do

##Funnction creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
  set <- function(y) {                           #set the value of the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x                            #get the value of the matrix
  setinverse <- function(inverse) i <<- inverse  #set inverse of the matrix
  getinverse <- function() i                     #get the inverse matrix
  list(set = set,                                #Returning the list 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)


}    


##Function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {     #function  that returns a matrix that is the inverse of 'x'
        
         i <- x$getinverse()         #get the inverse
  if (!is.null(i)) {                 #checking if inverse of the matrix has been calculated already 
    message("getting cached data")   #Displying the message for getting catched Data
    return(i)                        # return the inverse of the matrix
  }
  data <- x$get()
  i <- solve(data, ...)              #If the inverse already calculated(if inverse of the matrix is not changed
  x$setinverse(i)                    #Getting value of the matrix and inversing the matrix
  i                                  # Returning a matrix that is the inverse of 'x'
}
