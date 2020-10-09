## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function receives an array and creates a list which 
#can contain the value of its inverse if it has already been calculated.

makeCacheMatrix <- function(x = matrix()){
  Inverse <- NULL
  set <- function(y){
    x <<- y #This can reassign the value of x, even outside of the function
    Inverse <<- NULL
  }
  get <- function(){x} #Call the matrix
  setInverse <- function(InverseCalculated){
    Inverse <<- InverseCalculated #Assign the inverse 
  }
  getInverse <- function(){Inverse} #Call the inverse if it was already calculated
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #Firts part
  #If the inverse is already stored just call it 
  Inverse <- x$getInverse()
  if (!is.null(Inverse)){ #If is not null...
    message("getting cached data")
    return(Inverse)
  }
  #Second part
  #If the Inverse was null then we calculate it 
  
  data <- x$get() 
  Inverse <- solve(data,...) #Calculates the inverse
  x$setInverse(Inverse) #Stores it
  Inverse   ## Return a matrix that is the inverse of 'x'

}

#Example
#Firts we creates the list of the information about x's
x <- makeCacheMatrix(matrix(rnorm(16), ncol = 4))

#Second We verify that you can indeed call the matrix or its inverse
x$get()

x$getInverse()

#Third We calculate its inverse

cacheSolve(x)

#Again we check with the firts function
x$getInverse()

#Done
