## These functions allow the user to cache the inverse of a matrix.  This allows any dependent algorithms
## to recall the inverse of the matrix efficiently without needing to recalculate it repeatedly


##makeCacheMatrix takes a matrix object as an input and then creates a List Object with 4 attributes that 
##allow you to store a matrix x, retrive the same matrix, cache its inverse and retrieve the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## The if statement stops the function from executing in the event a non-Matrix object is passed to the function
  if(!is.matrix(x)){
      message("This variable is not a matrix.")
      return(0)
    }
  
    inv <- NULL
    
    ## Attribute 1: A set function that assigns the default input matrix and stores it in a variable called x.  Then it creates a variable called inv
    ##              which is assigned a NULL value since the inverse for this matrix has not yet been calculated.
    ##              This attribute can be used to change the default input matrix that for which the inverse is calculated
    
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    ## Attribute 2: a get function that returns the value of the default matrix x assigned using the set attribute
    get <- function() x
    
    ## Attribute 3: setInverse function that caches the value of the inverse of the input matrix x
    setInverse <- function(inv_calc) inv <<- inv_calc
    
    ## Attribute 4: getInverse function that retrieves the cached inverse of the input matrix x
    getInverse <- function() inv
    
    #The list is created and returned as the output from this function
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }


## The cacheSolve matrix takes a "CacheMatrix" object that is created using the makeCacheMatrix function above
## and checks to see if this object already has a cached value for the matrix's inverse.  If no cached value is found
## the inverse is then calculated and cached in the CacheMatrix object.  Otherwise, when the cached value of the matrix is
## retrived and returned as the output.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Stores the cached value of the inverse of the CacheMatrix object in the variable inv
    inv <- x$getInverse()
    
    ## Checks if the CacheMatrix object actually has a value stored or not and returns it if it does
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    ## If not cached value is found, it is calculated and stored in the CacheMatrix object and returned
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    return(inv)
  }

