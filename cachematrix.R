## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function takes a supplied matrix, and can cache
## and load the matrix and inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  
  ##cachedInverse <- NULL
  #### GET AND SET INVERSES
  getInverse <- function() 
  {
    if (exists("cachedInverse"))
    {
      return(cachedInverse)
    } else
    {
      return(NULL)
    }
  }
  setInverse <- function(y)
  {
    print("setting cached Inverse")
    l <-solve(y)
    cachedInverse <<- l
  }
  getMatrix <- function() 
  {
    return(cachedMatrix)
  }
  setMatrix <- function()
  {
    
    cachedMatrix <<- x
  }
  get <- function() 
  {
    return(x)
  }
  
  
  list(
    getMatrix = getMatrix,
    setMatrix = setMatrix,
    get = get,
    getInverse = getInverse,
    setInverse = setInverse
  )
}



## Write a short comment describing this function
## returns an inverse of the supplied matrix, using a cached version
## when nessesary

cacheSolve <- function(x, ...) {
  
  ## CHECK EQUALITY ###############
  checkEquality <- function(x,y){
    a <- (x==y)
    nrows <- nrow(a)
    ncols <- ncol(a)
    for (i in 1:ncols)
    {
      for (j in 1:nrows)
      {
        if (a[i,j]==FALSE)
        {
          return(FALSE)
        }
      }
    }
    return(TRUE)
  }
  
  
  if (!exists("cachedMatrix"))
  {
    print("No chached matrix found, creating..")
    x$setMatrix()
  }
  ## Return a matrix that is the inverse of 'x'
  ##Check if cached matrix exists
  if(!is.null(x$getInverse()))
  {
    print("Cached Inverse exists")
    ##if so, check if newMatrix=old matrix
    if(checkEquality(cachedMatrix,x$get()))
    {
      print("Cached matrix same as supplied matrix")
      ##if so, return oldinverse
      return(x$getInverse())
    }
    ##else make newinverse, store and return
    print("Cached matrix does not match supplied,ceating new")
    x$setMatrix()
    x$setInverse(x$get())
    return(x$getInverse())
  } 
  else 
  {
    ##else create new newinverse, store and return
    print("No chached inverse found, creating new")
    x$setInverse(x$get())
    return(x$getInverse())
  }
}


