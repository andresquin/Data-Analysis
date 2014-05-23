## Put comments here that give an overall description of what your
## functions do
## Pair of functions that cache the inverse of a matrix.
## In order to be more eficient computation wise and save some time machine.


## Write a short comment describing this function
## makeCacheMatrix: create an advanced matrix which also contains its inverse

makeCacheMatrix <- function(x = matrix()) {

##  set the value of the matrix
##  get the value of the matrix
##  set the value of inverse of the matrix
##  get the value of inverse of the matrix

inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv

## Return the matrix with the new functions

    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

##  Compute the inverse of a matrix if the inverse has not been calculated already
## Return a matrix that is the inverse of 'x'
   inv <- x$getinv()

# If the inverse is already calculated, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

## The inverse is not yet calculated, that makes it calaculate
    data <- x$get()
    inv <- solve(data, ...)

## Cache the inverse
    x$setinv(inv)

## Return the result
    inv

}

## Exemple
## 
##  x <- matrix(rnorm(25), nrow = 5)   I  Created a matrix x
##  c <- makeCacheMatrix(x)    I returned the special matrix
##  c$get()     Return the matrix
##  cacheSolve(c)      Return the inverse
