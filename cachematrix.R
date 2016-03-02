
## This pair of functions is designed to save computation time for a process that requires frequently
## calculating the inverse of a matrices. 
## Together, the functions calculate and cache the inverse of a matrix argument. If the inverse of the matrix
## argument has previously been calculated, the cached answer is returned instead of being calculated,
## thus saving time and computational power. 

## The makeCacheMatrix function creates a list that contains 4 functions: set, get, setInv, and getInv

## 1. The "set" function is used to assign a matrix to be inverted. It also sets the inverse back to null 
## because the matrix has been changed and if there was already a cached inverse, it is no longer valid. This
## function is initially called when the makeCacheMatrix is created, but it can also be called on its own to 
## change the matrix to be inverted.
## 2. The "get" function simply returns the matrix that will be inverted.
## 3. The "setInv" function is used to assign the matrix inverse to the variable "i". "i" is null until the 
## inverse is calculated once. 
## 4. the "getInv" function returns the value of the matrix inverse, "i". 

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(A) {
                x <<- A
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inv) i <<- inv
        getInv <- function() i
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve takes the list variable produced by makeCacheMatrix and checks to see if the inverse "i" has
## been calculated. If the matrix inverse was previously cached, "i" will not be NULL and the cached value
## is returned. Otherwise, if "i" is NULL, the inverse is calculated and cached.

cacheSolve <- function(x, ...) {
        
        i <- x$getInv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setInv(i)
        i
}
        
