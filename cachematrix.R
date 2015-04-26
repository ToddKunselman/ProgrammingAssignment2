## The following two functions, makeCacheMatrix and cacheSolve are used inconjunction 
## to cache the inverse of a matrix and then either computes the matrix or retrieves 
## the inverse cache matrix.


## makeCacheMatrix  is a matrix function which contains a list of 4 other functions: 
## set, get, setsolve, and getsolve.  The function takes the matrix and caches its 
## value for future use.

makeCacheMatrix <- function(x = matrix()) {
        rix <- NULL             ## This allows the variable rix undefined, yet still 
                                ## be used later.
        
                                ## The set function changes the matrix stored in the 
                                ## main function. This function isn't needed unless 
                                ## the matrix changes.  
        set <- function(y) {
                x <<- y         ## this substitutes the matrix x with a y input 
                                ## in the mainfunction.  The <<- operator causes 
                                ## a search through parent environments for an 
                                ## existing definition.  Without this operator 
                                ## the matrix x would only be substituted with y 
                                ## in the set function
                
                rix <<- NULL    ## restores the null value to the variable rix, 
                                ## because the old value is no longer needed and 
                                ## the new value needs to be recaculated in the 
                                ## function cacheSolve
        }
        get <- function() x     ## get is a function that returns the matrix stored
                                ## in the main function
        
        setsolve <- function (solve) rix <<- solve   ## setsolve is a function that 
                                ## stores the value of the input in the variable rix
                                ## into the main function.
        
        getsolve <- function() rix      ## getsolve returns the value in setsolve
        
        list(set = set, get = get, setsolve = setsolve, getsolve=getsolve)
                                ## the list stores the 4 functions of makeCacheMatrix
                                ## so that the main function can have an object stored to it
        

}


## cacheSolve is a function that computes the inverse of the special matrix object 
## returned by the function makeCacheMatrix.  If the inverse has already been calculated
## then cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## it first verifies the value solve stored previously with getsolve.  If it 
        ## exists in memory, it returns the message and the value solve.
        solve <- x$getsolve()
        if(!is.null(solve)) {
                message ("getting cached data")
                return (solve)
        }
        data <- x$get()  ## data gets the matrix stored with makeCacheMatrix
        solve <- solve(data, ...) ## solve calculates the inverse of the matrix
        x$setsolve(solve)  ## this stores it in the object solve
        solve
}
