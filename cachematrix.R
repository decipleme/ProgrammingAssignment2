
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. Below are defined, two functions that can help
## cache the inverse of the matrix

## makeCacheMatrix: This function creates a special "matrix" object that 
##                  can cache its inverse
## It defines five functions
## 1. setcachematrix() - Sets the matrix to be cached
## 2. getcachematrix() - Retrieves the cached matrix
## 3. getprevmatrix()  - Retrieves the cached previous matrix
## 4. setinverse()     - Sets the inverse of the matrix to be cached
## 5. getinverse()     - Retrieves the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL   ## holds the cached inverse of the matrix
    prev_mat <- NULL  ## holds the previous matrix
    
    setcachematrix <- function(cache_mat) {
        if (!is.null(prev_mat)) {
            ## cache the previous value and update the new one
            prev_mat <<- x
            x <<- cache_mat
        }
        else {
            x <<- cache_mat
            prev_mat <<- x
            inv_mat <<- NULL
        }
        
    }
    
    getcachematrix <- function() {
        x
    }
    
    getprevmatrix <- function() {
        prev_mat
    }
    
    setinv <- function(inverse) {
        inv_mat <<- inverse
    }
    
    getinv <- function() {
        inv_mat
    }
    
    list(setcachematrix = setcachematrix, 
         getcachematrix = getcachematrix,
         getprevmatrix  = getprevmatrix,
         setinverse     = setinv,
         getinverse     = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
##             returned by makeCacheMatrix above. If the inverse has already 
##             been calculated (and the matrix has not changed), then the 
##             cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## first get the inverse from the cache and check if it is usable
    inv_mat <- x$getinverse()
    if(!is.null(inv_mat) && 
       identical(x$getprevmatrix(), x$getcachematrix())) {
        message("getting cached inverted matrix")
        return(inv_mat)
    }
    
    ## next get the inverse of the matrix and update the cache
    cmat <- x$getcachematrix()
    inv_mat <- solve(cmat)
    x$setinverse(inv_mat)
    
    ## finally, return a matrix that is the inverse of 'x'
    inv_mat
}
