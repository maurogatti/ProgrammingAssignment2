#This R script file contains four functions
#The function makeCacheMatrix creates the special vector required by cacheSolve function
#The function cacheSolve computes the inverse matrix or retrieves it from cache (when available)
#The function testCacheMatrix1 is used to verify that the method cacheSolve actually returns the inverse matrix
#The function testCacheMatrix2 is used to verify that the method cacheSolve actually retrieves the data item from cache when possible

#Function that creates the special vector used by cacheSolve function
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setCacheMatrix <- function(solve) m <<- cacheSolve
        getCacheMatrix <- function() m
        list(set = set, get = get,
             setCacheMatrix = setCacheMatrix,
             getCacheMatrix = getCacheMatrix)
}

#Function that computes the inverse matrix or retrieves it from cache whenever value has been cached
cacheSolve <- function(x, ...) {
        m <- x$getCacheMatrix()
        if(!is.null(m)) {
                message("Successful test. Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setCacheMatrix(m)
        m
}

#Unit test 1: the function should return Successul test. This test verifies that
#the returned matrix is actually the inverse matrix
testCacheMatrix1 <- function() {
        n<-sample(20,1); #matrix to be inverted dimension
        testMatrix<-matrix(rnorm(n^2,0,1),n,n); #matrix to be inverted
        inverseMatrix<-cacheSolve(makeCacheMatrix(testMatrix));
        if(isTRUE(all.equal(testMatrix%*%inverseMatrix,diag(n))))
        {
                message("Successful test. The cached matrix is the inverse matrix.")     
        }
        else
        {
                message("Unsuccessful test. The cached matrix is not the inverse matrix.") 
        }   
}

#Unit test 2: the function should return Successul test. This test verifies that 
#the value is actually returned from cache
testCacheMatrix2 <- function() {
        n<-sample(20,1); #matrix to be inverted dimension
        testMatrix<-matrix(rnorm(n^2,0,1),n,n); #matrix to be inverted
        testCacheMatrix<-makeCacheMatrix(testMatrix)
        inverseMatrix<-cacheSolve(testCacheMatrix);
        inverseMatrix<-cacheSolve(testCacheMatrix);
}

