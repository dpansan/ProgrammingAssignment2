#makeCacheMAtrix function 
#Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {
        
        #Initially the solved matrix is null
        s <- NULL
        
        #function to set
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        #function to get
        get <- function() x
        
        #function to setsolve
        setsolve <- function(solve)
                {s <<- solve}
        
        #function to getsolve
        getsolve<- function() 
                {s}
        
        #Retrieve a list with the above functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


#cacheSolve function
#Check whether the inverse of the matrix is already calculated in cache. 
#If it has calculated shows the calculation of the cache, otherwise it will be calculated.
cachesolve <- function(x, ...) {
        
        
        #Check whether the inverse of the matrix is already calculated 
        s <- x$getsolve()
        
        #If it has calculated shows "getting cached data" and the calculation of the cache
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        #else
        data <- x$get()
        
        #It estimates the inverse of the matrix and returns the value to the main function.
        s <- solve(data, ...)
        x$setsolve(s)
        s
}



#>matriz<-matrix(1:4,2,2)
#>a <- makeCacheMatrix(matriz)
#>a$get() OK
#   [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#>a$getsolve()
#NULL OK
#>cachesolve(a) OK
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > a$getsolve() OK
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cachesolve(a) OK
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5