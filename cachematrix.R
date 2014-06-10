## The First function, makeCacheMatrix creates a special "matrix object that can :

## Set the value of the matrix 
## Get the value of the matrix 
## Cache the inverse of the matrix
## Get the cached value of the inverse of the matrix 

## Example of use:
## 1-    a<- makeCacheMatrix(matrix(1:4,2,2)) <==== Set the matrix 
## 2-    a$get()                              <==== get the matrix
## 3-    cacheSolve(a)                        <==== computes the inverse of the matrix a
## 4-    cacheSolve(a)                        <==== Check if the function retrieve the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
        
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)      

}


## The function, cacheSolve calculates the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, the function retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
            
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setmatrix(m)
        m
             
}
