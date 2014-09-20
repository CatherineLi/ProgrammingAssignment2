## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it repeatedly

## This function generates a list of four functions including set, get,
## setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }

        get<-function() {
                x
        }
        setinverse<-function(inverse){
                i<<-inverse
        }
        getinverse<-function(){
                i
        }
        list(set=set, get=get, 
             setinverse=setinverse,
             getinverse=getinverse
             )
}


## This function check whether computes the inverse of the 
# special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(! is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i      
        
}

