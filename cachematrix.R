## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function#The first function, makeVector creates a special "matrix", 
#which is really a list containing functions to
# set the value of the matrix
# get the value of the matrix  
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        
        if(det(x==0)){
                message("Non invertible matrix")
        }
        else{
                inv <- NULL
                set <-function(y){
                        x<<-y
                        inv <- NULL
                }
                get<-function()x
                setInv <- function(inverse) inv <<-inverse
                getInv <- function() inv
                list(set = set, get =get, setInv=setInv, getInv=getInv)
        }
}
## Write a short comment describing this function#The following function calculates the inverse of the special "Matrix" created with the above function.
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the
#inverse in the cache via the setInv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <-solve(data,...)
        x$setInv(inv)
        inv # return
}
        ## Return a matrix that is the inverse of 'x'
}
