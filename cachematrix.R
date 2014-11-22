## Create by Harry Werder for Coursera Assignment 2
## makeCacheMatrix creates a list with four functions
## After the call of makeCacheMatrix the user must call the function "set" to create the cached values.
## cacheSolve is calculating the inverse and store the value in the "global" variable
## getinverse will retieve the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    #at first call; initialise the inverse variable
    Xinverse<-NULL
    
    #set creates two "global" variables. One for matrix x and one for the inverse
        set<- function(y=matrix()){
        #assign a "global" variable to matrix x
        x<<-y
        #assign a "global" variable to matrix inverse
        xinverse<<-NULL
        }
   
    #get returns the "global value of x as defined in set
        get<- function()x
    
    #setiverse assigns the inverse z to the global variable that was created with set
        setinverse<- function(z) xinverse <<-z
    
    #getinverse returns the matrix of the inverse
        getinverse<-function()xinverse
    
    
    #create the list with the functions as the return von makeCacheMatrix
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##  cachesove calculates the inverse only if it is not yet available in the cache of global variable xinverse

cacheSolve<-function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    #load the value to "global" variable xinverse
    z<-x$getinverse()
       if(!is.null(z)){
           message("getting cached data")
           return(z)
        #return is the final result of cacheSolve
       }
   
   #if was false then we must calculate the inverse
   #load the global variable with the values from matrix X
   data<-x$get()
   #calculate the inverse of x
   datainverse<-solve(data)
   #assign the calculate inverse to the global variable xinverse
   x$setinverse(datainverse)
      
}
