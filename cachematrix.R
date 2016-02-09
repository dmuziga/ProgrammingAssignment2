## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        #begining by defining the Inverse matrix to Null as a place holder for future value
        
        I <- NULL
        
        set <- function(y) {
                
                #re-assign the  matrix to Y
                x <<-y 
                #re- assign the Inverse of the matrix to NULL
                I <<- NULL
                
        }
        #get the matrix 
        get <- function () x  
        
        #set the Inverse Matrix
        
        setInverse <- function(Inverse) I <<- Inverse
        
        # Get the inverse Matrix stored in the memory
        getInverse <- function() I
        
        list (set = set,get=get, setInverse= setInverse,getInverse = getInverse)

        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #I<- x$getInverse()
        
        #check if there is an Inverse Matrix in the memory 
        I <- x$getInverse()
        
        #if I it is not null return the Inverse Matrix otherwise compute the Inverse with the Solve () function
       if (!is.null(I)){
                
                message("Generating the Inverse")
                
                return (I)
        }
        #retrieve the Matrix
        dataMa<-x$get()
        
        #calculate the Inverse of the Matrix
        I <- solve(dataMa)
        
       # Assign the Inverse vector to the I variable
        x$setInverse(I)
        
        #return the Inverse Matrix
        I
}
