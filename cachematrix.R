  #Enter the martix Ex:V<-makeVector(c(4,3,3,2),r,c);r=rows;c=columns
# This function will return an object which is a list
makeCacheMatrix <- function(x = numeric(),r,c) {  # input x will be a vector
  k<-matrix(x,r,c)                          # converts it into matrix
  m <- NULL                                 # m is set to NULL everytime makeVector is called
  set <- function(y) {                      # use to set values, not used here
    k <<- y
    m <<- NULL
  }
  get <- function() k                       # returns the value of the original vector
  setinverse <- function(inverse) m <<- inverse # used to store the value
  getinverse <- function() m                    # if the inverse is already stored,this function will return the stored value
  list(set = set, get = get,                     # list of internal function
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function will check if the inverse already stored,if its stored then returns inverse else it calculates inverse and returns inverse 
cacheSolve <- function(k,...) {
  m <- k$getinverse()                      #gets the value of the inverse
  if(!is.null(m)) {                     # checks whether m is NULL
    message("getting cached data")        
    return(m)                             #returns the inverse if m is not NULL
  }
  data <- k$get()                         
  m <- solve(data,...)                    #calculates the inverse
  k$setinverse(m)                         # stores the calculated inverse
  m                                        # returns the inverse
}

