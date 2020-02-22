# Jasjeet S. Sekhon
# UC Berkeley
# nov 13, 2013
#
# version 4.1, updated for parallel (instead of the older snow package)
# Version 3, fixes a warning message on OS X
# Version 2, runs also on Windows but requires special version of snow, but no cygwin
#

library(rgenoud)
library(parallel)

NCPUS <- function(nchips=FALSE, ...)
{
  nchips <- as.integer(nchips)
  if(!is.integer(nchips))
    stop("'nchips' must be an integer.\n")
  
  if(nchips < 0)
    stop("'nchips' must be a positive integer if it is specified.\n")
  
  
  #should we figure out how many cpus to use?
  if(nchips==FALSE)
  {
    if(.Platform$OS.type=="unix")
    {
      sys <- as.list(Sys.info())$sysname
      
      if(is.null(sys))
      {
        stop("as.list(Sys.info())$sysname returned a NULL")            
      }
      
      if(sys=="Linux" | sys=="linux")
      {
        foo <- paste("cat /proc/cpuinfo | grep processor | wc | awk '{print $1}'")
        nchips <- as.integer(system(foo, intern=TRUE))
        
      } else if (sys=="Darwin" | sys=="darwin") #this should also work for any BSD system?
      {
        foo <- paste("sysctl hw.ncpu | head -1 | awk '{print $2}'")
        nchips <- as.integer(system(foo, intern=TRUE))            
      } else {
        stop("unix system not found. Please explicitly specify the number of chips you want to use.")
      }
      
    } else {
      stop("I think you are using Windows. For this OS please explicitly specify the number of chips you want to use.") 
    }
  }
  
  cat("...using",nchips,"CPUs on this computer.\n")
  
  if(.Platform$OS.type != "windows")
  {
    cl <- makeCluster(rep("localhost", nchips), type="PSOCK")
  } else {
    warning("it may not work on Windows")
    cl <- makeCluster(rep("localhost", nchips), type="SOCK")
  }
  return(cl)
}