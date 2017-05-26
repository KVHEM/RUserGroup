d <- iris # let "d" refer to one of R's built in data sets
vars <- c('Sepal.Length','Sepal.Width','Petal.Length')
yName <- 'Species'
yLevels <- sort(unique(as.character(d[[yName]])))

bindToEnv <- function(bindTargetEnv=parent.frame(),objNames,doNotRebind=c()) {
  # Bind the values into environment
  # and switch any functions to this environment!
  for(var in objNames) {
    val <- get(var,envir=parent.frame())
    if(is.function(val) && (!(var %in% doNotRebind))) {
      # replace function's lexical environment with our target (DANGEROUS)
      environment(val) <- bindTargetEnv
    }
    # assign object to target environment, only after any possible alteration
    assign(var,val,envir=bindTargetEnv)
  }
}

fitOneTargetModel <- function(yName,yLevel,vars,data) {
  formula <- paste('(',yName,'=="',yLevel,'") ~ ',
                   paste(vars,collapse=' + '),sep='')
  glm(as.formula(formula),family=binomial,data=data)
}

parallelCluster <- parallel::makeCluster(parallel::detectCores())

mkWorker <- function() {
  bindToEnv(objNames=c('yName','vars','d','fitOneTargetModel'))
  function(yLevel) {
    fitOneTargetModel(yName,yLevel,vars,d)
  }
}

models <- parallel::parLapply(parallelCluster,yLevels,
                              mkWorker())
names(models) <- yLevels
print(models)

if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster <- c()
}

##Parallel loop

library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

finalMatrix <- foreach(i=1:150000, .combine=cbind) %dopar% {
  tempMatrix = functionThatDoesSomething() #calling a function
  #do other things if you want
  
  tempMatrix #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
}
#stop cluster
stopCluster(cl)