#' @export
CHull.default <-
  function(data, bound = "lower", PercentageFit = 1){
    Error <- 0

    # Input is a percentage, further computations are treated as proportion (0-1)
    PercentageFit <- PercentageFit / 100

    # CHECK DATA
    N <- nrow(data)
    J <- ncol(data)
    if (J != 2){
      print("ERROR: data should have 2 columns")
    }
    if (is.data.frame(data)==F){
      data <- as.data.frame(data)
    }
    check <- rownames(data)==as.character(1:N)
    if (sum(check)==N){
      rownames(data) <- paste("model",1:N,sep="")
    }
    colnames(data) <- c("complexity","fit")
    data <- cbind(data,1:N)
    order <- sort(data[,1],index.return=T)
    sorted <- data[order$ix,]
    data <- sorted
    if (bound=="upper"){
      if (cor(data)[2]<0){
        print("WARNING: Check whether input parameter 'bound' is specified correctly")
      }
      data[,2] <- data[,2]*-1
    } else {
      if (cor(data)[2]>0){
        print("WARNING: Check whether input parameter 'bound' is specified correctly")
      }
    }

    # ANALYSIS
    # 1. Retain best model per complexity
    UniqueComplex <- unique(data[,1])
    nUniqueComplex <- length(UniqueComplex)
    red_x <- array(NA,c(nUniqueComplex,3))
    red_x <- as.data.frame(red_x)
    I <- array(NA,c(nUniqueComplex,1))
    for (teller in 1:nUniqueComplex){
      tempdata <- data[which(data[,1]==UniqueComplex[teller]),]
      tempindex <- which.min(tempdata[,2])
      I[teller] <- tempdata[tempindex,3]
      I[teller] <- which(data[,3]==I[teller])
    }
    red_x <- data[I,]

    # 2. Monotonical decrease
    Go_on <- T
    mon_x <- red_x
    while (Go_on){
      N_remain <- nrow(mon_x)
      t <- (mon_x[2:N_remain,2] - mon_x[1:N_remain-1,2])<0
      t <- c(T,t)
      mon_x <- mon_x[t,]
      Go_on <- sum(t==F)>0
    }
    N_remain <- nrow(mon_x)
    if (N_remain<3){
      ErrorMess <- "Not enough data points available to compute the convex hull"
      print(ErrorMess)
      results <- ErrorMess
    } else {

      # 3. Convex hull
      k <- convex_hull(as.matrix(mon_x[,1:2]))
      index <- which.min(k$resverts)
      if (length(k$resverts)==index){
        k2 <- k$resverts[seq(index,1,by=-1)]
      } else {
        k2 <- k$resverts[c(seq(index,1,by=-1),seq(length(k$resverts),index+1,by=-1))]
      }
      index2 <- which.max(k2)
      k3 <- k2[1:index2]
      k3 <- k3+1
      conv_x <- mon_x[k3,]
      N_remain <- nrow(conv_x)
      FitDiff <- array(NA,c(N_remain,1))
      FitDiff[1] <- 9999
      FitDiff[2:N_remain] <- (conv_x[1:(N_remain-1),2] - conv_x[2:N_remain,2]) / abs(conv_x[1:(N_remain-1),2])
      conv_x <- conv_x[which(FitDiff>=PercentageFit),]
      N_remain <- nrow(conv_x)

      # 4. Compute st values
      st <- array(NA,c(N_remain,1))
      if (N_remain>2){
        for (j in 2:(N_remain-1)){
          st[j] <- ((conv_x[j,2]-conv_x[j-1,2])/(conv_x[j,1]-conv_x[j-1,1])) / ((conv_x[j+1,2]-conv_x[j,2])/(conv_x[j+1,1]-conv_x[j,1]))
        }
        hull <- conv_x[,1:2]
        hull["st"] <- st
        SelectedSol <- hull[which.max(st),1:2]
      } else {
        hull <- SelectedSol <- conv_x[,1:2]
        hull["st"] <- st
      }


      # CHECK MULTIPLE SOLUTIONS
      Multi <- intersect( which( data[,1]==SelectedSol[1,1] ) , which( data[,2]==SelectedSol[1,2] ) )
      if (N_remain==2){
        Multi2 <- intersect( which( data[,1]==SelectedSol[2,1] ) , which( data[,2]==SelectedSol[2,2] ) )
        Solution <- data[c(Multi,Multi2),1:2]
      } else {
        Solution <- data[Multi,1:2]
      }

      # OUTPUT
      if (bound=="upper"){
        Solution[,2] <- Solution[,2]*-1
        hull[,2] <- hull[,2]*-1
        data[,2] <- data[,2]*-1
      }
      results <- list(Solution=Solution,Hull=hull,OrigData=data[,1:2],Bound=bound,PercentageFit=PercentageFit*100)
      class(results) <- "CHull"
    }
    return(results)
  }
