#===============================================================================================
#                    KNN from scratch
#===============================================================================================


KNN <- function(data,k_neigh= c(1),method = c("Regression","Classification"),split = 0.7,tresh = c(5), uniq = 75, y = "")
{
  library(heuristica)
  categ <- c()                                          #A vector to store categorical variables
  numb <- c()                                           #A vector to store numerical variables
  j = 1                                                 #Both used to appenend into the above to vectors
  k = 1
  
  set.seed(123)
  data <- data[sample(nrow(data)),]
  #View(data)

  # SORTING THE DATASET INTO CATEGORICAL & NUMERICAL  
  
  Y <- as.data.frame(data[[y]])

  
  data_1 <- as.data.frame(data)                         #Copy of the orginal dataset
  #View(data_1)
  
  for (i in 1:ncol(data))                               #removing the unique id from the data so it does not interfere with the tests
  {
    if(length(unique(data[,i])) > uniq/100*nrow(data) && uniq != 0)
    {
      print("\n The Unique Identifier in the dataset \n")
      print("")
      print(colnames(data[i]))
      print("")
      unique_id <- data.frame(data[,i])                 #if it does not retun a value, then there are no unique id
      data[,i] <- NULL
      break
      
    }
  }
  
  for(i in 1:ncol(data))                                #checks is the data is 'char' or 'factor'
  {
    if(class(data[1,i]) == "character" || class(data[1,i]) == "factor")
    {
      categ[j] = i                                      #if true, appened the index number to 'categ'
      j <- j + 1
    }
  }
  #View(categ)
  
  for(i in 1:ncol(data))
  {
    test = table(data[,i])
    if((class(data[1,i]) == "numeric" ||class(data[1,i]) == "integer" )
       && length(test) < tresh/100*nrow(data) )         #tresh need to be adjust based on the dataset
    {                                                   #checks if its numeric and the number of unique elements
      categ[j] = i                                      #If true, appends the index number to 'categ'
      j <- j + 1
    }
  }
  #View(categ)
  
  for(i in 1:ncol(data))
  {
    test = table(data[,i])
    if(((class(data[1,i]) == "numeric" ||class(data[1,i]) == "integer" )&& length(test) > tresh/100*nrow(data) ) && class(data[,i]) != "character")
    {                                                   #checks if its numeric and the number of unique elements
      numb[k] = i                                       #If true, appends the index number to 'numb'
      k <- k + 1
    }
  }
  #View(numb)
  
  
  categ_data <- data.frame(data[,as.numeric(categ)])    #the index from 'categ' is used to make the 'categ_data', which is also a dataframe
  
  if ( ncol(categ_data) > 1)
  {
    for (i in 1:ncol(categ_data))
    {                                                   # conveting categorical varibales into Numerical indicator variable
      categ_data[,i] <- as.factor(categ_data[,i])
      categ_data[,i] <- as.numeric(categ_data[,i])
    }
    
  }
  
  else
  {
    categ_data <- as.factor(categ_data)
    categ_data <- as.numeric(categ_data)
  }
 
  categ_data_frame <- as.data.frame(categ_data)
  #View(categ_data)
 
  
  print("\n Categorical Variables in the dataset \n")
  print("")
  print(colnames(categ_data))
  #print("Check the data to see if it is classified correctly, if not - tune tesh")
  numb_data <- data[,as.numeric(numb)]                  #the index from 'numb' is used to make the 'numb_data', which is also a dataframe
  numb_data_frame <- as.data.frame(numb_data)
  #View(numb_data)
  print("")
  print("\n Numerical Variables in the dataset \n")
  print("")
  print(colnames(numb_data))
  #print("Check the data to see if it is classified correctly ,if not -> try diffrent numbers for tesh")
  print("")
  
  line <- readline("Are the variable sorted correctly ?? (y/n)")
  if(line == "n")
  {
    print("press 'ESC' button &")
    print("please adjust the tresh values in the function till the variables are sorted properly")
    break()
  }
  
  #print(ncol(categ_data_frame))
  
  if(ncol(categ_data_frame)== 1) 
    {
      data_new <- data.frame(numb_data)
    }
  else
    {
      data_new <- data.frame(categ_data,numb_data)
    }
  
  #View(data_new)
  
  #View(data_new)
  #set.seed(1123)
  train <- data_new[1:round(nrow(data_new)*split),]
  test <- data_new[(round(nrow(data_new)*split)+1):nrow(data_new),]
  Y_test <- Y[round(nrow(Y)*split+1):nrow(Y),]
  Y <- Y[1:round(nrow(Y)*split),]
  
  #print(dim(train))
  #print(dim(test))
  #View(train)
  #View(Y)
  #View(test)
  #View(Y_test)
  
  
  NN <- function(a,b,c,k,method = c("Regression","Classification"))
  {
    
    train <- data.matrix(train)
    test <- data.matrix(test)
    z <- c
    pred <-c()
    for( j in 1:nrow(test))
    {
      d = c()
      Y <- z
      for (i in 1:nrow(train))
      {
        d[i] <- list(c((sqrt(sum((test[j,]-train[i,])^2))),(i)))
      }
      d <- data.frame(d)
      d <- t(d)
      d <- data.frame(d)
      d <- d[order(d$X1),]
      min_dist <- d$X1[1:k_neigh]
      #print(min_dist)
      index <- d$X2[1:k_neigh]
      #print(index)
      Y <- Y[index]
      
      if(method == "Classification")
      {
        pred[j] <- list(c((max(Y)),(j+nrow(train))))
      }
      if(method == "Regression")
      {
        pred[j] <- list(c((as.character(round(mean(Y),1))),(as.character(j+nrow(train)))))
      }
    }
    return(pred)
  }
  
  a=NN(train,test,Y,k_neigh,method)
  print("######################Prediction#####################")
  print(paste("######## Target Variable :",y,"#################"))
  a <- data.frame(a)
  a <- t(a)
  rownames(a) <- 1:nrow(a)
  colnames(a) <- c("pred","test_index")
  print(a)
  
  b <- data.frame(a,Y_test)
  names(b) <- c("Pred","test_index","Actual")
  rownames(b) <- 1:nrow(b)
  
  
  if(method == "Classification")
  {
    #print('yo')
    true = 0
    for (i in 1:nrow(b))
    {
      if(b$Pred[i] == b$Actual[i])
      {
        true = true + 1 
      }
    }
    accuracy = true/nrow(test)
    print(paste("The accuracy is : ",accuracy))
  }
  else
  {
    for(i in 1:nrow(b))
    {
      accuracy[i] = abs(b$Actual[i] - as.numeric(b$Pred[i]))^2/nrow(b)
    }
    accuracy <- sqrt(sum(accuracy))
    
    print(paste("The RMSE is : ",accuracy))
  }
  return(b)
  
  #View(distance)
}


b = KNN(cars,k_neigh =3,y= "Origin",method = "Classification",tresh = 5)


View(b)
cars= read.csv("C:\\Users\\ethic\\Downloads\\python files\\cars.csv")

install.packages("sklearn")
library(sklearn)
confu