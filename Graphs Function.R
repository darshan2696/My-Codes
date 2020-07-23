# A FUNCTION THAT EXPORTS GRAPHS
#----------------------------------


Graphs <- function(data,var= 1:ncol(data), direct= "",tresh=10)                     #feed your working directry path into direct 
{
  setwd(direct)
  for (i in var)                                                                    #var is the number of choosen columns in the dataset, by default all columns will be taken
  {
    test = table(data[,i])  
    if(is.numeric(data[,i]) && length(test) > tresh/100*nrow(data))                 #tresh is used to ensure that the categorical and numerical variables are classified correctly
    {
      
      png(paste(names(data)[i], ".png", sep=""))                                    #NOTE this step
      
      par(mfrow=c(2,1))                                                             #used to display 2 graphs in a single picture 
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]),                 #boxplot
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]),                  #histogram
           xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
      
      
      
      dev.off()                                                                     #export
      
    }
    else
    {
      png(paste(names(data)[i], ".png", sep=""))
      
      par(mfrow=c(2,1))
      
      barplot(table(data[,i]) , main = paste("Barplot of", names(data)[i]),        #barplot
              ylab = names(data)[i], col = "maroon", border = "grey5" )
      
      pie(table(data[,i]) , main = paste("Piechart of", names(data)[i]),           #pie chart
          ylab = names(data)[i], col = "lightgreen")
      
      dev.off()
    }
  }
}


#example:
#Graphs(cars,c(1,3,5,10,11))
#here, only the graphs relating to columns 1,2,5,10,11 will get eporting to you set 
#working directory
