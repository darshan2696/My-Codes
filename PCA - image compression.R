#============================================================
# Question 1 - White BG
#============================================================

#install.packages("jpeg")
library(jpeg)

img = readJPEG("D:\\Desktop folders\\test.jpeg")
dim(img)


r <- img[,,1]
g <- img[,,2]
b <- img[,,3]


img.r.pca <- prcomp(r, center = FALSE)
img.g.pca <- prcomp(g, center = FALSE)
img.b.pca <- prcomp(b, center = FALSE)

rgb.pca <- list(img.r.pca,img.g.pca,img.b.pca)

#X = P*t(A) 
# x = P
# Rotation = A

ncomp = 50
R = img.r.pca$x[,1:ncomp]%*%t(img.r.pca$rotation[,1:ncomp])
G = img.g.pca$x[,1:ncomp]%*%t(img.g.pca$rotation[,1:ncomp])
B = img.b.pca$x[,1:ncomp]%*%t(img.b.pca$rotation[,1:ncomp])

View(r)
View(R)

image = array(c(R,G,B), dim = c(dim(img)[1:2],3))

summary(image)  #the pixel value has a max of more than 1 and less than 0, it needs attention
writeJPEG(img, "puppy.jpg")



#============================================================
# Question 2 - Pixel Correction
#============================================================


R[R < 0 ] <- 0      #setting the min values to 0 if less than 0
G[G < 0 ] <- 0
B[B < 0 ] <- 0

R[R > 1 ] <- 1      #setting the max values to 1 if more than 1
G[G > 1 ] <- 1
B[B > 1 ] <- 1

image = array(c(R,G,B), dim = c(dim(img)[1:2],3))

summary(image)  
writeJPEG(img, "puppy.jpg")


#==============================================================
# Question 3 - Calculating The proprting of variance by R,G,B
#==============================================================

a <- sum(apply(R,2,var))/sum(apply(r,2,var))*100
print(paste("proportion of variance explained by R :",a))

bb <-sum(apply(G,2,var))/sum(apply(g,2,var))*100
print(paste("proportion of variance explained by G :",bb))

c <-sum(apply(B,2,var))/sum(apply(b,2,var))*100
print(paste("proportion of variance explained by B :",c))


#================================================================
# Question 4 - Choose the componentes that 99.5% of the variance
#================================================================

library(jpeg)

img = readJPEG("D:\\Desktop folders\\test.jpeg")
dim(img)


r <- img[,,1]
g <- img[,,2]
b <- img[,,3]


img.r.pca <- prcomp(r, center = FALSE)
img.g.pca <- prcomp(g, center = FALSE)
img.b.pca <- prcomp(b, center = FALSE)

x = cumsum(img.r.pca$sdev^2)/sum(img.r.pca$sdev^2)*100
y = cumsum(img.g.pca$sdev^2)/sum(img.g.pca$sdev^2)*100
z = cumsum(img.b.pca$sdev^2)/sum(img.b.pca$sdev^2)*100

for (i in 1:length(x))
{
  f=0
  f = x[i]
  if(f >= 99.5)
  {
    col = i+1
    #print(col)
    break
  }
}

for (i in 1:length(y))
{
  h=0
  h = y[i]
  if(h >= 99.5)
  {
    col_1 = i+1
    #print(col_1)
    break
  }
  
}

for (i in 1:length(z))
{
  j=0
  j = z[i]
  if(j >= 99.5)
  {
    col_2 = i+1
    #print(col_2)
    break
  }
}

ncomp_list  = c(col,col_1,col_2)                    #min number of columns required to explain 99.5% variance
ncomp = max(ncomp_list)                             #chooses the max from the above list
print(paste(paste("99.5% of the variance is explained by :",ncomp),"columns"))

rgb.pca <- list(img.r.pca,img.g.pca,img.b.pca)

#X = P*t(A) 
# x = P
# Rotation = A

R = img.r.pca$x[,1:ncomp]%*%t(img.r.pca$rotation[,1:ncomp])
G = img.g.pca$x[,1:ncomp]%*%t(img.g.pca$rotation[,1:ncomp])
B = img.b.pca$x[,1:ncomp]%*%t(img.b.pca$rotation[,1:ncomp])
dim(R)
R[R < 0 ] <- 0      #setting the min values to 0 if less than 0
G[G < 0 ] <- 0
B[B < 0 ] <- 0

R[R > 1 ] <- 1      #setting the max values to 1 if more than 1
G[G > 1 ] <- 1
B[B > 1 ] <- 1

image = array(c(R,G,B), dim = c(dim(img)[1:2],3))
dim(image)
summary(image)  
writeJPEG(img, "puppy.jpg")
