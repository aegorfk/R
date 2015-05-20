data <- read.csv(file="matrix.csv")

#Item Based Collaborative Filtering
data.ibs <- (data[,!(names(data) %in% c("user"))])

getCosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

data.ibs.similarity  <- matrix(NA, nrow=ncol(data.ibs), ncol=ncol(data.ibs),
                           dimnames=list(colnames(data.ibs), colnames(data.ibs)))


# Lets fill in those empty spaces with cosine similarities
# Loop through the columns
for(i in 1:ncol(data.ibs)) {
  # Loop through the columns for each column
  for(j in 1:ncol(data.ibs)) {
    # Fill in placeholder with cosine similarities
    data.ibs.similarity[i,j] <- getCosine(as.matrix(data.ibs[i]),as.matrix(data.ibs[j]))
  }
}

# Back to dataframe
data.ibs.similarity <- as.data.frame(data.ibs.similarity)
data.neighbours <- matrix(NA, nrow=ncol(data.ibs.similarity),ncol=11,dimnames=list(colnames(data.ibs.similarity)))


for(i in 1:ncol(data.ibs)) 
{
  data.neighbours[i,] <- (t(head(n=11,rownames(data.ibs.similarity[order(data.ibs.similarity[,i],decreasing=TRUE),][i]))))
}


