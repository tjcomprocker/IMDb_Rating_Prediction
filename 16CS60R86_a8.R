#Taking Dataset As Input
dataset_org = read.csv("movie_metadata.csv")
cleaned_dataset <- dataset_org
temp  <- c()
i <- 0
j <- 0

#Cleaning the Dataset
for (i in 1:ncol(dataset_org))
{
  if (!is.numeric(dataset_org[1,i]))
  {
    temp <- c(temp,i)
  }
}

i <- 0

for (values in temp)
{
  cleaned_dataset[,values-i] <- NULL
  i <- i + 1
}

for (i in 1:nrow(cleaned_dataset))
{
  for (j in 1:ncol(cleaned_dataset))
  {
    if (is.na(cleaned_dataset[i,j]))
    {
      cleaned_dataset[i,j] <- 0
    }
  }
}

#This function prints Top 5 Movie Reccomendation using Cosine Similarity or Euclidean Distance
top5 <- function(dataset_org,cleaned_dataset,flag)
{
  user_input <- readline(prompt="Enter the link: ")
  
  i <- 0
  j <- 0
  row_no <- 0
  dist <- 0
  distance_matrix = data.frame(no = numeric(nrow(dataset_org)), distance = numeric(nrow(dataset_org)))
  
  for (i in 1:nrow(dataset_org))
  {
    if (dataset_org[i,18] == user_input)
    {
      row_no <- i
      break
    }
  }
  
  for (i in 1:nrow(cleaned_dataset))
  {
    if (i != row_no)
    {
      a <- as.numeric(cleaned_dataset[i, ])
      b <- as.numeric(cleaned_dataset[row_no, ])
      
      if (flag == 0)
      {
        dist <- ((sum(a*b))/(sqrt(sum(a^2)))*(sqrt(sum(b^2))))
      }
      else
      {
        dist <- (sqrt(sum((a-b)^2)))
      }
      
      distance_matrix$no[i] <- i
      distance_matrix$distance[i] <- dist
    }
    else
    {
      distance_matrix$no[i] <- i
      distance_matrix$distance[i] <- -1
    }
  }
  
  distance_matrix <- distance_matrix[order(distance_matrix$distance), ]
  
  for(i in 2:6)
  {
    print (dataset_org[distance_matrix[i,1],12])
  }
}

#This function prints Top 5 Director-Actor Duo from Jaccard Similarity
jaccard <- function(dataset_org,cleaned_dataset)
{
  temp <- dataset_org[,c("movie_title","director_name","actor_1_name")]
  names(temp) <- c("movie_title","director_name","actor_2_name")
  temp <- rbind(temp,dataset_org[,c("movie_title","director_name","actor_2_name")])
  names(temp) <- c("movie_title","director_name","actor_3_name")
  temp <- rbind(temp,dataset_org[,c("movie_title","director_name","actor_3_name")])
  names(temp) <- c("Movie","Director","Actor")
  temp2 <- c()
  
  for (i in 1:nrow(temp))
  {
    if (is.na(temp[i,2]) || is.na(temp[i,3]) || temp[i,2] == "" || temp[i,3] == "" || temp[i,2] == " " || temp[i,3] == " " || is.na(temp[i,1]) || temp[i,1] == " " || temp[i,3] == "")
    {
      temp2 <- c(temp2,i)
    }
  }
  
  i <- 0
  
  for (values in temp2)
  {
    temp <- temp[(values-i)*(-1),]
    i <- i + 1
  }
  
  temp <- temp[order(temp$Director), ]
  unique_duos <- unique(temp[,c(2,3)])
  unique_duos <- cbind(unique_duos,Jaccard = 0)

  for (i in 1:nrow(unique_duos))
  {
    a <- temp[temp$Director == unique_duos[i,1] , 1]
    a <- unique(a)
    b <- temp[temp$Actor == unique_duos[i,2] , 1]
    b <- unique(b)
    unique_duos[i,3] <- (length(intersect(a,b)))/(length(union(a,b)))
  }
  
  top_duos <- data.frame(matrix(nrow=0,ncol=3))
  unique_duos <- unique_duos[order(-unique_duos$Jaccard), ]
  for (i in 1:5)
  {
    top_duos <- rbind(top_duos,unique_duos[i,])
    print (top_duos[i,])
  }
  return (top_duos)
}

#This function Predicts IMDb Score from Linear Regression
linear <- function(dataset_org,cleaned_dataset,flag)
{
  row_no <- 1
 
  if (flag == -1)
  {
    user_input <- readline(prompt="Enter the link: ")
    
    for (i in 1:nrow(dataset_org))
    {
      if (dataset_org[i,18] == user_input)
      {
        row_no <- i
        break
      }
    }
  }
  else
  {
    row_no <- flag
  }
 
  wgh <- lm(imdb_score ~ num_critic_for_reviews	+ duration + director_facebook_likes + actor_3_facebook_likes	+ actor_1_facebook_likes + gross + num_voted_users + cast_total_facebook_likes + facenumber_in_poster + num_user_for_reviews + budget + title_year + actor_2_facebook_likes +	aspect_ratio + movie_facebook_likes , data = cleaned_dataset)
  temp <- data.frame(cleaned_dataset[row_no,])
  
  if (flag == -1)
  {
    print (predict(wgh,temp))
    return (0)
  }
  else
  {
    return (as.numeric(predict(wgh,temp)))
  }
}

#This function plots graphs and prints RMSE for linear regression
plotgraph <- function(dataset_org,cleaned_dataset)
{
  y <- c()
  y_cap <- c()
  
  for (i in 1:100)
  {
    y <- c(y,cleaned_dataset[i,14])
    y_cap <- c(y_cap,linear(dataset_org,cleaned_dataset,i))
  }
  
  print (sqrt(sum((y-y_cap)^2)/100))
  
  plot(y, type ="o",col="green",xlab="Test Example No.",ylab="IMDb Score",main="Prediction of IMDb Score" , font.main=4)
  lines(y_cap, type="o", col="red")

}

#This functions trains a KNN classifier
nearest <- function(dataset_org,cleaned_dataset)
{
  library(class)
  temp <- cbind(cleaned_dataset,dataset_org[,10])
  temp2 <- c()
 
   for (i in 1:nrow(temp))
  {
    if (is.na(temp[i,17]) || temp[i,17] == "" || temp[i,17] == "")
    {
      temp2 <- c(temp2,i)
    }
  }
  
  i <- 0
  for (values in temp2)
  {
    temp <- temp[(values-i)*(-1),]
    i <- i + 1
  }
  
  train_set <- cleaned_dataset[1:2000,]
  test_set <- cleaned_dataset[2001:4000,]
  target_train_set <- temp[1:2000,17]
  target_test_set <- temp[2001:4000,17]
  
  knn_classifier <- knn(train_set,test_set,target_train_set,sqrt(nrow(test_set)))
  return(table(target_test_set,knn_classifier))
}

# Infinite While loop which gives choices
while (1)
{
  print ("1. Top 5 Recommendation with Cosine Similarity.")
  print ("2. Top 5 Reccomendation with Euclidean Distance.")
  print ("3. Top 5 Director Actor Duo.")
  print ("4. Predict IMDb Score.")
  print ("5. Plot Graph for Linear Regression & Print RMSE.")
  print ("6. KNN.")
  print ("7. Exit.")
  choice <- as.numeric(readline(prompt = "Enter your Choice:"))
  
  if (choice == 1)
  {
    top5(dataset_org,cleaned_dataset,0)
  }
  else if (choice == 2)
  {
    top5(dataset_org,cleaned_dataset,1)
  }
  else if (choice == 3)
  {
    top_duos <- jaccard(dataset_org,cleaned_dataset)
  }
  else if (choice == 4)
  {
    linear(dataset_org,cleaned_dataset,-1)
  }
  else if (choice == 5)
  {
    plotgraph(dataset_org,cleaned_dataset)
  }
  else if (choice == 6)
  {
    confusion_matrix <- nearest(dataset_org,cleaned_dataset)
    View(confusion_matrix)
  }
  else if (choice == 7)
  {
    break
  }
}

