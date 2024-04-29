library(dplyr)
library(tidyverse)  
library(factoextra)  
library(caret)
library(cluster)
library(ggplot2)

data_student <- read.csv("C:\\Users\\Dhair\\Downloads\\oulad-students.csv")

str(data_student)
summary(data_student)

data_student$final_result <- as.factor(data_student$final_result)
data_student$gender <- as.numeric(data_student$gender)
data_student$id_student <- as.numeric(data_student$id_student)


# Generate simulated learning data
set.seed(123)
n_students <- 1000
student_id <- 1:n_students
feature1 <- rnorm(n_students, mean = 50, sd = 10)
feature2 <- rnorm(n_students, mean = 70, sd = 15)
data <- data.frame(Student_ID = student_id, Feature_1 = feature1, Feature_2 = feature2)

# Perform PCA for dimensionality reduction
pca_red <- prcomp(data[,c("Feature_1", "Feature_2")], center = TRUE, scale. = TRUE)

# Plot the PCA results
fviz_eig(pca_red) + ggtitle("Scree Plot")
fviz_pca_var(pca_red) + ggtitle("Variable Loading Plot")
fviz_pca_biplot(pca_red, repel = TRUE, title = "Biplot")

# Perform K-means clustering
set.seed(123)
kmeans <- kmeans(pca_red$x[,1:2], centers = 5, nstart = 20)

# Plot the clustering results
fviz_cluster(kmeans, data[,c("Feature_1", "Feature_2")]) + ggtitle("Clustering Results")

# Add cluster labels to the original data
data$Cluster <- as.factor(kmeans$cluster)

# Summary of the clustering results
summary(data$Cluster)