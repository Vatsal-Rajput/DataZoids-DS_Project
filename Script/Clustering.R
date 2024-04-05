library(readxl)
library(dplyr)

data=read_excel('MarketingData.xlsx')

dim(data)
head(data)


# seperating out categorical and numerical columns
categorical_columns <- data %>%
  select(where(is.character))
numerical_columns <- data %>%
  select(-all_of(colnames(categorical_columns)))

# Identify and filter out columns with zero variance or constant values 
required_columns <- numerical_columns[, sapply(numerical_columns, function(col) var(col, na.rm = TRUE) != 0)]    

# Check for any missing values in the entire dataset  
sum(is.na(required_columns))

# Remove rows with missing values
required_columns <- na.omit(required_columns) 

# Now perform PCA on the filtered data  
pc <- prcomp(required_columns, scale. = TRUE)  

# Check the summary of the PCA  
summary(pc)  

# plotting the result
plot(pc)
plot(cumsum(pc$sdev^2/sum(pc$sdev^2)))

# retaining components which explain upto 80% variance
k <- which(cumsum(pc$sdev^2/sum(pc$sdev^2)) >= .8)[1]
reduced_compo <- pc$x[,1:3]

autoplot(pc, data=required_columns)




# Install required libraries

library(cluster)
library(factoextra)
library(ggplot2)


n_data = reduced_compo


# Elbow method to find optimal number of clusters and Plotting the elbow curve
fviz_nbclust(n_data, kmeans, method = "wss") +
  labs(title = "Elbow Method", x = "Number of clusters", y = "WCSS")


# Perform k-means clustering with 4 clusters
kmeans_model <- kmeans(n_data, centers = 4, nstart = 25)

# Create a data frame with cluster assignments and PCA coordinates
cluster_data <- data.frame(
  cluster = as.factor(kmeans_model$cluster),
  pca_1 = n_data[, 1],
  pca_2 = n_data[, 2]
)

# Plot the clusters
ggplot(cluster_data, aes(x = pca_1, y = pca_2, color = cluster)) +
  geom_point(size = 3) +
  geom_point(data = data.frame(pca_1 = kmeans_model$centers[, 1], pca_2 = kmeans_model$centers[, 2]),
             aes(x = pca_1, y = pca_2), color = "black", size = 5, shape = "x") +
  scale_color_manual(values = c("orange", "green","pink","blue")) +
  labs(title = "Clusters of customers", x = "pca_1", y = "pca_2") +
  theme_minimal()

cluster = cluster_data[,1]
final_data = cbind(required_columns,cluster)
dim(final_data)
head(final_data)
View(final_data)

final_data = final_data %>% mutate(total_Mnt = MntWines+MntFruits+MntMeatProducts+MntFishProducts+MntSweetProducts+MntGoldProds)
final_data = final_data %>% mutate(num_children = Teenhome+Kidhome)
final_data = final_data %>% mutate(num_total_purchases = NumDealsPurchases+NumWebPurchases+NumCatalogPurchases+NumStorePurchases+NumWebVisitsMonth)

# Creating a scatter plot displaying income and amount spend by different clusters
ggplot(final_data, aes(x = Income, y = total_Mnt, color = factor(cluster))) +
  geom_point() +
  labs(color = "Cluster") + coord_cartesian(xlim =c(0, 160000), ylim = c(0, 2500))

# Remove outliers from the data
df_no_outliers <- subset(final_data, Income <= 250000)

View(df_no_outliers)
# Define basic information columns
basic_info <- c('Income', 'num_children', 'Kidhome', 'Teenhome', 'num_total_purchases', 'total_Mnt', 'cluster')
# Group by cluster and calculate mean for basic information
basic_info_summary <- t(aggregate(cbind(Income, num_children, Kidhome, Teenhome, num_total_purchases, total_Mnt) ~ cluster, data = df_no_outliers, FUN = mean))

basic_info_summary = as.data.frame(basic_info_summary)

#from summary we can see that arrangement of cluster in decreasing order of income is 1,4,2,3

#studying nature of spending and income by different clusters

p1 <- ggplot(df_no_outliers, aes(x = Income, y = total_Mnt, color = factor(cluster))) +
  geom_point() +
  ggtitle("Income vs Total Spending") + coord_cartesian(xlim =c(0, 160000), ylim = c(0, 2500))

plot(p1)

<<<<<<< HEAD
p2 <- ggplot(df_no_outliers, aes(x = factor(cluster), y = Income),fill = cluster) +
  geom_boxplot(fill = "pink")+theme(legend.position="none")+
=======
p2 <- ggplot(df_no_outliers, aes(x = factor(cluster), y = Income, fill = factor(cluster))) +
  geom_boxplot()+theme(legend.position="none")+
>>>>>>> db55d91d629e76f58f6b9eacc355eafaa892c6de
  ggtitle("Income by Cluster")+coord_cartesian( ylim = c(0, 160000))

plot(p2)

<<<<<<< HEAD
p3 <- ggplot(df_no_outliers, aes(x = factor(cluster), y = total_Mnt),fill = cluster) +
  geom_boxplot(fill = "blue")+theme(legend.position="none")+
=======
p3 <- ggplot(df_no_outliers, aes(x = factor(cluster), y = total_Mnt, fill = factor(cluster))) +
  geom_boxplot()+theme(legend.position="none")+
>>>>>>> db55d91d629e76f58f6b9eacc355eafaa892c6de
  ggtitle("Total Spending by Cluster")+coord_cartesian( ylim = c(0, 2500))
plot(p3)




# Count plots for num_children, Kidhome, Teenhome

p4 <- ggplot(df_no_outliers, aes(x = factor(cluster), fill = factor(num_children))) + geom_bar(position = "dodge")+labs(title = "Count of Clusters by Number of Children")
p5 <- ggplot(df_no_outliers, aes(x = factor(cluster), fill = factor(Kidhome))) + geom_bar(position = "dodge")+labs(title = "Count of Clusters by Kidhome")
p6 <- ggplot(df_no_outliers, aes(x = factor(cluster), fill = factor(Teenhome))) + geom_bar(position = "dodge") + labs(title = "Count of Clusters by Teenhome")


plot(p4)
plot(p5)
plot(p6)


# Spending category analysis
<<<<<<< HEAD
spending_category <- c('MntWines', 'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds', 'cluster')
spend <- aggregate(cbind(MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds) ~ cluster, data = df_no_outliers, FUN = sum)
spend <- spend[, -ncol(spend)]
View(spend)
spend = as.matrix(spend)
typeof(spend)


# Percentage of spending by clusters within each category
spend_pct_by_cluster <- prop.table(spend,1)

# Check data types of columns
str(df_no_outliers)

# Convert character columns to numeric if necessary
df_no_outliers$MntWines <- as.numeric(df_no_outliers$MntWines)
df_no_outliers$MntFruits <- as.numeric(df_no_outliers$MntFruits)
df_no_outliers$MntMeatProducts <- as.numeric(df_no_outliers$MntMeatProducts)
df_no_outliers$MntFishProducts <- as.numeric(df_no_outliers$MntFishProducts)
df_no_outliers$MntSweetProducts <- as.numeric(df_no_outliers$MntSweetProducts)
df_no_outliers$MntGoldProds <- as.numeric(df_no_outliers$MntGoldProds)

# Aggregate the sum of spending categories by cluster
spending_category <- c('MntWines', 'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds', 'cluster')
spend <- aggregate(. ~ cluster, data = df_no_outliers[, spending_category], FUN = sum)

# Remove the last column (cluster) before converting to matrix
spend <- spend[, -ncol(spend)]

# View the aggregated data
View(spend)

# Convert to matrix
spend_matrix <- as.matrix(spend)

# Calculate percentages by row
spend_pct_by_cluster <- prop.table(spend_matrix, 1)

# View the percentage data
View(spend_pct_by_cluster)




# Percentage of spending by category within each cluster
spend_pct_by_category <- spend / colSums(spend)
spend_pct_by_category <- t(spend_pct_by_category)
spend_pct_by_category <- cbind(kmeans_cluster = row.names(spend_pct_by_category), spend_pct_by_category)

# Plotting percentage of spending by category and cluster
ggplot(melt(spend_pct_by_category, id.vars = "cluster"), aes(x = factor(kmeans_cluster), y = value, fill = variable)) +
=======
spending_columns <- c('MntWines', 'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds')
spend <- aggregate(. ~ cluster, data = df_no_outliers[, c(spending_columns, 'cluster')], FUN = sum)
spend <- spend[, -which(names(spend) == "cluster")] 
spend_matrix <- as.matrix(spend)

# Percentage of spending by clusters within each category
spend_pct_by_category <- prop.table(spend_matrix, 1)

# View the percentage data
View(spend_pct_by_category) 

# Plotting percentage of spending by category and cluster
cluster <- c(rep("1" , 6) , rep("2" , 6) , rep("3" , 6) , rep("4" , 6) )
category <- rep(spending_columns , 4)
spend_pct_by_category = as.numeric(spend_pct_by_category )
spending_data <- data.frame(cluster, category, spend_pct_by_category)

ggplot(spending_data, aes(y = spend_pct_by_category, fill = category, x=cluster)) +
>>>>>>> db55d91d629e76f58f6b9eacc355eafaa892c6de
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Percentage of Spending by Category and Cluster") +
  xlab("Cluster") +
  ylab("Percentage")



# Spending platform analysis
spend_platform <- c('NumWebPurchases', 'NumCatalogPurchases', 'NumStorePurchases', 'kmeans_cluster')
spend_platform_df <- aggregate(cbind(NumWebPurchases, NumCatalogPurchases, NumStorePurchases) ~ kmeans_cluster, data = df_no_outliers, FUN = sum)
spend_platform_df <- spend_platform_df[, -ncol(spend_platform_df)]

# Percentage of purchases by platform within each cluster
spend_platform_pct_by_cluster <- t(spend_platform_df / rowSums(spend_platform_df))

# Percentage of purchases by platform across all clusters
spend_platform_pct_overall <- spend_platform_df / colSums(spend_platform_df)
spend_platform_pct_overall <- t(spend_platform_pct_overall)

# Plotting percentage of purchases by platform
barplot(spend_platform_pct_overall, main = "Percentage of Purchases by Platform", xlab = "Platform", col = rainbow(nrow(spend_platform_pct_overall)), legend.text = row.names(spend_platform_pct_overall))



# Web visits by cluster
web_visits_by_cluster <- aggregate(NumWebVisitsMonth ~ cluster, data = df_no_outliers, FUN = sum)
web_visits_by_cluster

# Promotion analysis
promotion_list <- c('NumDealsPurchases', 'AcceptedCmp1', 'AcceptedCmp2', 'AcceptedCmp3', 'AcceptedCmp4', 'AcceptedCmp5', 'Response', 'cluster')
promotion_summary <- aggregate(cbind(NumDealsPurchases, AcceptedCmp1, AcceptedCmp2, AcceptedCmp3, AcceptedCmp4, AcceptedCmp5, Response) ~ cluster, data = df_no_outliers, FUN = sum)
promotion_summary

# Recency distribution and boxplot
ggplot(df_no_outliers, aes(x = Recency, fill = factor(cluster))) + geom_histogram(position = "dodge")
ggplot(df_no_outliers, aes(x = factor(cluster), y = Recency, fill = factor(cluster))) + geom_boxplot()


# Days as customer distribution and barplot
ggplot(df_no_outliers, aes(x = days_customer_for, fill = factor(cluster))) + geom_density(alpha = 0.5)
ggplot(df_no_outliers, aes(x = factor(cluster), y = days_customer_for, fill = factor(cluster))) + geom_bar(stat = "summary", fun = "mean", position = "dodge")




# Define spending categories
spending_category <- c('MntWines', 'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds', 'cluster')

# Calculate sum of spending categories grouped by cluster
spend <- final_data[, spending_category] %>%
  group_by(cluster) %>%
  summarise(across(everything(), sum)) %>%
  t()


rownames(spend)=NULL

spend=spend[-1,]

spend = as.numeric(spend)

spend=matrix(spend,nrow=6,ncol=4,byrow=F)
spend = prop.table(spend,1) 
colnames(spend) = c("1","2","3","4")
rownames(spend) = c('MntWines', 'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds')


# Calculate the row sums
row_sums <- rowSums(spend)

# Divide each element of spend by the row sums
sum_spend <- t(t(spend) / row_sums)

# Reset row names and transpose the result
sum_spend <- as.data.frame(sum_spend)
rownames(sum_spend) <- NULL

# Add cluster index as a column
sum_spend <- cbind(cluster_index = 1:nrow(sum_spend), sum_spend)

# Print the result
print(sum_spend)




# Create a bar plot
ggplot(spend, aes(x = cluster, y = value, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Spending by Category and Cluster",
       x = "Cluster",
       y = "Percentage",
       fill = "Category") +
  theme(legend.position = "bottom")


# Calculate sum of spending categories grouped by cluster
spend1 <- final_data[, cluster] %>%
  group_by(spending_category) %>%
  summarise(across(everything(), sum)) %>%
  t()


rownames(spend)=NULL

spend=spend[-1,]

spend = as.numeric(spend)

spend=matrix(spend,nrow=6,ncol=4,byrow=F)
spend = prop.table(spend,1) 
colnames(spend) = c("1","2","3","4")
rownames(spend) = c('MntWines', 'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds')





