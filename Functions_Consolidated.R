# Function 1: NULLremover-------------------------------------------------------------

NULLremover <- function(data,perc) {
  a <- lapply(data, function(x) sum(is.na(x))/length(x)) %>% unlist()
  ind <- which(a<perc)
  return(data[,ind])
}


# Function 2: NoSdRemover-------------------------------------------------------------

SDremover <- function(data,perc) {
  a <- lapply(data, function(x) sd(as.numeric(x),na.rm=T)/
                (max(as.numeric(x),na.rm=T)-min(as.numeric(x),na.rm = T))) %>%
    unlist()
  ind <- which(a>perc)
  return(data[,ind])
}

# Function 3: CategoryToOther-------------------------------------------------------------

#Function taking a column and replacing all categories with a Frequency less than
#thres with the category "Other" - in case of numeric variables no change.
categoryToOther <- function(col,thres) {
  check <- 0
  out <- col
  if(is.factor(col)) {
    t <- table(col)
    r <- which(t<thres)
    for(i in seq(1,max(1,length(r)),100)) {
      if(length(r)-i>=100) {
        out <- gsub(paste(names(r)[i:(i+99)],collapse="|"),"other",out) %>% factor()
      }
      else {
        out <- gsub(paste(names(r)[i:length(r)],collapse="|"),"other",out) %>% factor()
      }
    }
    check <- 1
  } 
  cat(check) #just to see how far the process got
  return(out)
}
#for applying this to a dataframe df call
#df <- df %>% lapply(function(x) categoryToOther(x,50)) %>% data.frame()

# Function 4: binaryEncoding-------------------------------------------------------------

#
binaryEncoding <- function(col) {
  if(is.factor(col)) {
    col_num <- col %>% as.numeric() %>% intToBits() %>% 
      as.integer() %>% matrix(ncol=length(col)) %>% t() %>% data.frame() 
    ind <- col_num %>% apply(2,function(x) all(x==0)) %>% {which(.==FALSE)} %>% 
      max()
    return(col_num[,1:ind])
  }
  else return(col)
}
#for applying this to a dataframe df call
#df <- df %>% lapply(function(x) x <- binaryEncoding(x)) %>% data.frame()

# Function 5: OneHotEncoder-------------------------------------------------------------

#library(fastDummies)

OneHotEncoder <- function(df) {
  
  dummy_cat %>% dummy_cols(df, select_columns = NULL, remove_first_dummy = FALSE,
                           remove_most_frequent_dummy = FALSE)
  return(dummy_cat)
}

# Function 6: BasicReplace-------------------------------------------------------------

#Replacing: If numeric (Mean). If character: "Missing"
BasicReplace <- function(df) {
  
  NoNa <- df %>% mutate_if(is.numeric, funs(ifelse(is.na(.), mean(.,na.rm = T),.))) %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.character,funs(factor(ifelse(is.na(.), "missing", .))))
  
  return(NoNa)
}

# Function 7: Principal Component Analysis (PCA)-------------------------------------------------------------

# Function 8: Multiple Correspondance Analysis (MCA)-------------------------------------------------------------

# Function 9: Hierarchical Clustering (HC)-------------------------------------------------------------

cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}


HC_Divisive <- function(df,n) {
  #----- Dissimilarity Matrix -----#
  library(cluster) 
  # to perform different types of hierarchical clustering
  # package functions used: daisy(), diana(), clusplot()
  gower.dist <- daisy(df, metric = c("gower"))
  # class(gower.dist) 
  ## dissimilarity , dist
  
  #------------ DIVISIVE CLUSTERING ------------#
  divisive.clust <- diana(as.matrix(gower.dist), 
                          diss = TRUE, keep.diss = TRUE)
  #plot(divisive.clust, main = "Divisive")
  
  stats.df.divisive <- cstats.table(gower.dist, divisive.clust, n)
  
  # --------- Choosing the number of clusters ---------#
  # Using "Elbow" and "Silhouette" methods to identify the best number of clusters
  # to better picture the trend, I will go for more than 7 clusters.
  library(ggplot2)
  # Elbow
  elbow <- ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, n+10))), 
         aes(x=cluster.number, y=within.cluster.ss)) + 
    geom_point()+
    geom_line()+
    ggtitle("Divisive clustering") +
    labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
    theme(plot.title = element_text(hjust = 0.5))
  return(elbow)
  
  # Silhouette
  silhouette <-ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, n+10))), 
         aes(x=cluster.number, y=avg.silwidth)) + 
    geom_point()+
    geom_line()+
    ggtitle("Divisive clustering") +
    labs(x = "Num.of clusters", y = "Average silhouette width") +
    theme(plot.title = element_text(hjust = 0.5))
  return(silhouette)
}

HC_Agglomerative <- function(df,n) {
  #library(cluster) 
  # to perform different types of hierarchical clustering
  # package functions used: daisy(), diana(), clusplot()
  gower.dist <- daisy(df, metric = c("gower"))
  # class(gower.dist) 
  ## dissimilarity , dist
  
  #------------ AGGLOMERATIVE CLUSTERING ------------#
  # I am looking for the most balanced approach
  # Complete linkages is the approach that best fits this demand - I will leave only this one here, don't want to get it cluttered
  aggl.clust.c <- hclust(gower.dist, method = "complete")
  #plot(aggl.clust.c, main = "Agglomerative, complete linkages")
  
  #Choosing n as the number of clusters
  stats.df.aggl <-cstats.table(gower.dist, aggl.clust.c, n)
  
  # --------- Choosing the number of clusters ---------#
  # Using "Elbow" and "Silhouette" methods to identify the best number of clusters
  # to better picture the trend, I will go for more than 7 clusters.
  library(ggplot2)
  # Elbow
  elbow <-ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, n+10))), 
         aes(x=cluster.number, y=within.cluster.ss)) + 
    geom_point()+
    geom_line()+
    ggtitle("Agglomerative clustering") +
    labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
    theme(plot.title = element_text(hjust = 0.5))
  return(elbow)
  
  #Silhouette
  silhouette <-ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, n+10))), 
         aes(x=cluster.number, y=avg.silwidth)) + 
    geom_point()+
    geom_line()+
    ggtitle("Agglomerative clustering") +
    labs(x = "Num.of clusters", y = "Average silhouette width") +
    theme(plot.title = element_text(hjust = 0.5))
  return(silhouette)
}

#link_Reference: https://medium.com/@anastasia.reusova/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995

# Function 10: K-Means Clustering-------------------------------------------------------------

# Function 11: Correlation Filter-------------------------------------------------------------
# calculate correlation matrix
CorrFilter <- function(df, Factor) {
  correlationMatrix <- cor(df)
  #anyNA(correlationMatrix)
  CorrMatrix <- round(correlationMatrix, 2)
  #library(corrplot)
  Corrplotting <-corrplot(CorrMatrix, order = "hclust", tl.cex = 0.2) #tl.cex for the variable text size
  return(Corrplotting)
  highlyCorrelated <- findCorrelation(CorrMatrix, cutoff=Factor)
  NoCorrelated <- df[,-c(highlyCorrelated)]
  return(NoCorrelated)
}


# Function 12: Mice (Num, Cat)-------------------------------------------------------------

#library(mice)
Mice_Imputation <- function(df) {
  imp_cat <- parlmice(df, method = 'rf' ,seed = 123)  # perform mice imputation, based on random forests.
  Mice_Imputed <- complete(imp_cat)  # generate the completed data.
  #anyNA(train_cat_DS)
  return(Mice_Imputed)
}


# Function 13: Sampling -------------------------------------------------------------

#library(caret)

Sampled <- function(df,Method) {
  
  if(Method == "Up") {  Sampled<-upSample(df) }
  else if(Method == "Down") { Sampled<-downSample(df)  }
  else {
    Sampled <- df
  }
  return(Sampled)
}


# Function 14: Variable Importance (VarImp)-------------------------------------------------------------

# Function 15: Paralelling Processing-------------------------------------------------------------

library(doParallel)
cl <- makePSOCKcluster(3)
registerDoParallel(cl)

## All subsequent models are then run in parallel
model_rf <- train( Churn ~ ., data = clean_df, method = "rf")

## When you are done:
stopCluster(cl)