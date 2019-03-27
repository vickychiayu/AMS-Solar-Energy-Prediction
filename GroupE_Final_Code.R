############################### [0] Installing Packages and Reading Documents #####################################################

library(data.table);
library(ggplot2);
install.packages("outliers");
library(outliers);
install.packages("leaflet")
library(leaflet)
install.packages("caret");
library(caret);
library(scales);
install.packages("corrplot");
library(corrplot);
install.packages("e1071");
library(e1071);
install.packages("DT")
library(DT);

folder_path <- "/Users/vickylin/Desktop/MBD/R/workgroup_project";

dt <- readRDS(file.path(folder_path, "solar_dataset.RData"));
dt <- as.data.table(dt);

station_info <- read.table(file.path(folder_path, "station_info.csv"), sep = ",", header = TRUE);
station_info <- as.data.table(station_info);

############################################# [1] EDA #####################################################

#Dataset Division
dt_s1_orig <- as.data.frame(na.omit(dt[, 2:99], invert=FALSE)); #S1 - Station production data
dt_s2_orig <- as.data.frame(dt[1:nrow(dt_s1_orig), 100:456]); #S2 - PCA data (Upper)
dt_s3_orig <- as.data.frame(dt[(1+nrow(dt_s1_orig)):nrow(dt), 100:456]); #S3 - PCA data (Lower)

dim(dt_s3_orig);
################################### [1.1] Step 1: Compute Statistcs #######################################

#S1 - Station production data (col 2-99) from 1994 to 2007
station <- colnames(dt_s1_orig);
mean_station <- sapply(dt_s1_orig, mean, na.rm=TRUE);
sd_station <- sapply(dt_s1_orig, sd, na.rm=TRUE);
quantile_station <- sapply(dt_s1_orig, quantile, na.rm=TRUE);

statistics_s1 <- data.table(Station = c(station),
                 Mean = mean_station,
                 Sd = sd_station,
                 Min = quantile_station[1,],
                 Sec_qrt = quantile_station[2,],
                 Median = quantile_station[3,],
                 Fourth_qrt = quantile_station[4,],
                 Max = quantile_station[5,]
);

statistics_s1;


#S2 - Upper PCA data (col 100-456) from 1994 to 2007
PCA <- colnames(dt_s2_orig);
mean_station <- sapply(dt_s2_orig, mean, na.rm=TRUE);
sd_station <- sapply(dt_s2_orig, sd, na.rm=TRUE);
quantile_station <- sapply(dt_s2_orig, quantile, na.rm=TRUE);

statistics_s2 <- data.table(PCA = c(PCA),
                    Mean = mean_station,
                    Sd = sd_station,
                    Min = quantile_station[1,],
                    Sec_qrt = quantile_station[2,],
                    Median = quantile_station[3,],
                    Fourth_qrt = quantile_station[4,],
                    Max = quantile_station[5,]
);

statistics_s2;


#S3 - Lower PCA data (col 100-456) from 1994 to 2007
PCA <- colnames(dt_s3_orig);
mean_station <- sapply(dt_s3_orig, mean, na.rm=TRUE);
sd_station <- sapply(dt_s3_orig, sd, na.rm=TRUE);
quantile_station <- sapply(dt_s3_orig, quantile, na.rm=TRUE);

statistics_s3 <- data.table(PCA = c(PCA),
                            Mean = mean_station,
                            Sd = sd_station,
                            Min = quantile_station[1,],
                            Sec_quantile = quantile_station[2,],
                            Median = quantile_station[3,],
                            Fourth_quantile = quantile_station[4,],
                            Max = quantile_station[5,]
);

statistics_s3;



################################ [1.2] Step 2: Outlier detection #########################

#Identify outliers by using IQR 1.5 function
dt_s1_orig.iqr <- dt_s1_orig

Outliers <- c()

for(i in 1:98){
  
  max <- quantile(dt_s1_orig.iqr[,i],0.75, na.rm=TRUE) + (IQR(dt_s1_orig.iqr[,i], na.rm=TRUE) * 1.5 )
  min <- quantile(dt_s1_orig.iqr[,i],0.25, na.rm=TRUE) - (IQR(dt_s1_orig.iqr[,i], na.rm=TRUE) * 1.5 )
  
  idx <<- which(dt_s1_orig.iqr[,i] < min | dt_s1_orig.iqr[,i] > max)
  
  print(paste(i, length(idx), sep=''))
  
  Outliers <<- c(Outliers, idx) 
}

Outliers #No Outliers detected





################################### [1.3] Step 3: Statistcal Visualization #########################


################################### [1.3.1] Map ###################################
map <- leaflet();
map <- addTiles(map);

for (i in 1:nrow(station_info)){
  map <- addCircleMarkers(map, lng = as.numeric(station_info[i,3]), lat = as.numeric(station_info[i,2]),
                          label=paste0(colnames(dt_s1_orig[i])," with production mean ",round(statistics_s1[i,2]/1000000, 1),"m"), 
                          labelOptions = labelOptions(noHide = F), radius = 7, 
                          color = if(as.numeric(round(statistics_s1[i,2]/1000000, 1)) < 16) {"red"}
                          else if (as.numeric(round(statistics_s1[i,2]/1000000, 1)) >= 16 & as.numeric(round(statistics_s1[i,2]/1000000, 1)) < 17) {"green"}
                          else {"blue"}
  )};
map;


### CLUSTER (heads)

#BLUE (Mean >17m): GOOD
blue_st <- c()
blue_mean <- c()

for (i in 1:nrow(station_info)){
  if(as.numeric(round(statistics_s1[i,2]/1000000, 1)) > 17){
    blue_st <- matrix(append(blue_st,statistics_s1[i,1]));
    blue_mean <- matrix(append(blue_mean,statistics_s1[i,2]))}
}

blue <- matrix(c(blue_st,blue_mean), ncol=2)
blue <- as.data.table(blue); setnames(blue, c("Station","Mean"))
head(blue)


#GREEN (Mean between 16m and 17m): ACME
green_st <- c()
green_mean <- c()

for (i in 1:nrow(station_info)){
  if(as.numeric(round(statistics_s1[i,2]/1000000, 1)) >= 16 & as.numeric(round(statistics_s1[i,2]/1000000, 1)) < 17){
    green_st <- matrix(append(green_st,statistics_s1[i,1]));
    green_mean <- matrix(append(green_mean,statistics_s1[i,2]))}
}

green <- matrix(c(green_st,green_mean), ncol=2)
green <- as.data.table(green); setnames(green, c("Station","Mean"))
head(green)



#RED (Mean <16m ): STIG
red_st <- c()
red_mean <- c()

for (i in 1:nrow(station_info)){
  if(as.numeric(round(statistics_s1[i,2]/1000000, 1)) < 16){
    red_st <- matrix(append(red_st,statistics_s1[i,1]));
    red_mean <- matrix(append(red_mean,statistics_s1[i,2]))}
}

red <- matrix(c(red_st,red_mean), ncol=2)
red <- as.data.table(red); setnames(red, c("Station","Mean"))
head(red)


################################### [1.3.2] Visualization of Statistics ########

#S1 - Visualization of solar panel statistics

#Mean
means_plot <- ggplot(statistics_s1, aes(x = statistics_s1$Station, y = statistics_s1$Mean));
means_plot <- means_plot + geom_point(color = 'red');
means_plot <- means_plot + ggtitle("Mean of Production by Station") + xlab("Station");
means_plot <- means_plot + theme(plot.title = element_text(face = "bold"));
means_plot <- means_plot + scale_y_continuous(name = "Mean (millions)", 
                                              breaks = waiver(), 
                                              labels = c("15","16","17","18","19"));
means_plot <- means_plot + theme(axis.text.x=element_text(angle = 90, vjust = 0.5, size = 5));
means_plot <- means_plot + geom_text(aes(label = ifelse(Mean > 18000000 | Mean < 15000000, as.character(Station),'')),
                                     hjust = 0, 
                                     vjust = 0, 
                                     size = 3);
means_plot <- means_plot + theme(panel.background = element_rect(fill = 'white'),
                                 panel.grid.minor = element_blank(),
                                 panel.grid.major = element_line(color = "black", linetype = "dotted"),
                                 panel.grid.major.x = element_blank(),
                                 axis.line = element_line(colour = "black"));
means_plot;



#Standard deviation
sd_plot <- ggplot(statistics_s1, aes(x = statistics_s1$Station, y = statistics_s1$Sd));
sd_plot <- sd_plot + geom_point(color = 'red');
sd_plot <- sd_plot + ggtitle("Standard Deviation of Production by Station") + xlab("Station");
sd_plot <- sd_plot + theme(plot.title = element_text(face = "bold"));
sd_plot <- sd_plot + scale_y_continuous(name = "Standard Deviation (millions)", 
                                        breaks = waiver(), 
                                        labels = c("7.5","7.6","7.7","7.8","7.9","8.0","8.1"));
sd_plot <- sd_plot + theme(axis.text.x=element_text(angle = 90, vjust = 0.5, size = 5));
sd_plot <- sd_plot + geom_text(aes(label = ifelse(Sd > 8100000 | Sd < 7600000, as.character(Station),'')),
                               hjust = 0, 
                               vjust = 0, 
                               size = 3);
sd_plot <- sd_plot + theme(panel.background = element_rect(fill = 'white'),
                           panel.grid.minor = element_blank(),
                           panel.grid.major = element_line(color = "black", linetype = "dotted"),
                           panel.grid.major.x = element_blank(),
                           axis.line = element_line(colour = "black"));
sd_plot;


#S1 - Density
plot(density(10^(-7)*dt_s1_orig[,1]), 
     main = "Density distribution of Station's solar production", ylim =c(0,0.6),
     xlab = "Production per station in Joules (*10^7)");

for (i in 2:ncol(dt_s1_orig)){
  lines(density(10^(-7)*dt_s1_orig[, i]),col=i);
};


#S2 - Density
plot(density(dt_s2_orig[,1]),
     main = "Density of Each PCI \n From 1994 to 2007",
     xlab = "Distribution of PCA values",xlim=c(-60,60),ylim=c(0,0.2));
for (i in 1:ncol(dt_s2_orig)){
  lines(density(dt_s2_orig[, i]),col=i);
};


#S3 - Density
plot(density(dt_s3_orig[,1]),
     main = "Density of Each PCI \n From 2008 to 2012-11-30",
     xlab = "Distribution of PCA values",xlim=c(-60,60),ylim=c(0,0.2));
for (i in 1:ncol(dt_s3_orig)){
  lines(density(dt_s3_orig[, i]), col = i)};





################################ [1.4] Step 4: Data scaling #########################

tipify <- function(x){
  mu <- mean(x, na.rm = TRUE);
  s <- sd(x, na.rm = TRUE);
  s[s == 0] <- 1;
  x <- (x - mu) / s;
}


df_s1_standard <- as.data.frame(sapply(dt_s1_orig,tipify));
df_s2_standard <- as.data.frame(sapply(dt_s2_orig,tipify));
df_s3_standard <- as.data.frame(sapply(dt_s3_orig,tipify));

plot(density(df_s1_standard[,1]),ylim=c(0,0.5), main = "Density of tipified solar panels' production",
     xlab = "Distribution of scaled values");
for (i in 2:ncol(df_s1_standard)){
  lines(density(df_s1_standard[, i]),col=i)
}

plot(density(df_s2_standard[,1]),ylim=c(0,0.7),main = "Density of tipified predictors' values",
     xlab = "Distribution of scaled values");
for (i in 1:ncol(df_s2_standard)){
  lines(density(df_s2_standard[, i]),col=i)
}

plot(density(df_s3_standard[,1]),ylim=c(0,0.7),main = "Density of tipified predictors' values",
     xlab = "Distribution of scaled values");
for (i in 1:ncol(df_s3_standard)){
  lines(density(df_s3_standard[, i]),col=i)
}




####################### [1.5] Step 5: General correlation ####################################################

#Correlations between solar panels and PCA
variable <- c();

for(iterator in 1:98){
  corr<-(as.data.table(cor(dt_s1_orig[,iterator],dt_s2_orig)));
  variable <- c(variable, max.col(corr))
}
variable; #PCA 1 with highest correlation
length(variable);

corr_main <- as.data.table(statistics_s1[,1])
corr_main$Corr_PCA <- variable
corr_main

#Station to PCA table
input_corr<-cor(x=dt_s1_orig,y=df_s2_standard);
corrplot(input_corr[1:98,1:98],tl.cex = 0.3);




#Correlation between explanatory variables themselves
corr_PCA<-cor(dt_s2_orig);

#Remove the own correlation of variables
for (i in 1:length(colnames(corr_PCA))){
  corr_PCA[i,i]<-0
}
#obtain vector with the highest correlation per PCA to all other PCAs
max_corr_per_PCA<-c();
for (i in 1:length(colnames(corr_PCA))){
  a<-abs(max(abs(corr_PCA[,i])));
  max_corr_per_PCA<-append(max_corr_per_PCA,a)
};

max(max_corr_per_PCA);

#PCA to PCA table
input_corr_PCA<-cor(dt_s2_orig);
corrplot(input_corr_PCA[1:100,1:100],order = "hclust",tl.cex = 0.3);

#The highest absolute value of correlation coeficient of each PCA to other PCAs is 0,072.
#Not enough evidence of correlation using this approach to make some PCAs redundant.





####################### [1.6] Step 6: Visualization of correlation ########################################

#histogram for solar panel to PCA by frequency
variable_df <- as.data.frame(variable);
hist_fq <- ggplot(variable_df, aes(variable)) + geom_histogram(aes(y=(..count..)), binwidth = 1, color = 'white');
hist_fq <- hist_fq + ggtitle("Most Correlated PCA") + xlab("PCA") + ylab("Frequency (Sum = 98)"); #add titles
hist_fq <- hist_fq + theme(plot.title = element_text(face = "bold")); #change the face of main title
hist_fq <- hist_fq + theme(panel.grid.minor = element_blank(), 
                           panel.grid.major.x = element_blank(),
                           axis.line = element_line(colour = "black")); #remove unnecessary grid lines.
hist_fq <- hist_fq + scale_x_continuous(breaks = c(1,2),
                                        labels = c("PC1","PC2"));
hist_fq <- hist_fq + stat_bin(binwidth= 1, geom="label", aes(label=..count..));
hist_fq;

#Histogram representing the maximum correlation coeficient for each PCA to all other PCAs
max_corr_per_PCA_df <- as.data.frame(max_corr_per_PCA);
hist_cor <- ggplot(max_corr_per_PCA_df, aes(max_corr_per_PCA)) + geom_histogram(aes(y=(..count..)/sum(..count..)), binwidth = 0.005, color = 'white');
hist_cor <- hist_cor + ggtitle("PCA Correlation") + xlab("Maximum Correlation Coefficient Value") + ylab("Percentage of Total 357 PCA"); #add titles
hist_cor <- hist_cor + theme(plot.title = element_text(face = "bold")); #change the face of main title
hist_cor <- hist_cor + theme(panel.grid.minor = element_blank(), 
                             panel.grid.major.x = element_blank(),
                             axis.line = element_line(colour = "black")); #remove unnecessary grid lines.
hist_cor <- hist_cor + scale_y_continuous(labels = percent_format());
hist_cor;



####################### [1.7] Step 7: Dimensionality reduction ####################################################
help("filterVarImp");

select_important <- function(dat, n_vars, y){
  varimp <- filterVarImp(x = dat, y=y, nonpara=TRUE);
  varimp <- data.table(variable=rownames(varimp),imp=varimp[, 1]);
  varimp <- varimp[order(-imp)];
  selected <- varimp$variable[1:n_vars];
  return(selected);
};

#BLUE group
select_important(dat = dt_s2_orig, n_vars = 10, y = dt_s1_orig$GOOD);

#GREEN group
select_important(dat = dt_s2_orig, n_vars = 10, y = dt_s1_orig$ACME);

#RED group
select_important(dat = dt_s2_orig, n_vars = 10, y = dt_s1_orig$STIG);


####################### [2] Modelling and prediction ####################################################

#define dataset for training
df_s1_2_std<-cbind(dt_s1_orig,df_s2_standard);
dim(df_s1_2_std);
#this dataset is compesed by the original values of the solar panels and the tipified
#values of the PCA variables.
# row indices for training data, the first (70%) rows. Oldest records. We don't randomize the training set
# since time could by relevant for the analysis.
train_index<-1:(0.7*nrow(df_s1_2_std));
tail(train_index,2);
# row indices for validation data (15%)
val_index<-(ceiling(0.7*nrow(df_s1_2_std))):(floor(0.85*nrow(df_s1_2_std)));
# row indices for test data (15%). Most recently recorded data 
test_index <- (ceiling(0.85*nrow(df_s1_2_std))):(nrow(df_s1_2_std));

#Let's work with data tables
dt_s1_2_st<-as.data.table(df_s1_2_std);


# split data for training and validation
train <- dt_s1_2_st[train_index,]; 
val <- dt_s1_2_st[val_index,]; 
test  <- dt_s1_2_st[test_index,];

#Define the dataset that will be used for prediction
pred<- df_s3_standard;

# The mean used to tipify values of PCA is the total 5k rows mean, so 
#we understand we are not "cheating" with the cross-validation of our results, since the mean
#used for validation and test datasets is the total 5k rows mean for each PCA.

#Check dimension of datasets
dim(df_s1_2_std);
dim(train);
dim(val);
dim(test);
dim(pred);

#let's train a Linear Model for ACME, the first train station

formula<-paste0(colnames(dt_s1_orig[1])," ~ ", paste0(colnames(dt_s2_orig), collapse = " + "));

lm_model<-lm(formula, data = train);


# Get model predictions
predictions_train <- predict(lm_model, newdata = train);
predictions_test <- predict(lm_model, newdata = test);

# Get errors
errors_train <- predictions_train - train$ACME;
errors_test <- predictions_test - test$ACME;

# Compute Metrics
mse_train <- round(mean(errors_train^2), 2);
mae_train <- round(mean(abs(errors_train)), 2);

mse_test <- round(mean(errors_test^2), 2);
mae_test <- round(mean(abs(errors_test)), 2);

# Build comparison table
comp_lm_ACME<-c();
comp_lm_ACME <- rbind(comp_lm_ACME,
                      data.table(model = c("lm_allvar"), 
                                 mse_train = mse_train, mae_train = mae_train,
                                 mse_test = mse_test, mae_test = mae_test));
comp_lm_ACME; 

#we get an mae of 2130504, seems accpetable comparing to the kaggle results.

#Create the data table where we will add predictions, do far it has only the dates
predicted<-dt[(1+nrow(dt_s2_orig)):6909,1];
dim(predicted);
head(maetable);

for (i in 1:ncol(dt_s1_orig)) {
  print(paste0("Starting model for variable nr. ",i));
  lm_model<-lm(paste0(colnames(dt_s1_orig[i])," ~ ",paste0(colnames(dt_s2_orig), collapse = " + ")),data=train);
  predictions_pred <-data.table(predict(lm_model, newdata = pred));
  colnames(predictions_pred)<-colnames(dt_s1_orig[i]);
  predicted<-cbind(predicted,predictions_pred);
};

predicted; #is the dataset for submission

#let's export it to upload it to kaggle
write.csv(predicted,file="lm_model.csv",row.names = FALSE);


#We will fine-tune 3 svm models according to the groups of solar panels relating to mean values of production:
# GREEN, BLUE and RED

### GRID SEARCH FOR GREEN MODEL

c_values <- 1;
eps_values <- 10^seq(from = -2, to = 0, by = 0.5);
gamma_values <- 0.001;

#this is he result of the zoomed-in grid search. We starter with ranges -3 to 3 by 0.5 for cost and gamma

formula<-as.formula(paste0(colnames(dt_s1_orig[1])," ~ ", paste0(colnames(dt_s2_orig), collapse = " + ")));

grid_results_ACME <- data.table();

for (c in c_values){
  for (eps in eps_values){
    for (gamma in gamma_values){
      
      print(sprintf("Start of c = %s - eps = %s - gamma = %s", c, eps, gamma));
      
      # train SVM model with a particular set of hyperparamets
      model <- svm(formula,data = train, kernel="radial",
                   cost = c, epsilon = eps, gamma = gamma);
      
      # Get model predictions
      predictions_train <- predict(model, newdata = train);
      predictions_val <- predict(model, newdata = val);
      
      # Get errors
      errors_train <- predictions_train - train$ACME;
      errors_val <- predictions_val - val$ACME;
      
      # Compute Metrics
      mse_train <- round(mean(errors_train^2), 2);
      mae_train <- round(mean(abs(errors_train)), 2);
      
      mse_val <- round(mean(errors_val^2), 2);
      mae_val <- round(mean(abs(errors_val)), 2);
      
      grid_results_ACME <- rbind(grid_results_ACME,
                                 data.table(c = c, eps = eps, gamma = gamma, 
                                            mse_train = mse_train, mae_train = mae_train,
                                            mse_val = mse_val, mae_val = mae_val));
    }
  }
}


# Order results by increasing mse and mae
grid_results_ACME <- grid_results_ACME[order(mse_val, mae_val)];
head(grid_results_ACME,20);

# Check results
best_ACME <- grid_results_ACME[4]; #given more consistency betwee train and validation
best_ACME; 

#selected c=1, eps= 0.3162278, gamma=0.001

model <- svm(formula,data = train, kernel="radial",
             cost = best_ACME$c, epsilon = best_ACME$eps, gamma = best_ACME$gamma);


#Get predictions
predictions_train <- predict(model, newdata = train);
predictions_val <- predict(model, newdata = val);
predictions_test <- predict(model, newdata = test);


# Get errors
errors_train <- predictions_train - train$ACME;
errors_val <- predictions_val - val$ACME;
errors_test <- predictions_test - test$ACME;

# Compute Metrics
mse_train <- round(mean(errors_train^2), 2);
mae_train <- round(mean(abs(errors_train)), 2);

mse_val <- round(mean(errors_val^2), 2);
mae_val <- round(mean(abs(errors_val)), 2);

mse_test <- round(mean(errors_test^2), 2);
mae_test <- round(mean(abs(errors_test)), 2);

# Build comparison table
comp_ACME<-c();
comp_ACME <- rbind(comp_ACME,
                   data.table(model = c("optimized_svm_ACME"), 
                              mse_train = mse_train, mae_train = mae_train,
                              mse_test = mse_test, mae_test = mae_test));
comp_ACME; #Similar result for the test dataset

model_green<-model; #we save the model for the final predictions


### GRID SEARCH FOR RED STATIONS MODEL
grid_results_STIG <- data.table();

formula<-as.formula(paste0("STIG"," ~ ", paste0(colnames(dt_s2_orig), collapse = " + ")));

c_values <- 1;
eps_values <- 10^seq(from = -2, to = 0, by = 0.2);
gamma_values <- 10^-3;

for (c in c_values){
  for (eps in eps_values){
    for (gamma in gamma_values){
      
      print(sprintf("Start of c = %s - eps = %s - gamma = %s", c, eps, gamma));
      
      # train SVM model with a particular set of hyperparamets
      model <- svm(formula, data = train, kernel="radial",
                   cost = c, epsilon = eps, gamma = gamma);
      
      # Get model predictions
      predictions_train <- predict(model, newdata = train);
      predictions_val <- predict(model, newdata = val);
      
      # Get errors
      errors_train <- predictions_train - train$STIG;
      errors_val <- predictions_val - val$STIG;
      
      # Compute Metrics
      mse_train <- round(mean(errors_train^2), 2);
      mae_train <- round(mean(abs(errors_train)), 2);
      
      mse_val <- round(mean(errors_val^2), 2);
      mae_val <- round(mean(abs(errors_val)), 2);
      
      grid_results_STIG <- rbind(grid_results_STIG,
                                 data.table(c = c, eps = eps, gamma = gamma, 
                                            mse_train = mse_train, mae_train = mae_train,
                                            mse_val = mse_val, mae_val = mae_val));
    }
  }
}


# Order results by increasing mse and mae
grid_results_STIG <- grid_results_STIG[order(mse_val, mae_val)];
head(grid_results_STIG,20);

# Check results
best_STIG <- grid_results_STIG[8];
best_STIG;

#c  eps gamma    mse_train mae_train      mse_val mae_val
#1: 1 0.10 0.001 6.605042e+12   1701998 1.061341e+13 2378607

model <- svm(formula,
             data = train, kernel="radial",
             cost = best_STIG$c, epsilon = best_STIG$eps, gamma = best_STIG$gamma);

#Get predictions
predictions_train <- predict(model, newdata = train);
predictions_val <- predict(model, newdata = val);
predictions_test <- predict(model, newdata = test);


# Get errors
errors_train <- predictions_train - train$STIG;
errors_val <- predictions_val - val$STIG;
errors_test <- predictions_test - test$STIG;

# Compute Metrics
mse_train <- round(mean(errors_train^2), 2);
mae_train <- round(mean(abs(errors_train)), 2);

mse_val <- round(mean(errors_val^2), 2);
mae_val <- round(mean(abs(errors_val)), 2);

mse_test <- round(mean(errors_test^2), 2);
mae_test <- round(mean(abs(errors_test)), 2);

# Build comparison table
comp_STIG<-c();
comp_STIG <- rbind(comp_STIG,
                   data.table(model = c("optimized_svm_STIG"), 
                              mse_train = mse_train, mae_train = mae_train,
                              mse_test = mse_test, mae_test = mae_test));
comp_STIG;
#we call this model model_red, as we will aply it to our group of lower mean solar panels
model_red<-model;


### GRID SEARCH FOR GOOD MODEL
#We change the range ofn the parameter to optimize as the previous one was not giving a satisfactory result
c_values <- 1;
eps_values <- 10^seq(from = -2, to = 0, by = 0.25);
gamma_values <- 0.001;

formula<-as.formula(paste0("GOOD"," ~ ", paste0(colnames(dt_s2_orig), collapse = " + ")));

grid_results_GOOD <- data.table();

for (c in c_values){
  for (eps in eps_values){
    for (gamma in gamma_values){
      
      print(sprintf("Start of c = %s - eps = %s - gamma = %s", c, eps, gamma));
      
      # train SVM model with a particular set of hyperparamets
      model <- svm(formula
                   , data = train, kernel="radial",
                   cost = c, epsilon = eps, gamma = gamma);
      
      # Get model predictions
      predictions_train <- predict(model, newdata = train);
      predictions_val <- predict(model, newdata = val);
      
      # Get errors
      errors_train <- predictions_train - train$GOOD;
      errors_val <- predictions_val - val$GOOD;
      
      # Compute Metrics
      mse_train <- round(mean(errors_train^2), 2);
      mae_train <- round(mean(abs(errors_train)), 2);
      
      mse_val <- round(mean(errors_val^2), 2);
      mae_val <- round(mean(abs(errors_val)), 2);
      
      grid_results_GOOD <- rbind(grid_results_GOOD,
                                 data.table(c = c, eps = eps, gamma = gamma, 
                                            mse_train = mse_train, mae_train = mae_train,
                                            mse_val = mse_val, mae_val = mae_val));
    }
  }
}


# Order results by increasing mse and mae
grid_results_GOOD <- grid_results_GOOD[order(mse_val, mae_val)];
head(grid_results_GOOD,20);
# Check results
best_GOOD <- grid_results_GOOD[1];
best_GOOD;

model <- svm(formula,
             data = train, kernel="radial",
             cost = best_GOOD$c, epsilon = best_GOOD$eps, gamma = best_GOOD$gamma);

#Get predictions
predictions_train <- predict(model, newdata = train);
predictions_val <- predict(model, newdata = val);
predictions_test <- predict(model, newdata = test);


# Get errors
errors_train <- predictions_train - train$GOOD;
errors_val <- predictions_val - val$GOOD;
errors_test <- predictions_test - test$GOOD;

# Compute Metrics
mse_train <- round(mean(errors_train^2), 2);
mae_train <- round(mean(abs(errors_train)), 2);

mse_val <- round(mean(errors_val^2), 2);
mae_val <- round(mean(abs(errors_val)), 2);

mse_test <- round(mean(errors_test^2), 2);
mae_test <- round(mean(abs(errors_test)), 2);

# Build comparison table
comp_GOOD<-c();
comp_GOOD <- rbind(comp_GOOD,
                   data.table(model = c("optimized_svm_GOOD"), 
                              mse_train = mse_train, mae_train = mae_train,
                              mse_test = mse_test, mae_test = mae_test));
comp_GOOD;

#We call this model model_blue, as we will use it to predict production in stations with medium mean
model_blue<-model;


#Lest compare our parameters
parameters<-rbind(best_ACME,best_GOOD,best_STIG);
parameters[,1:3];



###Get predictions ###

total_train<-dt_s1_2_st
dim(total_train);
pred_comb<-dt[(1+nrow(dt_s1_orig)):6909,1];
dim(pred_comb);

for (i in 1:ncol(dt_s1_orig)){
  formula<-as.formula(paste0(colnames(dt_s1_orig[i])," ~ ", paste0(colnames(dt_s2_orig), collapse = " + ")));
  if (sum(colnames(dt_s1_orig[i]) == green$Station)>0) {
    print(paste0("Start training model for station ",colnames(dt_s1_orig[i]), ", part of group: GREEN"));
    model_green<-svm(formula, data = train, kernel="radial",
                     cost = best_ACME$c, epsilon = best_ACME$eps, gamma = best_ACME$gamma);
    predictions<-data.table(predict(model_green,newdata = pred));
  } else if (sum(colnames(dt_s1_orig[i]) == blue$Station)>0) {
    print(paste0("Start training model for station ",colnames(dt_s1_orig[i]), ", part of group: BLUE"));
    model_green<-svm(formula, data = train, kernel="radial",
                     cost = best_GOOD$c, epsilon = best_GOOD$eps, gamma = best_GOOD$gamma);
    predictions<-data.table(predict(model_green,newdata = pred));
  } else {
    print(paste0("Start training model for station ",colnames(dt_s1_orig[i]), ", part of group: RED"));
    model_green<-svm(formula, data = train, kernel="radial",
                     cost = best_STIG$c, epsilon = best_STIG$eps, gamma = best_STIG$gamma);
    predictions<-data.table(predict(model_green,newdata = pred));
  }
  colnames(predictions)<-colnames(dt_s1_orig[i]);
  pred_comb<-cbind(pred_comb,predictions);
};



dim(pred_comb);
head(pred_comb,1);

write.csv(pred_comb,file="svm_model.csv",row.names = FALSE);


