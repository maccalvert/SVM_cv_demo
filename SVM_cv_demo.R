library(e1071)
library(ggplot2)

n <- 5000
set.seed(10111)
x <- matrix(rnorm(n*2), ncol = 2)
x[1:round(n/3),] <- x[1:round(n/3),] + 2
x[(round(n/3)+1):(round(n/3)*2),2] <- x[(round(n/3)+1):(round(n/3)*2),2] - 2
x[(round(n/3)+1):(round(n/3)*2),1] <- x[(round(n/3)+1):(round(n/3)*2),1] + 2
y <- c(rep(1,round(n/3,)*2), rep(-1,round(n/3)-1))
dat <- data.frame(x=x,y=as.factor(y))

# Plot data
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

#plot subsets 
samp <- dat[sample(nrow(dat),1000),]
ggplot(data = samp, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

samp <- dat[sample(nrow(dat),4000),]
ggplot(data = samp, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

#One off test of the 5 fold cv
samp <- base::sample(nrow(dat),4000)
svmfit = svm(y ~ ., data = dat[samp,], kernel = "linear", cost = 10, scale = FALSE)
tab_train <- as.data.frame(table(true = dat[samp,"y"], pred=predict(svmfit, dat[samp,])))
tab_test <- as.data.frame(table(true = dat[-samp,"y"], pred=predict(svmfit, dat[-samp,])))

acc_diff <- (tab_train$Freq[1]+tab_train$Freq[4])/sum(tab_train$Freq)-(tab_test$Freq[1]+tab_test$Freq[4])/sum(tab_test$Freq)
acc_diff

#specifcity 
tab_train$Freq[1]/(tab_train$Freq[3]+tab_train$Freq[1])
tab_test$Freq[1]/(tab_test$Freq[3]+tab_test$Freq[1])

#sensitivity 
tab_train$Freq[4]/(tab_train$Freq[2]+tab_train$Freq[4])
tab_test$Freq[4]/(tab_test$Freq[2]+tab_test$Freq[4])

#Ok, now for 1000 iterations 
cv <- function(nit, samp_size, kernel_func){
  sens <- vector()
  spec <- vector()
  acc <- vector()
  #demonstrate how the larger the training data set the lower the risk. 
  for(i in 1:nit){
    train <- base::sample(n,samp_size, replace = FALSE)
    svmfit <- svm(y~., data = dat[train,], kernel = kernel_func, cost = 10)
    tab_train <- as.data.frame(table(true = dat[train,"y"], pred=predict(svmfit, dat[train,])))
    tab_test <- as.data.frame(table(true = dat[-train,"y"], pred=predict(svmfit, dat[-train,])))
    sens[i] <- (tab_train$Freq[4]/(tab_train$Freq[2]+tab_train$Freq[4])) - (tab_test$Freq[4]/(tab_test$Freq[2]+tab_test$Freq[4]))
    spec[i] <- (tab_train$Freq[1]/(tab_train$Freq[3]+tab_train$Freq[1])) - (tab_test$Freq[1]/(tab_test$Freq[3]+tab_test$Freq[1]))
    acc[i] <- ((tab_train$Freq[1]+tab_train$Freq[4])/sum(tab_train$Freq)) - ((tab_test$Freq[1]+tab_test$Freq[4])/sum(tab_test$Freq))
  }
  error_ests <- data.frame(sensitivity = sens, specificity = spec, accuracy = acc)
  return(error_ests)
}

a <- Sys.time()
lin4000 <- cv(nit=1000, samp_size = 4000, kernel_func = "linear")
a - Sys.time()
#Histograms of accuracy
ggplot(data=lin4000, aes(x=accuracy))+
  geom_histogram(alpha=0.5, position="identity", binwidth = 0.0025,  color ="#00BFC4", fill ="#00BFC4")
#Histograms of sensitivity
ggplot(data=lin4000, aes(x=sensitivity))+
  geom_histogram(alpha=0.5, position="identity", binwidth = 0.0025, color ="#F8766D", fill ="#F8766D")
#Histograms of specificity
ggplot(data=lin4000, aes(x=specificity))+
  geom_histogram(alpha=0.5, position="identity", binwidth = 0.0025, color ="#7CAE00", fill ="#7CAE00")

#Ok what about messing with sample size. 
# sample size of 10 
lin10 <- cv(nit=1000, samp_size = 25, kernel_func = "linear")
# sample size of 100
lin100 <- cv(nit=1000, samp_size = 100, kernel_func = "linear")
# sample size of 1000
lin1000 <- cv(nit=1000, samp_size = 1000, kernel_func = "linear")

lin_accuracy <- data.frame(samp_size = c(rep(25,1000), rep(100,1000), rep(1000,1000), rep(4000,1000)),
                           accuracy = c(lin10$accuracy, lin100$accuracy, lin1000$accuracy, lin4000$accuracy))
lin_accuracy$samp_size <- as.character(lin_accuracy$samp_size)
ggplot(data=lin_accuracy, aes(x=accuracy, color=samp_size, fill=samp_size))+
  geom_histogram(alpha=0.5, position="identity",binwidth = 0.01)

#Sensitivity
lin_sensitivity <- data.frame(samp_size = c(rep(25,1000), rep(100,1000), rep(1000,1000), rep(4000,1000)),
                           sensitivity = c(lin10$sensitivity, lin100$sensitivity, lin1000$sensitivity, lin4000$sensitivity))
lin_sensitivity$samp_size <- as.character(lin_sensitivity$samp_size)
ggplot(data=lin_sensitivity, aes(x=sensitivity, color=samp_size, fill=samp_size))+
  geom_histogram(alpha=0.5, position="identity",binwidth = 0.01)

#Specificity
lin_specificity <- data.frame(samp_size = c(rep(25,1000), rep(100,1000), rep(1000,1000), rep(4000,1000)),
                              specificity = c(lin10$specificity, lin100$specificity, lin1000$specificity, lin4000$specificity))
lin_specificity$samp_size <- as.character(lin_specificity$samp_size)
ggplot(data=lin_specificity, aes(x=specificity, color=samp_size, fill=samp_size))+
  geom_histogram(alpha=0.5, position="identity",binwidth = 0.025)


#Now test against the radial classifier
rad4000 <- cv(nit=1000, samp_size = 4000, kernel_func = "radial")

#accuracy
comp_dat <- data.frame(class = c("radial", "linear"), 
                       avg=c(mean(rad4000$accuracy),mean(lin4000$accuracy)),
                       stddev=c(sd(rad4000$accuracy), sd(lin4000$accuracy)))

ggplot(comp_dat, aes(x=class, y=avg, color=class)) + 
  geom_point(stat="identity", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=avg-stddev, ymax=avg+stddev), width=.2,
                position=position_dodge(.9))+
  ylim(-0.015, 0.015)+
  geom_hline(yintercept=0)

#sensitivity
comp_dat <- data.frame(class = c("radial", "linear"), 
                       avg=c(mean(rad4000$sensitivity),mean(lin4000$sensitivity)),
                       stddev=c(sd(rad4000$sensitivity), sd(lin4000$sensitivity)))

ggplot(comp_dat, aes(x=class, y=avg, color=class)) + 
  geom_point(stat="identity", 
             position=position_dodge()) +
  geom_errorbar(aes(ymin=avg-stddev, ymax=avg+stddev), width=.2,
                position=position_dodge(.9))+
  ylim(-0.015, 0.015)+
  geom_hline(yintercept=0)

#specificity
comp_dat <- data.frame(class = c("radial", "linear"), 
                       avg=c(mean(rad4000$specificity),mean(lin4000$specificity)),
                       stddev=c(sd(rad4000$specificity), sd(lin4000$specificity)))

ggplot(comp_dat, aes(x=class, y=avg, color=class)) + 
  geom_point(stat="identity", 
             position=position_dodge()) +
  geom_errorbar(aes(ymin=avg-stddev, ymax=avg+stddev), width=.2,
                position=position_dodge(.9))+
  ylim(-0.05, 0.05)+
  geom_hline(yintercept=0)

#But what about the raw values of specificity and accuracy? 
cv_raw <- function(nit, samp_size, kernel_func){
  train_sens <- vector()
  train_spec <- vector()
  train_acc <- vector()
  test_sens <- vector()
  test_spec <- vector()
  test_acc <- vector()
  #demonstrate how the larger the training data set the lower the risk. 
  for(i in 1:nit){
    train <- base::sample(n,samp_size, replace = FALSE)
    svmfit <- svm(y~., data = dat[train,], kernel = kernel_func, cost = 10)
    tab_train <- as.data.frame(table(true = dat[train,"y"], pred=predict(svmfit, dat[train,])))
    tab_test <- as.data.frame(table(true = dat[-train,"y"], pred=predict(svmfit, dat[-train,])))
    train_sens[i] <- (tab_train$Freq[4]/(tab_train$Freq[2]+tab_train$Freq[4])) 
    train_spec[i] <- (tab_train$Freq[1]/(tab_train$Freq[3]+tab_train$Freq[1])) 
    train_acc[i] <- ((tab_train$Freq[1]+tab_train$Freq[4])/sum(tab_train$Freq))
    test_sens[i] <- (tab_test$Freq[4]/(tab_test$Freq[2]+tab_test$Freq[4]))
    test_spec[i] <- (tab_test$Freq[1]/(tab_test$Freq[3]+tab_test$Freq[1]))
    test_acc[i] <- ((tab_test$Freq[1]+tab_test$Freq[4])/sum(tab_test$Freq))
  }
  error_ests <- data.frame(train_sensitivity = train_sens, train_specificity = train_spec, train_accuracy = train_acc,
                           test_sensitivity = test_sens, test_specificity = test_spec, test_accuracy = test_acc)
  return(error_ests)
}

lin4000_raw <- cv_raw(nit=1000, samp_size = 4000, kernel_func = "linear")
rad4000_raw <- cv_raw(nit=1000, samp_size = 4000, kernel_func = "radial")

#accuracy
comp_dat <- data.frame(class = rep(c("radial", "linear"),2), 
                       data = c("training", "training", "test", "test"),
                       avg=c(mean(rad4000_raw$train_accuracy),mean(lin4000_raw$train_accuracy), mean(rad4000_raw$test_accuracy), mean(lin4000_raw$test_accuracy)),
                       stddev=c(sd(rad4000_raw$train_accuracy),sd(lin4000_raw$train_accuracy), sd(rad4000_raw$test_accuracy), sd(lin4000_raw$test_accuracy)))

ggplot(comp_dat, aes(x=class, y=avg, color=data, group=data)) + 
  geom_point(stat="identity", 
             position=position_dodge(0.9)) +
  geom_errorbar(aes(ymin=avg-stddev, ymax=avg+stddev), width=.2,
                position=position_dodge(.9))
  #ylim(-0.015, 0.015)+
  #geom_hline(yintercept=0)

#sensitivity
comp_dat <- data.frame(class = rep(c("radial", "linear"),2), 
                       data = c("training", "training", "test", "test"),
                       avg=c(mean(rad4000_raw$train_sensitivity),mean(lin4000_raw$train_sensitivity), mean(rad4000_raw$test_sensitivity), mean(lin4000_raw$test_sensitivity)),
                       stddev=c(sd(rad4000_raw$train_sensitivity),sd(lin4000_raw$train_sensitivity), sd(rad4000_raw$test_sensitivity), sd(lin4000_raw$test_sensitivity)))

ggplot(comp_dat, aes(x=class, y=avg, color=data, group=data)) + 
  geom_point(stat="identity", 
             position=position_dodge(0.9)) +
  geom_errorbar(aes(ymin=avg-stddev, ymax=avg+stddev), width=.2,
                position=position_dodge(.9))
#ylim(-0.015, 0.015)+
#geom_hline(yintercept=0)

#specificity
comp_dat <- data.frame(class = rep(c("radial", "linear"),2), 
                       data = c("training", "training", "test", "test"),
                       avg=c(mean(rad4000_raw$train_specificity),mean(lin4000_raw$train_specificity), mean(rad4000_raw$test_specificity), mean(lin4000_raw$test_specificity)),
                       stddev=c(sd(rad4000_raw$train_specificity),sd(lin4000_raw$train_specificity), sd(rad4000_raw$test_specificity), sd(lin4000_raw$test_specificity)))

ggplot(comp_dat, aes(x=class, y=avg, color=data, group=data)) + 
  geom_point(stat="identity", 
             position=position_dodge(0.9)) +
  geom_errorbar(aes(ymin=avg-stddev, ymax=avg+stddev), width=.2,
                position=position_dodge(.9))
#ylim(-0.015, 0.015)+
#geom_hline(yintercept=0)




# plot(svmfit,dat)
# set.seed(10111)
# # sample training data and fit model that is it at a 1:5 ratio
# sens <- vector()
# spec <- vector()
# acc <- vector()
# wrong <- vector()
# #demonstrate how the larger the training data set the lower the risk. 
# for(i in 1:1000){
#   train <- base::sample(n,n/10, replace = FALSE)
#   svmfit <- svm(y~., data = dat[train,], kernel = "radial", cost = 10)
#   tab <- as.data.frame(table(true = dat[-train,"y"], pred=predict(svmfit, dat[-train,])))
#   sens[i] <- tab$Freq[4]/(tab$Freq[2]+tab$Freq[4])
#   spec[i] <- tab$Freq[1]/(tab$Freq[3]+tab$Freq[1])
#   acc[i] <- (tab$Freq[1]+tab$Freq[4])/sum(tab$Freq)
#   wrong[i] <- sum(tab$Freq[2]+tab$Freq[3])
# }
# hist(acc)
# 
# cv <- function(nit, samp_size, kernel_func){
#   sens <- vector()
#   spec <- vector()
#   acc <- vector()
#   wrong <- vector()
#   bia <- vector()
#   #demonstrate how the larger the training data set the lower the risk. 
#   for(i in 1:nit){
#     train <- base::sample(n,samp_size, replace = FALSE)
#     svmfit <- svm(y~., data = dat[train,], kernel = kernel_func, cost = 10)
#     tab <- as.data.frame(table(true = dat[-train,"y"], pred=predict(svmfit, dat[-train,])))
#     sens[i] <- tab$Freq[4]/(tab$Freq[2]+tab$Freq[4])
#     spec[i] <- tab$Freq[1]/(tab$Freq[3]+tab$Freq[1])
#     acc[i] <- (tab$Freq[1]+tab$Freq[4])/sum(tab$Freq)
#     wrong[i] <- sum(tab$Freq[2],tab$Freq[3])
#     bia[i] <- tab$Freq[2]-tab$Freq[3]
#   }
#   error_ests <- data.frame(sensitivity = sens, specificity = spec, accuracy = acc, num_wrong = wrong, bias = bia)
#   return(error_ests)
# }
# 
# #five fold resampling
# 
# 
# #linear 
# # sample size of 10 
# lin10 <- cv(nit=1000, samp_size = 25, kernel_func = "linear")
# # sample size of 100
# lin100 <- cv(nit=1000, samp_size = 100, kernel_func = "linear")
# # sample size of 1000
# lin1000 <- cv(nit=1000, samp_size = 1000, kernel_func = "linear")
# 
# #Histograms of bias
# lin_bias <- data.frame(samp_size = c(rep(25,1000), rep(100,1000), rep(1000,1000)),
#                        bias = c(lin10$bias, lin100$bias, lin1000$bias))
# lin_bias$samp_size <- as.character(lin_bias$samp_size)
# ggplot(data=lin_bias, aes(x=bias, color=samp_size, fill=samp_size))+
#   geom_histogram(alpha=0.5, position="identity",binwidth = 50)
# 
# #Histograms of accuracy
# lin_accuracy <- data.frame(samp_size = c(rep(25,1000), rep(100,1000), rep(1000,1000)),
#                        accuracy = c(lin10$accuracy, lin100$accuracy, lin1000$accuracy))
# lin_accuracy$samp_size <- as.character(lin_accuracy$samp_size)
# ggplot(data=lin_accuracy, aes(x=accuracy, color=samp_size, fill=samp_size))+
#   geom_histogram(alpha=0.5, position="identity",binwidth = 0.01)+
#   xlim(0.6,1)+
#   geom_vline(xintercept=0.88)
# 
# # hist(lin10$bias)
# # hist(lin100$bias)
# # hist(lin1000$bias)
# 
# #what about probability? 
# #sample size of 25 
# ggplot(data=lin10, aes(x=accuracy))+
#   geom_histogram(alpha=0.5, position="identity",binwidth = 0.01, color ="#00BFC4", fill ="#00BFC4")+
#   xlim(0.6,1)+
#   geom_vline(xintercept=0.85)
# #accuracy below 0.8 is:
# sum(lin10$accuracy > 0.85)/1000
# #0.011
# #100
# ggplot(data=lin100, aes(x=accuracy))+
#   geom_histogram(alpha=0.5, position="identity",binwidth = 0.01, color ="#F8766D", fill ="#F8766D")+
#   xlim(0.6,1)+
#   geom_vline(xintercept=0.85)
# #accuracy below 0.8 is:
# sum(lin100$accuracy > 0.85)/1000
# 
# #1000
# ggplot(data=lin1000, aes(x=accuracy))+
#   geom_histogram(alpha=0.5, position="identity",binwidth = 0.01, color ="#7CAE00", fill ="#7CAE00")+
#   xlim(0.6,1)+
#   geom_vline(xintercept=0.85)
# #accuracy below 0.8 is:
# sum(lin1000$accuracy > 0.85)/1000
# 
# svmfit = svm(y ~ ., data = dat, kernel = "radial", cost = 10, scale = FALSE)
# tab <- as.data.frame(table(true = dat[,"y"], pred=predict(svmfit, dat)))
# (tab$Freq[1]+tab$Freq[4])/sum(tab$Freq)
# #89
# #radial
# # sample size of 10 
# rad10 <- cv(nit=1000, samp_size = 25, kernel_func = "radial")
# # sample size of 100
# rad100 <- cv(nit=1000, samp_size = 100, kernel_func = "radial")
# # sample size of 1000
# rad1000 <- cv(nit=1000, samp_size = 1000, kernel_func = "radial")
# 
# 
# 
# n <- 10000
# 
# set.seed(10111)
# x = matrix(rnorm(n, sd = 1.5), n/2, 2)
# y = rep(c(-1, 1), c(n/4, n/4))
# x[y == 1,] = x[y == 1,] + 3
# plot(x, col = y + 3)
# 
# dat = data.frame(x, y = as.factor(y))
# svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
# plot(svmfit, dat)
# 
# # set pseudorandom number generator
# set.seed(10111)
# # sample training data and fit model that is it at a 1:5 ratio
# sens <- vector()
# spec <- vector()
# acc <- vector()
# wrong <- vector()
# #demonstrate how the larger the training data set the lower the risk. 
# for(i in 1:1000){
#   train <- base::sample(n,n/10, replace = FALSE)
#   svmfit <- svm(y~., data = dat[train,], kernel = "linear", gamma = 1, cost = 1)
#   tab <- as.data.frame(table(true = dat[-train,"y"], pred=predict(svmfit, dat[-train,])))
#   sens[i] <- tab$Freq[4]/(tab$Freq[2]+tab$Freq[4])
#   spec[i] <- tab$Freq[1]/(tab$Freq[3]+tab$Freq[1])
#   acc[i] <- (tab$Freq[1]+tab$Freq[4])/sum(tab$Freq)
#   wrong[i] <- sum(tab$Freq[2]+tab$Freq[3])
# }
# hist(acc)
# 
# 
# 
# train <- base::sample(n,n/1.25, replace = FALSE)
# svmfit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
# plot(svmfit, dat)
# 
# #Sensitivity-specificity analysis
# tab <- as.data.frame(table(true = dat[-train,"y"], pred=predict(svmfit, dat[-train,])))
# sensetivity <- tab$Freq[4]/(tab$Freq[2]+tab$Freq[4])
# specificity <- tab$Freq[1]/(tab$Freq[3]+tab$Freq[1])
# 
# 
