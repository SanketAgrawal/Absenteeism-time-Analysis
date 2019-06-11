
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
     "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','xlsx','psych')

#install.packages(x)
lapply(x, require, character.only = TRUE)
require(miscTools)
require(tree)
require(mlr)
rm(x)
list.files(path = "../input")
#Load the data
dataset=read.xlsx('../input/Absenteeism_at_work_Project.xls',
                  sheetIndex =1)
str(dataset)

#Replace 0 with the nan
dataset$Reason.for.absence[dataset$Reason.for.absence %in% 0]=NA
dataset$Month.of.absence[dataset$Month.of.absence %in% 0]=NA

#Convert categorival variables
dataset$Reason.for.absence = as.factor(dataset$Reason.for.absence)
dataset$Month.of.absence = as.factor(dataset$Month.of.absence)
dataset$Day.of.the.week = as.factor(dataset$Day.of.the.week)
dataset$Seasons = as.factor(dataset$Seasons)
dataset$Disciplinary.failure = as.factor(dataset$Disciplinary.failure)
dataset$Education = as.factor(dataset$Education)
dataset$Son = as.factor(dataset$Son)
dataset$Pet = as.factor(dataset$Pet)
dataset$Social.drinker = as.factor(dataset$Social.drinker)
dataset$Social.smoker = as.factor(dataset$Social.smoker)

#Print the unique values for each columns
sapply(dataset, function(x) length(unique(x)))

##################################Missing Values Analysis###############################################
missing_val = data.frame(apply(dataset,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(dataset)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_perc.csv", row.names = F)

print(missing_val)

#Define a function for mode
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
# numeric_index = sapply(dataset,is.numeric) #selecting only numeric
numeric_index=c(6,7,8,9,10,11,18,19)
numeric_data = dataset[,numeric_index]

cnames = colnames(numeric_data)
print(cnames)
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i])), data = subset(dataset))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot of",cnames[i])))
}

## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)
gridExtra::grid.arrange(gn7,gn8,ncol=2)

#Replace outliers with the na
for(i in cnames){
  val = dataset[,i][dataset[,i] %in% boxplot.stats(dataset[,i])$out]
  dataset[,i][dataset[,i] %in% val] = NA
}

dataset$Reason.for.absence = as.numeric(dataset$Reason.for.absence)
dataset$Month.of.absence = as.numeric(dataset$Month.of.absence)
dataset$Day.of.the.week = as.numeric(dataset$Day.of.the.week)
dataset$Seasons = as.numeric(dataset$Seasons)
dataset$Disciplinary.failure = as.numeric(dataset$Disciplinary.failure)
dataset$Education = as.numeric(dataset$Education)
dataset$Son = as.numeric(dataset$Son)
dataset$Pet = as.numeric(dataset$Pet)
dataset$Social.drinker = as.numeric(dataset$Social.drinker)
dataset$Social.smoker = as.numeric(dataset$Social.smoker)


#Impute the continious variables
columns=c('Transportation.expense', 'Distance.from.Residence.to.Work',
       'Service.time', 'Age', 'Work.load.Average.day.', 'Hit.target'
       , 'Education', 'Son', 'Social.drinker',
       'Social.smoker', 'Pet','Weight', 'Height','Body.mass.index')
for(col in columns){
    print(col)
    for(i in unique(dataset$ID)){
        if(sum(is.na(as.numeric(dataset[dataset$ID==i,col])))>0){
            
            if(is.numeric(dataset[,col]))
              dataset[dataset$ID==i,col] <- ifelse(is.na(dataset[dataset$ID==i,col]), ifelse(is.na(round(mean(as.numeric(dataset[dataset$ID==i,col]),na.rm=TRUE))),mean(as.numeric(dataset[,col]),na.rm=TRUE),round(mean(as.numeric(dataset[dataset$ID==i,col]),na.rm=TRUE))), dataset[dataset$ID==i,col])
            else
              dataset[dataset$ID==i,col] <- ifelse(is.na(dataset[dataset$ID==i,col]), 
                                                             ifelse(is.na(mean(as.numeric(dataset[dataset$ID==i,col]))),round(median(as.numeric(dataset[,col]),na.rm=TRUE)),mean(as.numeric(dataset[dataset$ID==i,col]))), 
                                                             as.character(dataset[dataset$ID==i,col]))
        }
    }
}
#Impute the categorical variables
columns=c('Reason.for.absence', 'Month.of.absence', 'Day.of.the.week',
       'Seasons', 'Disciplinary.failure')
for(col in columns){
    print(col)
    if(sum(is.na(as.numeric(dataset[dataset$ID==i,col])))>0)
        dataset[,col] <- ifelse(is.na(dataset[,col]), getmode(as.numeric(dataset[,col])), as.numeric(dataset[,col]))
}
#Remove the iinstances where target variable is na
dataset <- na.omit(dataset)

missing_val = data.frame(apply(dataset,2,function(x){sum(is.na(x))}))
print(missing_val)

cont_var_set = c("ID","Transportation.expense","Distance.from.Residence.to.Work","Service.time","Age","Work.load.Average.day.","Hit.target","Height","Weight","Body.mass.index","Absenteeism.time.in.hours")
cat_var_set = c("Reason.for.absence","Month.of.absence","Day.of.the.week","Seasons","Disciplinary.failure","Education","Son","Pet","Social.drinker","Social.smoker")

#Univariate Analysis
multi.hist(dataset[,cont_var_set], main = NA, dcol = c("blue", "red"),
dlty = c("solid", "solid"), bcol = "linen")

#Correlation matrix
corrgram(dataset[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

dataset$Reason.for.absence = as.factor(dataset$Reason.for.absence)
dataset$Month.of.absence = as.factor(dataset$Month.of.absence)
dataset$Day.of.the.week = as.factor(dataset$Day.of.the.week)
dataset$Seasons = as.factor(dataset$Seasons)
dataset$Disciplinary.failure = as.factor(dataset$Disciplinary.failure)
dataset$Education = as.factor(dataset$Education)
dataset$Son = as.factor(dataset$Son)
dataset$Pet = as.factor(dataset$Pet)
dataset$Social.drinker = as.factor(dataset$Social.drinker)
dataset$Social.smoker = as.factor(dataset$Social.smoker)

#Chi-square test
factor_index = sapply(dataset,is.factor)
factor_data = dataset[,factor_index]
for (i in 1:10)
{ for(j in 1:10){
    if (i!=j){
    print(paste(names(factor_data)[i],names(factor_data)[j]))
    
  print(chisq.test(table(factor_data[,i],factor_data[,j]),simulate.p.value = TRUE)$p.value)
        }
}
  
}
#Anova summay
for(i in cat_var_set){
  print(i)
  aov_summary = summary(aov(Absenteeism.time.in.hours~dataset[,i],data = dataset))
  print(aov_summary)
}

#remove the variables
dataset = subset(dataset, 
                          select = -c(Social.smoker, Pet, Education, Body.mass.index))

dataset[] <- lapply(dataset, function(x) as.numeric(x))
print(dim(dataset))
#Split the dataset
set.seed(1234)
train.index = createDataPartition(dataset$Absenteeism.time.in.hours, p = .70, list = FALSE)
train = dataset[ train.index,]
test.index=-(train.index)
test  = dataset[-train.index,]

# Run the model without any transformations

linear = lm(Absenteeism.time.in.hours~.,data = dataset[train.index,])
predictions = predict(linear,dataset[train.index,])
print(paste("Train MSE=",mean((predictions-dataset$Absenteeism.time.in.hours[train.index])^2)))
print(paste("Train R2=",rSquared(predictions, resid = predictions-dataset$Absenteeism.time.in.hours[train.index])))
predictions = predict(linear,dataset[test.index,])
print(paste("Test MSE=",mean((predictions-dataset$Absenteeism.time.in.hours[test.index])^2)))
print(paste("Test R2=",rSquared(predictions, resid = predictions-dataset$Absenteeism.time.in.hours[test.index])))

model=tree(Absenteeism.time.in.hours~.,dataset,subset = train.index)
predictions = predict(model,dataset[train.index,])
print(paste("Train MSE=",mean((predictions-dataset$Absenteeism.time.in.hours[train.index])^2)))
print(paste("Train R2=",rSquared(predictions, resid = predictions-dataset$Absenteeism.time.in.hours[train.index])))
predictions = predict(model,dataset[test.index,])
print(paste("Test MSE=",mean((predictions-dataset$Absenteeism.time.in.hours[test.index])^2)))
print(paste("Test R2=",rSquared(predictions, resid = predictions-dataset$Absenteeism.time.in.hours[test.index])))

model=randomForest(Absenteeism.time.in.hours~.,data = dataset,subset = train.index,mtry = 11,ntree=25)
predictions = predict(model,dataset[train.index,])
print(paste("Train MSE=",mean((predictions-dataset$Absenteeism.time.in.hours[train.index])^2)))
print(paste("Train R2=",rSquared(predictions, resid = predictions-dataset$Absenteeism.time.in.hours[train.index])))
predictions = predict(model,dataset[test.index,])
print(paste("Test MSE=",mean((predictions-dataset$Absenteeism.time.in.hours[test.index])^2)))
print(paste("Test R2=",rSquared(predictions, resid = predictions-dataset$Absenteeism.time.in.hours[test.index])))

dataset1=dataset

#Normalize the data
cnames = c('ID','Transportation.expense', 'Distance.from.Residence.to.Work', 'Service.time', 'Age', 'Work.load.Average.day.', 'Hit.target', 'Weight', 'Height')

for(i in cnames){
  print(i)
  dataset[,i] = (dataset[,i] - min(dataset[,i]))/(max(dataset[,i]) - min(dataset[,i]))
}
#Run model after normalization
					
linear = lm(Absenteeism.time.in.hours~.,data = dataset[train.index,])
predictions = predict(linear,dataset[train.index,])
print(paste("Train MSE=",mean((predictions-dataset$Absenteeism.time.in.hours[train.index])^2)))
print(paste("Train R2=",rSquared(predictions, resid = predictions-dataset$Absenteeism.time.in.hours[train.index])))
predictions = predict(linear,dataset[test.index,])
print(paste("Test MSE=",mean((predictions-dataset$Absenteeism.time.in.hours[test.index])^2)))
print(paste("Test R2=",rSquared(predictions, resid = predictions-dataset$Absenteeism.time.in.hours[test.index])))

model=tree(Absenteeism.time.in.hours~.,dataset,subset = train.index)
predictions = predict(model,dataset[train.index,])
print(paste("Train MSE=",mean((predictions-dataset$Absenteeism.time.in.hours[train.index])^2)))
print(paste("Train R2=",rSquared(predictions, resid = predictions-dataset$Absenteeism.time.in.hours[train.index])))
predictions = predict(model,dataset[test.index,])
print(paste("Test MSE=",mean((predictions-dataset$Absenteeism.time.in.hours[test.index])^2)))
print(paste("Test R2=",rSquared(predictions, resid = predictions-dataset$Absenteeism.time.in.hours[test.index])))

model=randomForest(Absenteeism.time.in.hours~.,data = dataset,subset = train.index,mtry = 11,ntree=25)
predictions = predict(model,dataset[train.index,])
print(paste("Train MSE=",mean((predictions-dataset$Absenteeism.time.in.hours[train.index])^2)))
print(paste("Train R2=",rSquared(predictions, resid = predictions-dataset$Absenteeism.time.in.hours[train.index])))
predictions = predict(model,dataset[test.index,])
print(paste("Test MSE=",mean((predictions-dataset$Absenteeism.time.in.hours[test.index])^2)))
print(paste("Test R2=",rSquared(predictions, resid = predictions-dataset$Absenteeism.time.in.hours[test.index])))

dataset$Reason.for.absence = as.factor(dataset$Reason.for.absence)
dataset$Month.of.absence = as.factor(dataset$Month.of.absence)
dataset$Day.of.the.week = as.factor(dataset$Day.of.the.week)
dataset$Seasons = as.factor(dataset$Seasons)
dataset$Disciplinary.failure = as.factor(dataset$Disciplinary.failure)
dataset$Son = as.factor(dataset$Son)
dataset$Social.drinker = as.factor(dataset$Social.drinker)

#Add dummy variables
extended_dataset=createDummyFeatures(dataset, cols = c('Reason.for.absence', 'Month.of.absence', 'Day.of.the.week',
       'Seasons', 'Disciplinary.failure', 'Son', 'Social.drinker'))

extended_dataset[] <- lapply(extended_dataset, function(x) as.numeric(x))
print(dim(extended_dataset))

#Run models on the new dataset
linear = lm(Absenteeism.time.in.hours~.,data = extended_dataset[train.index,])
predictions = predict(linear,extended_dataset[train.index,])
print(paste("Train MSE=",mean((predictions-extended_dataset$Absenteeism.time.in.hours[train.index])^2)))
print(paste("Train R2=",rSquared(predictions, resid = predictions-extended_dataset$Absenteeism.time.in.hours[train.index])))
predictions = predict(linear,extended_dataset[test.index,])
print(paste("Test MSE=",mean((predictions-extended_dataset$Absenteeism.time.in.hours[test.index])^2)))
print(paste("Test R2=",rSquared(predictions, resid = predictions-extended_dataset$Absenteeism.time.in.hours[test.index])))

model=tree(Absenteeism.time.in.hours~.,extended_dataset,subset = train.index)
predictions = predict(model,extended_dataset[train.index,])
print(paste("Train MSE=",mean((predictions-extended_dataset$Absenteeism.time.in.hours[train.index])^2)))
print(paste("Train R2=",rSquared(predictions, resid = predictions-extended_dataset$Absenteeism.time.in.hours[train.index])))
predictions = predict(model,extended_dataset[test.index,])
print(paste("Test MSE=",mean((predictions-extended_dataset$Absenteeism.time.in.hours[test.index])^2)))
print(paste("Test R2=",rSquared(predictions, resid = predictions-extended_dataset$Absenteeism.time.in.hours[test.index])))

model=randomForest(Absenteeism.time.in.hours~.,data = extended_dataset,subset = train.index,mtry = 11,ntree=25)
predictions = predict(model,extended_dataset[train.index,])
print(paste("Train MSE=",mean((predictions-extended_dataset$Absenteeism.time.in.hours[train.index])^2)))
print(paste("Train R2=",rSquared(predictions, resid = predictions-extended_dataset$Absenteeism.time.in.hours[train.index])))
predictions = predict(model,extended_dataset[test.index,])
print(paste("Test MSE=",mean((predictions-extended_dataset$Absenteeism.time.in.hours[test.index])^2)))
print(paste("Test R2=",rSquared(predictions, resid = predictions-extended_dataset$Absenteeism.time.in.hours[test.index])))

							 
#Suggestions
for(col in colnames(dataset)){
    print(col)
    group = tapply(dataset1$Absenteeism.time.in.hours, dataset1[,col],FUN = sum)
    barplot(group)
}


for(col in colnames(dataset)){
    print(col)
    group = tapply(dataset1$Absenteeism.time.in.hours, dataset1[,col],FUN = mean)
    barplot(group)
}


#Trends

df=data.frame(aggregate(dataset1$Absenteeism.time.in.hours, by=list(dataset1[,'Month.of.absence']),FUN = sum))
test=data.frame(x=c(13,14,15,16,17,18,19,20,21,22,23,24),Group.1=c(0,0,0,0,0,0,0,0,0,0,0,0))
linear = lm(x~Group.1,data = df)
predictions = predict(linear,df)
plot(df[,'Group.1'],df[,'x'])
predictions = predict(linear,test)
print(predictions)

df=data.frame(aggregate(dataset1$Absenteeism.time.in.hours, by=list(dataset1[,'Month.of.absence']),FUN = mean))
test=data.frame(x=c(13,14,15,16,17,18,19,20,21,22,23,24),Group.1=c(0,0,0,0,0,0,0,0,0,0,0,0))
linear = lm(x~Group.1,data = df)
predictions = predict(linear,df)
plot(df[,'Group.1'],df[,'x'])
predictions = predict(linear,test)
print(predictions)

df=data.frame(aggregate(dataset1$Absenteeism.time.in.hours, by=list(dataset1[,'Month.of.absence']),FUN = length))
test=data.frame(x=c(13,14,15,16,17,18,19,20,21,22,23,24),Group.1=c(0,0,0,0,0,0,0,0,0,0,0,0))
linear = lm(x~Group.1,data = df)
predictions = predict(linear,df)
plot(df[,'Group.1'],df[,'x'])
predictions = predict(linear,test)
print(predictions)


