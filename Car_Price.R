getwd()
View(car_price)
str(car_price)

##univariate analysis
summary(car_price)

##bivariate analysis

##Car_Price_category vs symboling
t.test(car_price$symboling~car_price$Car_Price_category,var.equal=T)
##since pvalue is less than 0.05 reject Ho and variable is significant


##Car_Price_category vs normalized_losses
t.test(car_price$normalized_losses~car_price$Car_Price_category,var.equal=T)
##since pvalue is more than 0.05 accept Ho and variable is not significant


##car price category vs make
x1 <- table(car_price$Car_Price_category,car_price$make)
prop.table(x1,2)*100
chisq.test(x1)
##since pvalue is less than 0.05 reject Ho and variable is significant

##car price category vs fuel_type
x1 <- table(car_price$Car_Price_category,car_price$fuel_type)
prop.table(x1,2)*100
chisq.test(x1)
##since pvalue is less than 0.05 reject Ho and variable is significant
##2levels


##car price category vs aspiration
x1 <- table(car_price$Car_Price_category,car_price$aspiration)
prop.table(x1,2)*100
chisq.test(x1)
##since pvalue is more than 0.05 accept Ho and variable is not significant
##2levels


##car price category vs num_of_doors
x1 <- table(car_price$Car_Price_category,car_price$num_of_doors)
prop.table(x1,2)*100
chisq.test(x1)
##since pvalue is more than 0.05 accept Ho and variable is not significant(approximately)********
##3levels

##car price category vs body_style
x1 <- table(car_price$Car_Price_category,car_price$body_style)
prop.table(x1,2)*100
chisq.test(x1)
##since pvalue is less than 0.05 reject Ho and variable is significant
##5levels

##car price category vs drive_wheels
x1 <- table(car_price$Car_Price_category,car_price$drive_wheels)
prop.table(x1,2)*100
chisq.test(x1)
##since pvalue is less than 0.05 reject Ho and variable is significant
##3levels

##car price category vs engine_location
x1 <- table(car_price$Car_Price_category,car_price$engine_location)
prop.table(x1,2)*100
chisq.test(x1)
##since pvalue is less than 0.05 reject Ho and variable is significant**********
##3levels

##car price category vs wheel_base
t.test(car_price$wheel_base~car_price$Car_Price_category,var.equal=T)
##since pvalue is less than 0.05 reject Ho and variable is significant

##car price category vs length
t.test(car_price$length~car_price$Car_Price_category,var.equal=T)
##since pvalue is more than 0.05 accept Ho and variable is not significant

##car price category vs width
t.test(car_price$width~car_price$Car_Price_category,var.equal=T)
##since pvalue is less than 0.05 reject Ho and variable is significant

##car price category vs height
t.test(car_price$height~car_price$Car_Price_category,var.equal=T)
##since pvalue is less than 0.05 reject Ho and variable is significant

##car price category vs curb_weight
t.test(car_price$curb_weight~car_price$Car_Price_category,var.equal=T)
##since pvalue is less than 0.05 reject Ho and variable is significant

##car price category vs engine type
x1 <- table(car_price$Car_Price_category,car_price$engine_type)
prop.table(x1,2)*100
chisq.test(x1)
##since pvalue is less than 0.05 reject Ho and variable is significant*********
#7levels

##car price category vs num_of_cylinders
x1 <- table(car_price$Car_Price_category,car_price$num_of_cylinders)
prop.table(x1,2)*100
chisq.test(x1)
##since pvalue is less than 0.05 reject Ho and variable is significant
#7levels

##car price category vs engine_size
t.test(car_price$engine_size~car_price$Car_Price_category,var.equal=T)
##since pvalue is less than 0.05 reject Ho and variable is significant

##car price category vs fuelsystem
x1 <- table(car_price$Car_Price_category,car_price$fuel_system)
prop.table(x1,2)*100
chisq.test(x1)
##since pvalue is less than 0.05 reject Ho and variable is significant
#8levels

##car price category vs bore
t.test(car_price$bore~car_price$Car_Price_category,var.equal=T)
##since pvalue is less than 0.05 reject Ho and variable is significant

##car price category vs stroke
t.test(car_price$stroke~car_price$Car_Price_category,var.equal=T)
##since pvalue is more than 0.05 reject Ho and variable is not significant

##car price category vs compression_ratio
t.test(car_price$compression_ratio~car_price$Car_Price_category,var.equal=T)
##since pvalue is less than 0.05 reject Ho and variable is significant

##car price category vs horsepower
t.test(car_price$horsepower~car_price$Car_Price_category,var.equal=T)
##since pvalue is less than 0.05 reject Ho and variable is significant

##car price category vs peakrpm
t.test(car_price$peak_rpm~car_price$Car_Price_category,var.equal=T)
##since pvalue is less than 0.05 reject Ho and variable is significant

##car price category vs city_mpg
t.test(car_price$city_mpg~car_price$Car_Price_category,var.equal=T)
##since pvalue is less than 0.05 reject Ho and variable is significant

##car price category vs highway_mpg
t.test(car_price$highway_mpg~car_price$Car_Price_category,var.equal=T)
##since pvalue is less than 0.05 reject Ho and variable is significant


##Missing value analysis##
sum(is.na(car_price=="?"))
sum(is.na(car_price))
sapply(car_price,function(x)sum(is.na(x)))
##138 Missing values are found in 5 independent variables but out of which 128 is found 
##in normalized_losses so we neglecting that independent variable.

car_price$normalized_losses <- NULL

##Missing value treatment for bore(assiging mean to missing value)
car_price$bore[is.na(car_price$bore)] <- mean(car_price$bore, na.rm = T)
sum(is.na(car_price$bore))

##Missing value treatment for stroke
car_price$stroke[is.na(car_price$stroke)] <- mean(car_price$stroke, na.rm = T)
sum(is.na(car_price$stroke))

##Missing value treatment for horsepower
car_price$horsepower[is.na(car_price$horsepower)] <- mean(car_price$horsepower, na.rm = T)
sum(is.na(car_price$horsepower))

##Missing value treatment for peak_rpm
car_price$peak_rpm[is.na(car_price$peak_rpm)] <- mean(car_price$peak_rpm, na.rm = T)
sum(is.na(car_price$peak_rpm))

##Missing value "?" treatment for nom of doors
levels(car_price$num_of_doors)
table(car_price$Car_Price_category,car_price$num_of_doors)
#levels(car_price$num_of_doors) <- ifelse(levels(car_price$num_of_doors) == "?", "two","four" )
levels(car_price$num_of_doors) <- c("two","four","two")

##Missing value " " treatment for engine loacation
levels(car_price$engine_location)
table(car_price$Car_Price_category,car_price$engine_location)
levels(car_price$engine_location) <- c("front","front","rear")


sum(is.na(car_price))
View(car_price)

##Creating duplicate dataset to make analysis
car_price1 <- car_price

##Removing non significant variable for analysis
car_price1$num_of_doors_new <- NULL
car_price1$drive_wheel_new <- NULL
car_price1$engine_location_new <- NULL
car_price1$bore_new <- NULL
car_price1$stroke_new <- NULL
car_price1$horsepower_new <- NULL
car_price1$peak_rpm_new <- NULL

##Finding the duplicates
sum(duplicated(car_price1))
car_price1[duplicated(car_price),]

car_price1 <- car_price1[-119,]
View(car_price1)
sum(duplicated(car_price1))

##response variable coding
car_price1$Car_Price_category <- ifelse(car_price1$Car_Price_category=="Highprice",1,0)
table(car_price1$Car_Price_category)


##split the data into train and test data.
set.seed(123)
car_sam <- sample.int(n = nrow(car_price1),size = nrow(car_price1)*0.70)
head(car_sam)
car_train <- car_price1[car_sam,]
car_test <- car_price1[-car_sam,]

prop.table(table(car_price1$Car_Price_category))
prop.table(table(car_train$Car_Price_category))
prop.table(table(car_test$Car_Price_category))


#fit_car <- glm(Car_Price_category~symboling+make+fuel_type+aspiration+num_of_doors+body_style+
#               drive_wheels+engine_location+wheel_base+length+width+height+curb_weight+engine_type+
#               num_of_cylinders+engine_size+fuel_system+bore+stroke+compression_ratio+horsepower+
#               peak_rpm+city_mpg+highway_mpg, data = car_train, family = "binomial")

#fit3<-step(fit_car)
#warnings()
#summary(fit3)
               
fit_car1 <- glm(Car_Price_category~body_style+aspiration+fuel_type+num_of_doors+drive_wheels+
            engine_location+bore+stroke+horsepower+peak_rpm+wheel_base+
            curb_weight,data = car_train, family = "binomial") 
                 
summary(fit_car1)
fit2<-step(fit_car1)
summary(fit2)

##Assumption of Logistic regression
##multicollinearity 
library(car)
vif(fit2)

###Validation
##Classification table
car_train$pred_prob <- predict(fit2,car_train,type="response")
car_train$pred_Y <- ifelse(car_train$pred_prob>=0.5,"1","0")
table(car_train$Car_Price_category,car_train$pred_Y)

#Sensitivity
22/(5+22)
##0.8148

#Specificity
294/(294+1)
##0.9966

#Accuracy
x1 <- table(car_train$Car_Price_category,car_train$pred_Y)
View(x1)
prop.table(x1,1)
sum(diag(x1))/sum(x1)
##0.9813

##Test Data
car_test$pred_prob <- predict(fit2,car_test,type="response")
car_test$pred_Y <- ifelse(car_test$pred_prob>=0.5,"1","0")
x2 <- table(car_test$Car_Price_category,car_test$pred_Y)
prop.table(x2,1)
sum(diag(x2))/sum(x2)

##concordance
library(InformationValue)
Concordance(car_train$Car_Price_category,car_train$pred_prob)
Concordance(car_test$Car_Price_category,car_test$pred_prob)

##Somers D
somersD(car_train$Car_Price_category,car_train$pred_prob)
somersD(car_test$Car_Price_category,car_test$pred_prob)

##ROC curve
plotROC(car_train$Car_Price_category,car_train$pred_prob)
plotROC(car_test$Car_Price_category,car_test$pred_prob)

##Rank ordering method
decile <- cut(car_train$pred_prob,
              breaks= quantile(car_train$pred_prob,
                               probs= seq(0,1,by=0.1)),
                 include.lowest=T)
table(decile,car_train$Car_Price_category)
