getwd()
# loading the data
data<-read.csv('CityPayrollDataset.csv')

# Print of the data
print(data)

# Head of the data
head(data)

# view of the data
View(data)

# Summary of the data
summary(data)

#Question 1

#Subset of Police Officer II
x <- data[data$Job.Class.Title == "Police Officer II",]
x

#Removing $ from the data
y <- as.numeric(gsub("\\$", "", data$Temporary.Bonus.Pay))
y

#Length
n = length(x)
n
m = length(y)
m

#Alpha Value = 0.05, Critical Value = 1.65, Right Tail Test

#Mean of population
mean_population <- mean(y)
mean_population

#Finding sample of 100 size, mean and Std:

sample = sample(1:nrow(x), size=100)
sample

sample_of_x <- x[sample,]
sample_of_x

sample_of_x$Temporary.Bonus.Pay = as.numeric(gsub("\\$", "", sample_of_x$Temporary.Bonus.Pay))
sample_of_x$Temporary.Bonus.Pay

mean_of_x <- mean(sample_of_x$Temporary.Bonus.Pay)
mean_of_x

std_of_x <- sd(sample_of_x$Temporary.Bonus.Pay)
std_of_x

# t-distribution
t_x <- ((mean_of_x-mean_population)*sqrt(99))/std_of_x
t_x

#Function of t-test
t.test(sample_of_x$Temporary.Bonus.Pay, mu=mean_population, alt="greater", conf=0.95)

#Since t-distribution of the data which is 5.24 is lies outside the critical region hence lies in the rejection region. So Null Hypothesis has been rejected and the Employees working as Police Officer-II have a better chance of getting Temporary Bonus Pay.

#Question 2

#Subset of Public Works - Sanitation
x <- data[data$Department.Title == "Public Works - Sanitation",]
x

#Removing $ from the data
y <- as.numeric(gsub("\\$", "", data$Permanent.Bonus.Pay))
y

#Length
n = length(x)
n
m = length(y)
m

#Alpha Value = 0.05, Critical Value = 1.65, Right Tail Test

#Mean of population
mean_population <- mean(y)
mean_population

#Finding sample of 100 size, mean and Std:

sample = sample(1:nrow(x), size=100)
sample

sample_of_x <- x[sample,]
sample_of_x

sample_of_x$Permanent.Bonus.Pay = as.numeric(gsub("\\$", "", sample_of_x$Permanent.Bonus.Pay))
sample_of_x$Permanent.Bonus.Pay

mean_of_x <- mean(sample_of_x$Permanent.Bonus.Pay)
mean_of_x

std_of_x <- sd(sample_of_x$Permanent.Bonus.Pay)
std_of_x

# t-distribution
t_x <- ((mean_of_x-mean_population)*sqrt(99))/std_of_x
t_x

#Function of t-test
t.test(sample_of_x$Permanent.Bonus.Pay, mu=mean_population, alt="greater", conf=0.95)

#Since t-distribution of the data which is -6.292375 is lies outside the critical region hence lies in the rejection region. So Null Hypothesis has rejected and the Employees who get Permanent Bonus Pay are most likely to be from Public Works - Sanitation Department.

#Question 3

#Subset of Water And Power (DWP)
x <- data[data$Department.Title == "Water And Power (DWP)",]
x

y <- sub("^$", 0.00, data$Overtime.Pay)

#Removing $ from the data
y <- as.numeric(gsub("\\$", "", y))
y


#Length
n = length(x)
n
m = length(y)
m

#Alpha Value = 0.05, Critical Value = 1.65, Right Tail Test

#Mean of population
mean_population <- mean(y)
mean_population

#Finding sample of 100 size, mean and Std:

sample = sample(1:nrow(x), size=100)
sample

sample_of_x <- x[sample,]
sample_of_x


sample_of_x$Overtime.Pay <- sub("^$", 0.00, sample_of_x$Overtime.Pay)
sample_of_x$Overtime.Pay = as.numeric(gsub("\\$", "", sample_of_x$Overtime.Pay))
sample_of_x$Overtime.Pay

mean_of_x <- mean(sample_of_x$Overtime.Pay)
mean_of_x

std_of_x <- sd(sample_of_x$Overtime.Pay)
std_of_x

# t-distribution
t_x <- ((mean_of_x-mean_population)*sqrt(99))/std_of_x
t_x

#Function of t-test
t.test(sample_of_x$Overtime.Pay, mu=mean_population, alt="greater", conf=0.95)

#Since t-distribution of the data which is 2.928701 which is lies outside the critical region which set as 1.65 hence lies in the rejection region. So Null Hypothesis has been rejected and the Employees working in Water and Power (DWP) Department have a better chance of being employed overtime.

#Question 4

#Subset of Recreation And Parks and Year of 2014
x <- data[data$Department.Title == "Recreation And Parks",]
x <- x[x$Year == 2014,]
x

#Removing $ from the data
y <- as.numeric(gsub("\\$", "", data$Longevity.Bonus.Pay))
y


#Length
n = length(x)
n
m = length(y)
m

#Alpha Value = 0.05, Critical Value = 1.65, Right Tail Test

#Mean of population
mean_population <- mean(y)
mean_population

#Finding sample of 100 size, mean and Std:

sample = sample(1:nrow(x), size=100)
sample

sample_of_x <- x[sample,]
sample_of_x

sample_of_x$Longevity.Bonus.Pay = as.numeric(gsub("\\$", "", sample_of_x$Longevity.Bonus.Pay))
sample_of_x$Longevity.Bonus.Pay

mean_of_x <- mean(sample_of_x$Longevity.Bonus.Pay)
mean_of_x

std_of_x <- sd(sample_of_x$Longevity.Bonus.Pay)
std_of_x

# t-distribution
t_x <- ((mean_of_x-mean_population)*sqrt(99))/std_of_x
t_x

#Function of t-test
t.test(sample_of_x$Longevity.Bonus.Pay, mu=mean_population, alt="greater", conf=0.95)
#Since t-distribution of the data which is 0.6284411 which is lies inside the critical region which set as 1.65. So Null Hypothesis has fail to reject and In 2014, employees of Recreation and Parks Department were complaining that they have not been denied the Longevity Bonus Pay.

#Question 5

#Subset of Harbor (Port of LA) and Senior Clerk Typis
x <- data[data$Department.Title == "Harbor (Port of LA)",]
x <- x[x$Job.Class.Title == "Senior Clerk Typist",]
x

#Subset of Water And Power (DWP) and Senior Clerk Typis
z <- data[data$Department.Title == "Water And Power (DWP)",]
z <- z[z$Job.Class.Title == "Senior Clerk Typist",]
z

y <- as.numeric(gsub("\\$", "", data$Average.Health.Cost))
y

#Length
n = length(x)
n
m = length(y)
m
p = length(z)
p

#Alpha Value = 0.05, Critical Value = 1.65, Right Tail Test

#Mean of population
mean_population <- mean(y)
mean_population

#Finding sample of 100 size, mean and Std of x:

sample = sample(1:nrow(x), size=100)
sample

sample_of_x <- x[sample,]
sample_of_x

sample_of_x$Average.Health.Cost = as.numeric(gsub("\\$", "", sample_of_x$Average.Health.Cost))
sample_of_x$Average.Health.Cost

mean_of_x <- mean(sample_of_x$Average.Health.Cost)
mean_of_x

std_of_x <- sd(sample_of_x$Average.Health.Cost)
std_of_x

# t-distribution of x
t_x <- ((mean_of_x-mean_population)*sqrt(99))/std_of_x
t_x


#Finding sample of 100 size, mean and Std of z:
sample = sample(1:nrow(z), size=100)
sample

sample_of_z <- z[sample,]
sample_of_z

sample_of_z$Average.Health.Cost = as.numeric(gsub("\\$", "", sample_of_z$Average.Health.Cost))
sample_of_z$Average.Health.Cost

mean_of_z <- mean(sample_of_z$Average.Health.Cost)
mean_of_z

std_of_z <- sd(sample_of_z$Average.Health.Cost)
std_of_z

# t-distribution of z
t_z <- ((mean_of_z-mean_population)*sqrt(99))/std_of_z
t_z

#Function of t-test
t.test(sample_of_x$Average.Health.Cost,sample_of_z$Average.Health.Cost, mu=mean_population, alt="greater", conf=0.95)

#T-distribution of the Harbor (Port of LA) which is -1.416474 is lies inside the critical region and t-distribution of the Water And Power (DWP) which is 88.13205 is lies outside the critical region hence lies in the rejection region. So we have to reject the Null Hypothesis and and proved the claim.