read.csv('F:\\warehouse\\study_warehouse\\R\\Data Set\\datasets\\customer_churn.csv',stringsAsFactors = T)->churn
read.csv('F:/warehouse/study_warehouse/R/Data Set/datasets/customer_churn.csv',stringsAsFactors = T)->churn

View(churn)


# -----------------------DATA PREPROCESSING----------------------------------------------

str(churn)
sum(is.na(churn))
na.omit(churn)->churn

# extracting column from df and store it in another variable
churn$gender->gender
View(as.data.frame(gender))
sum(is.na(gender))

churn[,c(1:10)]->col
as.data.frame(col)->col
View(col)

# extracting rows & column
churn[c(1:5),c(2,3)]->rand
View(as.data.frame(rand))->rand

# 1 to 100 rows extracted
churn[1:100,]->ch
ch

churn[c(10,20,30,50),]->se
se

#OPERATORS

#substract
churn$MonthlyCharges[5]-20->churn$MonthlyCharges[5]

#addition
churn$MonthlyCharges[6]+20->churn$MonthlyCharges[6]

#multiply ,giving discount to custmer
churn$TotalCharges[1]*0.5->churn$TotalCharges[1]

#divide 
churn$TotalCharges[4]/2->churn$TotalCharges[4]

#greater then operator

churn$tenure > 60->over_60
over_60
table(over_60)
# storing all the true value in ov variable using subset 
subset(churn,over_60==T)->ov
ov
#less then operator
churn$MonthlyCharges < 10->greater
table(greater)


# logical operators & | =!
# & op
churn$gender=='Male' & churn$SeniorCitizen==1->and
table(and)

# | op
churn$InternetService=='DSL' | churn$InternetService=='Fiberoptic'->or
table(or)

# !=
churn$Partner!='Yes'->not
table(not)

# in_build functions

str(churn)
head(churn) # by default it is giving first 6 records
head(churn,10) # we can change it accordingly
tail(churn)
tail(churn,10)

nrow(churn) # gives number of rows 
ncol(churn) # give number of col

mean(churn$MonthlyCharges)

min(churn$MonthlyCharges)

max(churn$MonthlyCharges)

range(churn$MonthlyCharges) # give min and max value

sample(churn$customerID,5) # selecting 5 random custmer ID
sample(churn$customerID ,20) # selecting 20 random custmer ID

table(churn$gender) # gives the count 
table(churn$Churn)
table(churn$InternetService)
table(churn$Contract)
table(churn$StreamingMovies)
table(churn$PaymentMethod)


#CONTROL FLOW STATEMENTS

#switch continue if ifelse repeat break for while 

#selector statements if ifelse switch

if(churn$gender[2]=='Male'){ # we change the value from male to female
  churn$gender[2]<-"Female"
}
######
if(churn$tenure[10]>50){
  churn$MonthlyCharges[10]<-churn$MonthlyCharges[10]*0.10
} # we are giving discount if tenure is more then 50
 
######
if(churn$Churn[3]=='Yes'){
  print("Please give us feedback")
}else{
  print("Thankyou for using")
}

if(churn$Churn[1]=='Yes'){
  print("Please give us feedback")
}else{
  print("Thankyou for using")
}

switch (as.character(churn$InternetService[5]),
  'DSL' = churn$MonthlyCharges[5]*.10,
  'Fiber optic'= churn$MonthlyCharges[5]*0.8
)->churn$MonthlyCharges[5]

View(churn)

# looping statements
count=0
for (i in 1:nrow(churn)) {
  if(churn$gender[i]=='Male'){
    count=count+1
    }
}


#while loop
count=o
i=0
while (i<nrow(churn)) {
  if(churn$PaymentMethod[i]=='Electronic check'){
    count=count+1
  }
  i=i+1
}



#user define functions

gender_count <-function(x){
  couont=0
  for(i in 1:length(x)){
    if(x[i]=="Female"){
      count=count+1
    }
  }
  count
}


gender_count(churn$gender)
table(churn$gender)


#----------DATA STRUCTURE-----------


bird<-(c('pegion','parrot','sparrow'))
bird
class(bird)


num<-(1:9)
num
class(num)

decimal<-(1.5:10)
class(decimal)

logical<-(c(T,T,F))
class(logical)

#list

list(1,"hello",T,2.5)->mix
class(mix)

class(mix[[1]])
class(mix[[2]])
class(mix[[3]])
class(mix[[4]])


#matrix

num=(1:9)
matrix(data=num,nrow = 3,ncol=3)
class(matrix(data=num,nrow = 3,ncol=3))

#character matrix

c('a','b','c','d','e','f')->alpha
matrix(data = alpha,ncol = 3,nrow=2)->mat
class(mat)
mat[[5]]

#list matrix

list('a',1,'b',T,F,2.2)->mix
matrix(data = mix,nrow=3,ncol=2)

matrix(list('a',1,'b',T,F,2.2),nrow=3,byrow = T)


#ARRAY
num<-c(1:9)
num2<-c(10:18)
#dim=c(row,column,how many matrix)
array(data = c(num,num2),dim=c(3,3,2))->arr

#accessing elements
arr[2,2,1]
arr[3,3,1]
arr[3,3,2]


#---------------DATA MANIPULATION-----------------

library(dplyr)


# select()

select(churn,2)->c_2

select(churn,tenure)->c_tenure

select(churn,c(1,3,6,8))->c_nu

head(select(churn,1:5))

select(churn,c(4:10))->y
head(y)

select(churn,c(gender,tenure,Churn))
select(churn,gender:Churn)
select(churn,gender,tenure,Churn)->r
head(r)


select(churn,starts_with('Stream'))->stre
head(stre)

select(churn,ends_with("Charges"))->charges
head(charges)


# filter()

filter(churn,gender=='Female')->female
count(female)


filter(churn,MonthlyCharges>100)->c_100
count(c_100)


filter(churn,gender=='Female' & MonthlyCharges>100)->fe_100
count(fe_100)

filter(churn,StreamingTV=='Yes' & StreamingMovies=='Yes')->yes_stream
count(yes_stream)

#filter customer whose tenure is greater then 50 and internet service is DSL 
# and monthly charges is less then 100
filter(churn,tenure>50 & InternetService=='DSL' & MonthlyCharges<100)->oo
count(oo)

filter(churn,PaymentMethod=='Mailed check'| PaymentMethod=='Bank transfer (automatic)')->or
count(or)

filter(churn,Contract=='One year' | Contract=="Two year" & gender=='Male')->male_cust
count(male_cust)

# mutate()
# mutate is used to add column in dataset

mutate(churn,
       Age = ifelse(SeniorCitizen==0,
                    sample(x=16:55),
                    sample(x=56:100))
)->churn



mutate(churn,
       cust_cat=ifelse(MonthlyCharges<45,
                       'LowPaying',
                       ifelse(
                         MonthlyCharges<90,
                         "Medium Paying",
                         "High Paying"
                       )
                       )
       )->churn

read.csv('F:/warehouse/study_warehouse/R/Data Set/datasets/customer_churn.csv')->churn


# sample()
#sample_n
#sample_frac

sample_n(churn,10)->rand_10
sample_n(churn,100)->rand_100

# sample of custmer using % of customer
sample_frac(churn,.10)->rand_0.10
sample_frac(churn,.50)->rand_0.50


count(churn,Contract)
count(churn,gender)
count(churn,Dependents)

# summarise()

summarise(churn,nin_tenure=min(tenure),max_tenure=max(tenure))


summarise(churn,min_monthly=min(MonthlyCharges))
summarise(churn,max_monthly=max(MonthlyCharges))

summarise(churn,mean_monthly=mean(MonthlyCharges,na.rm=T))
summarise(churn,median_monthly=median(MonthlyCharges,na.rm = T))

summarise(group_by(churn,PaymentMethod),mean_tenure=mean(tenure))
summarise(group_by(churn,InternetService),mean_tenure=mean(tenure))
summarise(group_by(churn,InternetService),mean_tenure=mean(MonthlyCharges))
summarise(group_by(churn,Partner),mean_tenure=mean(TotalCharges,na.rm=T))


# pipe operator >>>   %>%
 # pipe is used to compine operations
churn %>% select(1:5) -> c_15

churn %>% select(1:5) %>% filter(gender=='Male')->male_5

churn %>% filter(gender=='Female' & Partner=='Yes')->female_partner

churn %>% select(1,2,4,5) %>%filter(gender=='Female' & Partner=='Yes')->female_partner


churn %>% group_by(PaymentMethod) %>% summarise(mean_tenure=mean(tenure)) %>%
  arrange(desc(PaymentMethod))

churn %>% select(1,2,10:21) %>% filter(Contract=='One year'| Contract=='Two year')%>%
  arrange(Contract)->c_contract


churn %>% filter(PaperlessBilling=='No') %>% group_by(TechSupport) %>% summarise(mean_tenure=mean(tenure))






# sqldf
#install.packages("sqldf")
library(sqldf)

sqldf('select Churn from churn')->c_Churn
View(c_Churn)

sqldf('select gender,Partner,MonthlyCharges from churn')->c_3
View(c_3)

sqldf('select * from churn')->all
View(all)

sqldf('select * from churn where gender=="Male"')->sql_male
View(sql_male)

sqldf("select * from churn where tenure>50")->greater_50
View(greater_50)
range(greater_50$tenure)

sqldf("select * from churn where InternetService=='DSL'")->dsl_cust
table(dsl_cust$InternetService)


sqldf('select * from churn where gender=="Male" and Contract=="One year"  ')->c_male1
View(c_male1)

sqldf("select * from churn where SeniorCitizen==1 and InternetService=='Fiber optic' and TechSupport=='Yes'")->c_sie
View(c_sie)


sqldf("select * from churn where Contract=='One year' or Contract=='Two year' and gender=='Female'")->fe_cont


sqldf("select avg(tenure),PaymentMethod from churn group by PaymentMethod")->goup
goup

sqldf("select avg(MonthlyCharges),Contract from churn group by Contract")


# DATA Visualiation__________________

read.csv('F:/warehouse/study_warehouse/R/Data Set/datasets/customer_churn.csv',stringsAsFactors = T)->churn

##############  FROM BASE PAKAGE ##############

plot(churn$InternetService)
plot(churn$InternetService,col='coral')
plot(churn$InternetService,col='coral',xlab="Dependents",main="Distribution of dependents")


plot(churn$PhoneService)
plot(churn$PhoneService,col='palegreen')
plot(churn$PhoneService,col='steelblue2',xlab='Phone Service',main='Distribution of phone Service',ylab='Frequency')

plot(churn$InternetService,col='steelblue2',xlab='Phone Service',main='Distribution of Internet Service',ylab='Frequency')

plot(churn$Contract,col='aquamarine',xlab='Contract',main='Distribution of Contract',ylab='Frequency')

#histogram
hist(churn$tenure,col = 'steelblue3')
hist(churn$tenure,col = 'steelblue3',breaks = 30)# breaks define bins
hist(churn$tenure,col = 'steelblue3',xlab = 'tenure',main = 'Distribution Of Tenure')

hist(churn$MonthlyCharges)
hist(churn$MonthlyCharges,col='palevioletred')
hist(churn$MonthlyCharges,col='palevioletred',breaks=50)

#density plot
plot(density(churn$tenure))
plot(density(churn$tenure),col='blue')
plot(density(churn$MonthlyCharges),col='red')



######FROM GGPLOT2 ############ 
library(ggplot2)

#histogram

ggplot(data=churn,aes(x=tenure))+geom_histogram()
ggplot(data=churn,aes(x=tenure))+geom_histogram(bins = 50,col='red',fill='black') # col for boundary color
ggplot(data=churn,aes(x=tenure))+geom_histogram(bins = 50,col='black',fill='red') # fill for bar color

ggplot(data=churn,aes(x=tenure,fill=Partner))+geom_histogram(bins = 50,col='black')
ggplot(data=churn,aes(x=tenure,fill=OnlineSecurity))+geom_histogram(bins = 50,col='black')

ggplot(churn,aes(x=tenure,fill=Contract))+geom_histogram(bins = 50,col='black')

ggplot(churn,aes(x=MonthlyCharges))+geom_histogram()
ggplot(churn,aes(x=MonthlyCharges))+geom_histogram(bins=50)
ggplot(churn,aes(x=MonthlyCharges))+geom_histogram(bins=50,col='black')
ggplot(churn,aes(x=MonthlyCharges))+geom_histogram(bins=50,col='red',fill='yellow')
ggplot(churn,aes(x=MonthlyCharges,fill=PhoneService))+geom_histogram(bins=50)
ggplot(churn,aes(x=MonthlyCharges,fill=PhoneService))+geom_histogram(bins=50,col='black',position ='identity' )
ggplot(churn,aes(x=MonthlyCharges,fill=InternetService))+geom_histogram(bins=50,col='black')
ggplot(churn,aes(x=MonthlyCharges,fill=StreamingTV))+geom_histogram(col='black',position = 'identity')

ggplot(churn,aes(x=TotalCharges))+geom_histogram(bins = 50)
ggplot(churn,aes(x=TotalCharges))+geom_histogram(bins = 50,fill='firebrick4',col='gold')
ggplot(churn,aes(x=TotalCharges,fill=DeviceProtection))+geom_histogram()
ggplot(churn,aes(x=TotalCharges,fill=DeviceProtection))+geom_histogram(col='white')


#geom_bar()

ggplot(churn,aes(x=SeniorCitizen))+geom_bar()

ggplot(churn,aes(x=Contract))+geom_bar()
ggplot(churn,aes(x=Contract))+geom_bar(col='white',fill='steelblue3')
ggplot(churn,aes(x=Contract,fill=InternetService))+geom_bar(col='white')
ggplot(churn,aes(x=Contract,fill=Contract))+geom_bar(col='white')
ggplot(churn,aes(x=Contract,fill=StreamingTV))+geom_bar(col='white')
ggplot(churn,aes(x=Contract,fill=PaymentMethod))+geom_bar(col='white')
ggplot(churn,aes(x=SeniorCitizen,fill=PaymentMethod))+geom_bar(col='white',position = 'dodge')
ggplot(churn,aes(x=SeniorCitizen,fill=InternetService))+geom_bar(col='white',position = 'dodge')

ggplot(churn,aes(x=gender))+geom_bar()
ggplot(churn,aes(x=gender,fill=InternetService))+geom_bar(col='white',position = 'dodge')
ggplot(churn,aes(x=gender,fill=InternetService))+geom_bar(col='white')
ggplot(churn,aes(x=gender,fill=StreamingTV))+geom_bar(col='white')
ggplot(churn,aes(x=gender,fill=StreamingTV))+geom_bar(col='white',position = 'dodge')

ggplot(churn,aes(x=Dependents))+geom_bar(col='white')
ggplot(churn,aes(x=Dependents))+geom_bar(col='white',fill='chocolate')
ggplot(churn,aes(x=Dependents,fill=gender))+geom_bar()
ggplot(churn,aes(x=Dependents,fill=InternetService))+geom_bar(position='dodge')


# geom_points()   or scatter plots

ggplot(churn,aes(x=tenure,y=TotalCharges))+geom_point()
ggplot(churn,aes(x=tenure,y=TotalCharges))+geom_point(col='red')
ggplot(churn,aes(x=tenure,y=TotalCharges,col=Partner))+geom_point() # col attribute is used in aes insteed of fill in scatter plot
ggplot(churn,aes(x=tenure,y=TotalCharges,col=Churn))+geom_point()
ggplot(churn,aes(x=tenure,y=TotalCharges,col=InternetService))+geom_point()
ggplot(churn,aes(x=tenure,y=TotalCharges,col=PaymentMethod))+geom_point()
ggplot(churn,aes(x=tenure,y=TotalCharges,col=PaymentMethod))+geom_point(alpha=.5)

ggplot(churn,aes(x=MonthlyCharges,y=TotalCharges,col=PaymentMethod))+geom_point()
ggplot(churn,aes(x=MonthlyCharges,y=TotalCharges,col=Dependents))+geom_point()
ggplot(churn,aes(x=MonthlyCharges,y=TotalCharges,col=Dependents))+geom_point(shape=4)
ggplot(churn,aes(x=MonthlyCharges,y=TotalCharges,col=InternetService))+geom_point(shape=7)

#internet service is mapped with shape attribute
ggplot(churn,aes(x=MonthlyCharges,y=TotalCharges,col=InternetService,shape=InternetService))+geom_point()
ggplot(churn,aes(x=MonthlyCharges,y=TotalCharges,col=InternetService,shape=InternetService,size=InternetService))+geom_point()
ggplot(churn,aes(x=MonthlyCharges,y=TotalCharges,col=InternetService,shape=InternetService))+geom_point()+geom_smooth()


#boxplot()
read.csv('F:/warehouse/study_warehouse/R/Data Set/datasets/customer_churn.csv',stringsAsFactors = T)->churn


ggplot(churn,aes(y=MonthlyCharges,x=SeniorCitizen))+geom_boxplot()
ggplot(churn,aes(y=MonthlyCharges,x=gender))+geom_boxplot(fill='palegreen4',outlier.colour = 'orange')
ggplot(churn,aes(y=MonthlyCharges,x=Dependents))+geom_boxplot(fill='palegreen4',outlier.colour = 'orange')


ggplot(churn,aes(y=MonthlyCharges,x=PaymentMethod))+geom_boxplot(fill='pink',col='red',outlier.colour = 'red')

ggplot(churn,aes(y=MonthlyCharges,x=InternetService))+geom_boxplot(fill='steelblue3',col='steelblue4',outlier.colour = 'red')
ggplot(churn,aes(y=MonthlyCharges,x=PaymentMethod,fill=Dependents))+geom_boxplot()
ggplot(churn,aes(y=MonthlyCharges,x=PaymentMethod,fill=PaperlessBilling))+geom_boxplot(outlier.colour = 'red')
ggplot(churn,aes(y=MonthlyCharges,x=PaymentMethod,fill=Churn))+geom_boxplot(outlier.colour = 'red')
ggplot(churn,aes(y=MonthlyCharges,x=PaymentMethod,fill=TechSupport))+geom_boxplot(outlier.colour = 'steelblue4')
ggplot(churn,aes(y=MonthlyCharges,x=PaymentMethod,fill=InternetService))+geom_boxplot(outlier.colour = 'steelblue4')

############ FACET GRID #############

ggplot(churn,aes(x=tenure,fill=InternetService))+geom_histogram()->g1
g1+facet_grid(~InternetService)

ggplot(churn,aes(x=PaymentMethod,fill=Contract))+geom_bar()->g2
g2+facet_grid(~Contract)

#always use col while making scatter plot ::::REMEMBER
ggplot(churn,aes(x=tenure,y=TotalCharges,col=Contract))+geom_point()+geom_smooth()->g3
g3+facet_grid(~Contract)

ggplot(churn,aes(x=PaymentMethod,y=TotalCharges,fill=InternetService))+geom_boxplot()->g4
g4+facet_grid(~InternetService)

# Theme

ggplot(data=churn,aes(x=tenure))+geom_histogram(bins = 50,col='black',fill='red')->g1
g1+labs(title = 'distribution')->g2
g2+theme(panel.background = element_rect(fill = 'palegreen'))->g3
g3+theme(plot.background = element_rect(fill = 'palegreen4'))->g4
g4+theme(plot.title = element_text(hjust = 0.5,face = 'bold',color='peachpuff'))


ggplot(churn,aes(x=tenure,y=TotalCharges,col=InternetService))+geom_smooth()->p1
p1+labs(title = "TotalCharges VS TENURE",x='Tenure')->p2
p2+theme(panel.background = element_rect(fill = 'thistle'))->p3
p3+theme(plot.background = element_rect(fill = 'tomato'))->p4
p4+theme(plot.title = element_text(hjust=0.5,face = 'bold',color = 'peachpuff'))


################# PLOTLY ##############
library(plotly)

read.csv('F:/warehouse/study_warehouse/R/Data Set/datasets/customer_churn.csv',stringsAsFactors = T)->churn
#always use ~ sign shown BELOW
plot_ly(data=churn,x=~InternetService,type = 'histogram')

plot_ly(data=churn,x=~InternetService,type = 'histogram',color=~InternetService)
plot_ly(data=churn,x=~InternetService,type = 'histogram',color=~Contract)
plot_ly(data=churn,y=~TotalCharges,x=~tenure,mode='markers') # scatter plot
plot_ly(data=churn,y=~TotalCharges,x=~tenure,mode='markers',color=~InternetService) # scatter plot
plot_ly(data=churn,y=~TotalCharges,x=~tenure,mode='markers',color=~InternetService) # scatter plot
plot_ly(data=churn,y=~MonthlyCharges,x=~InternetService,type='box',color=~InternetService)


################ STATISTICS #############
mean(churn$tenure)
mean(churn$MonthlyCharges,na.rm = T)
mean(churn$TotalCharges)
typeof(churn$tenure)
typeof(churn$MonthlyCharges)
as.integer(churn$TotalCharges)->churn$TotalCharges
mean(churn$TotalCharges,na.rm = T)
typeof(churn$MonthlyCharges)
typeof(churn$TotalCharges)
IQR(churn$tenure)
IQR(churn$TotalCharges,na.rm = T)
sd(churn$tenure)
sd(churn$TotalCharges,na.rm = T)

grep("milk.+",c("cow's milk","milkshake","milky","cat","milk1","milk"),value = T)




strsplit()


myfun<-function(x,y=2){
  x=y+100
  y=y+100
  return(y)
}
myfun(3)

c(0,1,2,3,4,5)[2:4]


chance_precipitation <- 0.80
if( chance_precipitation > 0.5 ) {
  print("Bring an umbrella") } else {
    print("Don't bring an umbrella")}


c(1,2)->c
c==1  
costs <- c(3, 15, 3, 10)
costs[c(10,15)]
costs(cost>5)
costs>5
matrix(c(1:25),5,5)->m
m[5,1]
john <- list("studentid" = 9, "age" = 18, "courses" = c("Data Science 101", "Data Science Methodology"))
john$courses
john["courses"]
john[3]
data.frame("student" = c("john", "mary"), "id" = c(1, 2))

