![image](https://github.com/user-attachments/assets/c9bb4484-c3cd-4f21-829c-15cad309087d)
---
title: "Product_Sales_Analysis"
author: "ERICK@Guru"
date: "2023-07-029"
---
# Product Sales Analysis Project For Data Analyst Professional
## Using the validation criteria, the following validation was made:
-   week: 6 unique values, without any missing data.
-   sales_method: had 5 unique values before validation: Email, Call, Email + Call, em + call, and email, which after validation were Email, Call, and Email + Call.
-   customer_id: 15,000 unique values. Needed no cleaning.
-   nb_sold: 10 unique values, no cleaning required and no missing values.
-   revenue: had 1074 missing values, of which the rows were removed from the data set.
-   years_as_customer: had two major values not corresponding: 47 and 63 which were way more than the number of years Pens and Printers has been in existence, 39 years. It made no sense having a customer when the business was not in existence. These rows were dropped.
-   nb_site_visits: Needed no cleaning.
-   state: Needed no cleaning too. At the end of the validation and cleaning process, the data that remained is 13,924 rows and 8 columns
## Data Import, Validation, Cleaning, and Exploration
```{r warning=FALSE, message=FALSE}
# install.packages("DataExplorer")
# install.packages("timetk")
# install.packages('DT')

#import Libraries
library(tidyverse)
library(janitor)
library(DataExplorer)
library(DT)
library(patchwork)
library(timetk)
# library(tinytex)
theme_set(theme_test())
```

```{r warning=FALSE, message=FALSE}
library(readr)
sales <- read_csv("product_sales.csv")
head(sales)#first 5 rows
```
![image](https://github.com/user-attachments/assets/5de8b150-63ef-42c9-b168-1c4edacc8e57)
```{r warning=FALSE, message=FALSE}
dim(sales)
```
- [1] 15000     8
- The data set contains **15,000** rows/observations and **8** columns/features before the cleaning and validation process.
```{r warning=FALSE, message=FALSE}
#check duplicates
sum(duplicated(sales$customer_id))#no duplicates
```
- [1] 0
```{r warning=FALSE, message=FALSE}
#Check mising values
colSums(is.na(sales)) %>% as.data.frame()#1074 missing values in #revenue column
```
![image](https://github.com/user-attachments/assets/db155f40-46d1-4a03-97b8-9cb8c5cee864)
```{r warning=FALSE, message=FALSE}
#Remove Missing values
sales <- na.omit(sales)
```
```{r warning=FALSE, message=FALSE}
# confirm if missing values still exist
colSums(is.na(sales)) %>% #No Missing values
        as.data.frame()
```
![image](https://github.com/user-attachments/assets/a322aa88-6dd6-4f67-bb6f-f65323a3194e)
```{r warning=FALSE, message=FALSE}
names(sales)# check column names
#Remove Column customer_id- because we do not it in our analysis
sales$customer_id <- NULL
```
- ![image](https://github.com/user-attachments/assets/dcb21f9c-a4f1-4684-84a5-d507acebe418)

```{r warning=FALSE, message=FALSE}
# Check the number of unque values in the column;week: 6 unique values, without any missing data.
unique(sales$week)# we've 6 unique values
```
- [1] 6 5 3 4 1 2

```{r warning=FALSE, message=FALSE}
#Unique values in the sales_method
# sales_method: had 5 unique values before validation: Email, Call, Email + Call, em + call, and email, which after validation were Email, Call, and Email + Call.

unique(sales$sales_method)
```
- [1] "Email + Call" "Call"         "Email"        "em + call"    "email"
```{r warning=FALSE, message=FALSE}
#Sales method now has 3 uniques values as per description
#So we need to clean the data as per the description
sales <- sales %>% 
     mutate(
       sales_method=ifelse(sales_method=="em + call","Email + Call",
                     ifelse(sales_method=="email","Email", sales_method)))

#Then we Check to confirm 
unique(sales$sales_method)
```
- [1] "Email + Call" "Call"         "Email"
```{r  warning=FALSE, message=FALSE}
# years_as_customer: had two major values not corresponding: 47 and 63 which were way more than the number of years Pens and Printers has been in existence, 39 years. It made no sense having a customer when the business was not in existence. These rows were dropped.
summary(sales$years_as_customer)
boxplot(sales$years_as_customer)
sales1 <- sales %>%  filter(!years_as_customer>39)#remove outliers(47,63)
```
![image](https://github.com/user-attachments/assets/4624abdf-e435-407f-a7e4-d7f2cd34a5cc)
**Our Data is Clean Now**
```{r warning=FALSE, message=FALSE}
sales_clean <- sales1
sales_clean$sales_method <- as.factor(sales_clean$sales_method)
glimpse(sales_clean)
#Reorder labels
# levels(sales_clean$sales_method) <- c("Email","Call" ,"Email + Call")
```
![image](https://github.com/user-attachments/assets/a2cf6614-6444-46eb-93e9-01da052a7e7f)
# Back to the Business Objectives
## **The Business goals**
1.  How many customers were there for each approach?
2.  What does the spread of the revenue look like overall? And for each method?
3.  Was there any difference in revenue over time for each of the methods?
4.  Based on the data, which method would you recommend we continue to use?
## **The Business Metrics**
## **The Recommendations**
### 1. How many customers were there for each sales method/ approach?
```{r warning=FALSE, message=FALSE}
sales_clean <- sales_clean %>%
  mutate(percentage = revenue / sum(revenue) * 100)
A1 <- sales_clean %>% 
  ggplot(aes(x=fct_infreq(sales_method)))+
  geom_bar(fill=rainbow(3))+
   geom_text(aes(label=after_stat(count)),                      
     stat='count',
     position=position_dodge(1.0),
     hjust=0.1,
    vjust= 1.6, 
     size=5)+
  theme(legend.position = 'bottom')+
   labs(
     x=' ',
     y='Sum of Each Approach',
     title = 'Number of Customers per Each Sales Method') +
 theme(plot.title = element_text(face = 'bold', 
                                 hjust = 0.5))#bold and center title
####################################
A2 <- sales_clean %>% 
  ggplot(aes(x=fct_infreq(sales_method)))+
  geom_bar(fill=rainbow(3)) +
     
  geom_text(aes(label = scales::percent(..count../sum(..count..))),
            stat = "count", 
            vjust = 1.8,
            position = position_stack(1),
            size = 6) +  # Adjust the position of labels
  theme(legend.position = 'bottom')+
   labs(
     x='',
     y=' ',
     title = 'Percentage(%) Count of Customers per Each Sales Method') +
    theme(plot.title = element_text(face = 'bold',# title in bold
                                    hjust = -1)) +# Center title
  theme(plot.title = element_text(face = 'bold', 
                                  hjust = 0.5))#bold and center title


A1/A2#combine plots
```
![image](https://github.com/user-attachments/assets/e07aee99-f38e-4940-9ecc-f353b27d7d1e)
- The Email sales method has the vast majority of 6921(50%) customers, followed by Call & Combination of Email and Call, with the respective count of 4780(34%) and 2223(16%).
## 2. What does the spread of the revenue look like overall? And for each method?
```{r}
x0 <-  sales_clean %>% 
  ggplot(aes(revenue, fill=sales_method))+
  geom_density()+
   ggtitle('Revenue Distibution')+
   theme(plot.title = element_text(hjust = 0.5,#Center Title
                                   face ='bold')) +# in bold
    theme(legend.position = 'none')

# install.packages("patchwork")
library(patchwork)
x1 <-  sales_clean %>% 
  ggplot(aes(revenue, fill=sales_method)) +
  geom_boxplot() +
  facet_wrap(~sales_method, scales = "free")+
   theme(legend.position = 'bottom')+
   ggtitle('Revenue And Sales Method') +
   theme(plot.title = element_text(face = 'bold',#title in bold
                                   hjust = 0.5))#center title
x0/x1#combine plots
```
![image](https://github.com/user-attachments/assets/0c06c01b-822d-4cb1-b015-896f5c902d40)
**Calls** is associated with lower Revenues in comparison to other methods. Both **Email** and (**Email+Call**) generates more revenues.
**Low revenues** were majorly generated by the **calls method**. This can be clearly seen on the Call graph above, with revenue ranging between 0-70
**Email sales_approach** generated revenues in the range of 80-120, with larger values start from 130-150.
**Both (Email + Call)** generate higher revenues ranging from 120-240 as seen from the histogram and boxplot for Email + call.
## 3. Was there any difference in revenue over time for each of the methods?

```{r warning=FALSE, message=FALSE}
d <- sales_clean %>% 
    group_by(sales_method) %>% 
  summarise(Total_Revenue=sum(revenue),
            Percen=round(sum(percentage), 1)) %>% 
  arrange(desc(Total_Revenue)) 

x2 <- DT::datatable(d)
x2
```
![image](https://github.com/user-attachments/assets/dfa2afca-18f0-46dc-8443-129df49624b2)
```{r warning=F, message=FALSE}
library(scales)
p1 <- d %>% 
  ggplot(aes(x=reorder(sales_method, desc(Total_Revenue)),
             y=Total_Revenue, fill=rainbow(3))) +
  geom_col()+
  theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::comma) +
 geom_text(aes(label = paste0(round(Percen, 1), "%")),
            vjust = -0.04,
          position = position_stack(0.5),
          hjust=1) +  # Adjust the position of labels
   xlab(' ')+
   ggtitle('Percentage Count of Total Revenue Per Sales Method') +
    theme(plot.title = element_text(hjust = 0.5,#center
                                    face = 'bold'))# bold 
###########################################################
p2 <- d %>% 
  ggplot(aes(x=reorder(sales_method, desc(Total_Revenue)),
             y=Total_Revenue, fill=rainbow(3))) +
  geom_col()+
  theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::comma) +
  geom_text(
    aes(label=Total_Revenue),
    position = position_stack(0.5),
    hjust=0.5,
    vjust=-.3) + 
  xlab(' ') +
   ggtitle('Total Revenue Per Sales Method') +
    theme(plot.title = element_text(hjust = 0.5,
                                    face = 'bold'))

p1/p2#combine plots
```
![image](https://github.com/user-attachments/assets/10c813b7-d0c2-4fea-b71a-d5f5b00c1a53)
- It can be seen that the Call method bring less revenue in comparison to the Email and (Email + Call) method. The Call method, generated only 17.4% of the total revenue as compared to the 31.2% generated through the Email + Call approach. But the Email sales method generate the higher revenue of 51.4% of the total revenue.
```{r warning=FALSE, message=FALSE}
library(scales)
d1 <- sales_clean %>% 
    arrange(week) %>% 
     group_by(week) %>% 
       summarise(Total_revenue = round(sum(revenue),0))

DT::datatable(d1)
```
![image](https://github.com/user-attachments/assets/41cd8cfe-8367-4299-baec-073e27bb6a06)

```{r warning=FALSE, message=FALSE}
z0 <- d1 %>% 
  ggplot(aes(x=week,
             y=Total_revenue,  fill=rainbow(6))) +
  geom_col() +
  scale_y_continuous(labels=scales::comma)+
  geom_text(aes(label=Total_revenue),
            position = position_stack(0.5),
           # hjust=1,
            vjust=-.2)+
  theme(legend.position = 'none')+
  xlab('Weekly Sales Total')+
  ggtitle("Total Sales Per Week") +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold"))
##############################################################
library(timetk)
z1 <- d1 %>%  plot_time_series(
  week, Total_revenue, .interactive = FALSE)

z0/z1#combine plots
```
![image](https://github.com/user-attachments/assets/fd042d87-30bf-4af9-b168-6de275582191)

```{r warning=FALSE, message=FALSE}
 
ab <-  sales_clean %>% 
   arrange(week ) %>% 
   group_by(week, sales_method) %>% 
   summarise(Total= sum(revenue)) %>% 
   ggplot(
     aes(x=week, y=Total, color=sales_method)) +
      geom_line(linetype = 2) +
      ggtitle("Weekly Sales Trend Per  Sales Method") +
   theme(plot.title = element_text(face="bold", hjust = 0.5)) +
   theme(legend.position = "bottom")
ab
```
![image](https://github.com/user-attachments/assets/91629c03-4ab2-4de3-9eaf-0f594316d8dd)
- Briefly we can that there is a positive increase in the revenue from 1st to 5th week for the Call and (Email + Call) sales methods, with a decline on week 6. While for the Email sales method we can see the negative revenue trend on a weekly basis, from week 1 to 6.
## **4. Relationship of Customer Tenure and the Total Revenue Generated.**
```{r}
sales_clean %>% 
  # select(years_as_customer, week, revenue) %>% 
  group_by(years_as_customer) %>% 
  mutate(Total_R=sum(revenue)) %>% 
  ggplot(
    aes(x=years_as_customer, y= Total_R, col=week)
  ) +
  geom_line() +
  ggtitle("Customer Tenure  And The Total Revenue Generated") +
  theme(legend.position = " ",
        plot.title = element_text(hjust = 0.5, face = "bold"))
  ```
![image](https://github.com/user-attachments/assets/0de9e834-9d33-406b-93bc-85d07f226732)
- The plot demonstrates the correlation between weekly revenue generated after product launch and customer tenure. The declining trend indicates that the majority of revenue comes from new customers who have been existing a period of 0 to 10 years.
## 5. Based on the data, which method would you recommend we continue to use?
```{r warning=FALSE, message=FALSE}
sales_clean %>% 
  ggplot(aes(x=week, y=revenue, colour = sales_method))+
  geom_line() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = 'bold', hjust = 0.5))
 ```
![image](https://github.com/user-attachments/assets/0f9e2e52-95d8-47bb-a35d-a520bb7db75b)
## **The Business Metrics**
Based on the analysis, I recommend discontinuing the Calls method and focusing only on the Email and Email + Call sales method. This is due to the higher sales and revenue generated by these approaches, as well as the shorter average time required per sale compared to calls approach which is (30 minutes). However, the Calls approach can still be used on condition the customer doesn't have an email address.
## **The Recommendations**
**Based on the analysis conducted using the provided data, the following recommendations are proposed:**
-   Monitor key metrics to track any changes in the sales approach.
-   It is recommended to utilize the Email method frequently to inform customers about new products. Additionally, follow-up calls in the second and third week can be made to discuss their needs and how the new product will assist them.
-   It is advisable to minimize the usage of the Call method or eliminate it altogether. This approach consumes more time for sales and ultimately generates the lowest revenue, despite having a higher number of sales.
-   The sales team should prioritize the Email and Email + Call approaches. As demonstrated in analysis, the Email sales approach yields the highest revenue during the initial three weeks, although it declines as the week progresses. To enhance sales and generate more revenue, a follow-up call should be made in the second or third week.
-   To broaden the customer segment, focus on enhancing marketing strategies and improving the conversion rate based on website visits. As indicated in the correlation graph, the longer customer tenure corresponds to lower revenue. To address this, onboard new customers and establish customer retention initiatives to boost sales and revenue from both new and existing customers.
-   Ensure accurate data collection to facilitate comprehensive analysis, particularly for revenue, which contains numerous missing values.
