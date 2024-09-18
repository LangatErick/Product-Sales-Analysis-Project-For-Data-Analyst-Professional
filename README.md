# Product-Sales-Analysis-Project-For-Data-Analyst-Professional

---
title: "Product_Sales_Analysis"
author: "ERICK@Guru"
date: "2023-07-029"
output:
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Product Sales Analysis Project For Data Analyst Professional

### Using the validation criteria, the following validation was made:

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

head(sales)
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

