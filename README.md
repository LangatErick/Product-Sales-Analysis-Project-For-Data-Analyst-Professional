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
