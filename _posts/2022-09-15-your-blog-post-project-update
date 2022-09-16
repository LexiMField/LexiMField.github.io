## Project 1 Reflection

I have just completed my first ST 558 project- well, complete is a strong word, but I turned it in. 
Frankly, I struggled immensely with this project and spent an estimated 50-60 hours working on it. 
I learned how a lot of new tools such as how to build a function, create a wrapper, plot data, and put coding skills together, but it was very apparent that I have a long way to go when it comes to learning R. The project both increased my confidence in my abiloty to problem solve but also decreased my confidence in my coding abilities. The biggest issue I had was not being able to complete my plotting functions and therefore the plotting part of the project was left uncompleted. I am proud of myself for completing the parts I was able to and I feel much more confident in creating and calling functions as well as sorting and filtering through data sets.

I think the main purpose of the project was to get us to push ourselves and our abilities in R by having us combine multiple skills and tools we have learned via the notes. The project had us use data sets and practice selecting only the information we wanted from them, combine data sets, plot data sets, and much more but I think the overall goal was to force us to use logic to apply basic R commands and functions in a way which coded a successful project.

Below I have included the Rmd code for my project, but I was not able to successfully create an html output so I am unable to link that here.

---
title: "Project 1 ST 558"
author: "Lexi Field"
date: "`r Sys.Date()`"
output: 
  html_document:
    toc: true
    toc_depth: 3
    toc_float:true
    code_folding: show
    df_print: tibble
    theme: readable
---

# Project 1

```{r}
library(readr)
sheet1 <- read_csv("https://www4.stat.ncsu.edu/~online/datasets/EDU01a.csv")
```

Here is where I have read in the data provided in the instructions. Next I will print the data and select the columns I want. Below I have also renamed Area_name to area_name and switched to long format. In the following code I parsed out the data to break it into the survey, the type of value and the data the information is from. I have also fixed the year to include the century information and added a date column.

```{r}
library(readr)
data1 <-
  sheet1 %>%
  rename("area_name" = Area_name) %>%
  select("area_name", "STCOU", ends_with("D")) %>%
  group_by(area_name) %>%
  pivot_longer(cols=3:12,  names_to = "variables", values_to ="enrollment")

print(data1)

data1$survey <- substr(data1$variables, start=1, stop=3)
data1$valuetype <- substr(data1$variables, start=4, stop=7)
data1$year<- substr(data1$variables, start=8, stop=9)


data1$year<- if_else(data1$year == (00:10),paste("20", data1$year, sep = ""),paste("19", data1$year, sep = "")) 


data1 %>%
  mutate(date=as.Date(year, format="%Y"))

```

## Creating two data sets

In the code below, I have used the code given to establish county data and divide the data set into two which represent the county and state data respectively. I have also created a column and divided states into their census divisions by creating groups. I manually grouped the states by typing in each state name and assigning it to the group number correlating it to its division number according to the census wikipedia information.

```{r}
countydata <- data1[grep(pattern = ", \\w\\w", data1$area_name), ]
class(countydata) <- c("county", class(countydata))
statedata <- data1[grep(pattern = "^[A-Za-z ]*$", data1$area_name), ]
class(statedata) <- c("state", class(statedata))
countydata$state <- substr(countydata$area_name, start = nchar(countydata$area_name)-2, stop=nchar(countydata$area_name))

group1 <- c("CONNECTICUT", "MAINE", "MASSACHUSETTS", "NEW HAMPSHIRE", "RHODE ISLAND", "VERMONT")
group2 <- c("NEW JERSEY", "NEW YORK"," PENNSYLVANIA")
group3 <- c("ILLINOIS", "INDIANA", "MICHIGAN", "OHIO", "WISCONSIN")
group4 <- c("IOWA", "KANSAS", "MINNESOTA", "MISSOURI", "NEBRASKA", "NORTH DAKOTA", "SOUTH DAKOTA")
group5 <- c("DELAWARE","FLORIDA", "GEORGIA", "MARYLAND", "NORTH CAROLINA", "SOUTH CAROLINA", "VIRGINIA", "WASHINGTON DC", "WEST VIRGINIA")
group6 <- c("ALABAMA", "KENTUCKY", "MISSISSIPPI", "TENNESSEE")
group7 <- c("ARKANSAS", "LOUISIANA", "OKLAHOMA", "TEXAS")
group8 <- c("ARIZONA", "COLORADO", "IDAHO", "MONTANA", "NEVADA", "NEW MEXICO", "UTAH", "WYOMING")
group9 <- c("ALASKA", "CALIFORNIA", "HAWAII", "OREGON", "WASHINGTON")

statedata$division <- 
statedata$division[statedata$area_name %in% group1] <- 1
statedata$division[statedata$area_name %in% group2] <- 2
statedata$division[statedata$area_name %in% group3] <- 3
statedata$division[statedata$area_name %in% group4] <- 4
statedata$division[statedata$area_name %in% group5] <- 5
statedata$division[statedata$area_name %in% group6] <- 6
statedata$division[statedata$area_name %in% group7] <- 7
statedata$division[statedata$area_name %in% group8] <- 8
statedata$division[statedata$area_name %in% group9] <- 9
statedata$division[statedata$area_name %in% "UNITED STATES"] <- "ERROR"

```

## Requirements Section

Below I have read in the second liked data set from the instructions.

```{r}
sheet2 <- read_csv("https://www4.stat.ncsu.edu/~online/datasets/EDU01b.csv")
```

### Function for steps 1 & 2

Here I am writing a function which does steps 1&2. This selects the desired columns and pivots the data into long format.

```{r}
function_for_step_1_2 <- function(sheet) {
  dataoutput <-
    rename("area_name" = Area_name) %>%
    select("area_name", "STCOU", ends_with("D")) %>%
    group_by(area_name) %>%
    pivot_longer(cols=3:12,  names_to = "variables", values_to ="enrollment")
  return(dataoutput)
}
```

### Function for step 3

Here I am writing a function which parses the string to make columns representing different parts of the data and creating a date column by converting the year into a numeric value.

```{r}
function_for_step_3 <- function(output_function_for_step_1_2) {
  dataoutput<-
  data1$survey <- substr(data1$variables, start=1, stop=3)
  data1$valuetype <- substr(data1$variables, start=4, stop=7)
  data1$year<- substr(data1$variables, start=8, stop=9)
return(dataoutput)
}
```

### Function for step 5

In this code I am creating a function which adds a column that tells us what state each county is in by using the two letter abbreviation for the state.

```{r}
function_for_step_5 <- function(sheet) {
  dataoutput <-
  class(statedata) <- c("state", class(statedata))
  countydata$state <- substr(countydata$area_name, start =nchar(countydata$area_name)-2, stop=nchar(countydata$area_name))
  return(dataoutput)
}
```

### Function for step 6

Here I am creating a function which groups states into their census divisions and adds a column to indicate which division the state is in. I took the code I used prior to do this and turned it into a function.

```{r}
function_for_step_6 <- function(sheet){
  dataoutput<-
  group1 <- c("CONNECTICUT", "MAINE", "MASSACHUSETTS", "NEW HAMPSHIRE", "RHODE ISLAND", "VERMONT")
  group2 <- c("NEW JERSEY", "NEW YORK"," PENNSYLVANIA")
  group3 <- c("ILLINOIS", "INDIANA", "MICHIGAN", "OHIO", "WISCONSIN")
  group4 <- c("IOWA", "KANSAS", "MINNESOTA", "MISSOURI", "NEBRASKA", "NORTH DAKOTA", "SOUTH DAKOTA")
  group5 <- c("DELAWARE","FLORIDA", "GEORGIA", "MARYLAND", "NORTH CAROLINA", "SOUTH CAROLINA", "VIRGINIA", "WASHINGTON DC", "WEST VIRGINIA")
  group6 <- c("ALABAMA", "KENTUCKY", "MISSISSIPPI", "TENNESSEE")
  group7 <- c("ARKANSAS", "LOUISIANA", "OKLAHOMA", "TEXAS")
  group8 <- c("ARIZONA", "COLORADO", "IDAHO", "MONTANA", "NEVADA", "NEW MEXICO", "UTAH", "WYOMING")
  group9 <- c("ALASKA", "CALIFORNIA", "HAWAII", "OREGON", "WASHINGTON")

  statedata$division <- 
  statedata$division[statedata$area_name %in% group1] <- 1
  statedata$division[statedata$area_name %in% group2] <- 2
  statedata$division[statedata$area_name %in% group3] <- 3
  statedata$division[statedata$area_name %in% group4] <- 4
  statedata$division[statedata$area_name %in% group5] <- 5
  statedata$division[statedata$area_name %in% group6] <- 6
  statedata$division[statedata$area_name %in% group7] <- 7
  statedata$division[statedata$area_name %in% group8] <- 8
  statedata$division[statedata$area_name %in% group9] <- 9
  statedata$division[statedata$area_name %in% "UNITED STATES"] <- "ERROR"
  
  return(dataoutput)
}
```

### Function for steps 4,5,6

This function takes the output from step 3, which is where we pulled out strings from the variable to make new columns and does step 4, creating two new tibbles as well as executing steps 5 and 6 by calling upon the codes created to do these steps

```{r}
function_for_step_4_5_6 <-function(output_function_for_step_3) {
  dataoutput<-
  countydata <- data1[grep(pattern = ", \\w\\w", data1$area_name), ] %>%
  class(countydata) <- c("county", class(countydata)) %>%
  statedata <- data1[grep(pattern = "^[A-Za-z ]*$", data1$area_name), ] %>%
  return(output) %>%
  function_for_step_5(output) %>%
  function_for_step_6(output) %>%
  return(output)
}

```

# Wrapper

In the code below I have placed all the functions I created above and then put them into a wrapper function which will serve to complete all of the previous functions together for a data set when called upon. At the end of the code chunk I called it on sheet3 and dataset1 to test it out.

```{r}
library(readr)
function_for_step_1_2 <- function(sheet, data_name) {
  dataoutput <-
    sheet %>%
    rename("area_name" = Area_name) %>%
    select("area_name", "STCOU", ends_with("D")) %>%
    group_by(area_name) %>%
    pivot_longer(cols=3:12,  names_to = "variables", values_to = data_name)
  return(dataoutput)
}
function_for_step_3 <- function(sheet) {
  sheet$survey <- substr(sheet$variables, start=1, stop=3) 
  sheet$valuetype <- substr(sheet$variables, start=4, stop=7) 
  sheet$year<- substr(sheet$variables, start=8, stop=9)
  
  sheet$year<- if_else(sheet$year == (00:10),paste("20", sheet$year, sep = ""),paste("19", sheet$year, sep = "")) 
  
  return(sheet)
  
}

function_for_step_5 <- function(countydata) {
  #class(countydata) <- c("state", class(countydata))
  countydata$state <- substr(countydata$area_name, start= nchar(countydata$area_name)-2, stop= nchar(countydata$area_name))
  return(countydata)
}

function_for_step_6 <- function(statedata){
  group1 <- c("CONNECTICUT", "MAINE", "MASSACHUSETTS", "NEW HAMPSHIRE", "RHODE ISLAND", "VERMONT")
  group2 <- c("NEW JERSEY", "NEW YORK"," PENNSYLVANIA")
  group3 <- c("ILLINOIS", "INDIANA", "MICHIGAN", "OHIO", "WISCONSIN")
  group4 <- c("IOWA", "KANSAS", "MINNESOTA", "MISSOURI", "NEBRASKA", "NORTH DAKOTA", "SOUTH DAKOTA")
  group5 <- c("DELAWARE","FLORIDA", "GEORGIA", "MARYLAND", "NORTH CAROLINA", "SOUTH CAROLINA", "VIRGINIA", "WASHINGTON DC", "WEST VIRGINIA")
  group6 <- c("ALABAMA", "KENTUCKY", "MISSISSIPPI", "TENNESSEE")
  group7 <- c("ARKANSAS", "LOUISIANA", "OKLAHOMA", "TEXAS")
  group8 <- c("ARIZONA", "COLORADO", "IDAHO", "MONTANA", "NEVADA", "NEW MEXICO", "UTAH", "WYOMING")
  group9 <- c("ALASKA", "CALIFORNIA", "HAWAII", "OREGON", "WASHINGTON")

  statedata$division <- 
  statedata$division[statedata$area_name %in% group1] <- 1
  statedata$division[statedata$area_name %in% group2] <- 2
  statedata$division[statedata$area_name %in% group3] <- 3
  statedata$division[statedata$area_name %in% group4] <- 4
  statedata$division[statedata$area_name %in% group5] <- 5
  statedata$division[statedata$area_name %in% group6] <- 6
  statedata$division[statedata$area_name %in% group7] <- 7
  statedata$division[statedata$area_name %in% group8] <- 8
  statedata$division[statedata$area_name %in% group9] <- 9
  statedata$division[statedata$area_name %in% "UNITED STATES"] <- "ERROR"
  
  return(statedata)
}

function_for_step_4_5_6 <-function(sheet) {
  countydata <- sheet[grep(pattern = ", \\w\\w", sheet$area_name), ]
  class(countydata) <- c("county", class(countydata))
  
  statedata <- sheet[grep(pattern = "^[A-Za-z ]*$", sheet$area_name), ]
  
  countydata <- function_for_step_5(countydata)
  statedata <- function_for_step_6(statedata)
  return(list(countydata=countydata, statedata=statedata))
}

mywrapper <- function(url, data_name = "enrollment"){
  sheet <- read_csv(url)  
  steps_1_2 <- function_for_step_1_2(sheet, data_name)
  step_3 <- function_for_step_3(steps_1_2)
  dataoutput <- function_for_step_4_5_6(step_3)
  return(dataoutput)
  

}
sheet3 <- mywrapper("https://www4.stat.ncsu.edu/~online/datasets/EDU01b.csv")
print(sheet3)

dataset1 <-mywrapper("https://www4.stat.ncsu.edu/~online/datasets/EDU01a.csv")
print(dataset1)

```

# Call It and Combine

In thia section of code I am combining the two list outputs of the county data and two outputs of the state data. Then U wrote a combinedata function which will work with futures lists and tested the new function but combining sheet3 and dataset 1.

```{r}
list(countydata=bind_rows(sheet3$countydata,dataset1$countydata),statedata=bind_rows(sheet3$statedata,dataset1$statedata))

combinedata <-function(list1,list2){
  list(countydata=bind_rows(list1$countydata,list2$countydata),statedata=bind_rows(list1$statedata,list2$statedata))
}

finaldata <- combinedata(sheet3,dataset1)
finaldata

```

# Writing a Generic Function for Something

```{r}
plot.function #what is used for a class = function
getS3method("plot","data.frame") #what is used for a class = data frame
```

### Plotting

Below is the function I wrote for the state plotting method which puts the year value on the x axis and the mean of the Division stattistic on the y axis.

```{r}
plot.state <- function(statedata, var_name = "enrollment" ) {
    differenttablestate <- statedata  %>%
      group_by(year,division) %>%
      summarise(meanvariable=mean(get(var_name)))
      ggplot(filter(differenttablestate, division != "ERROR"), aes(x = as.numeric(year), y = meanvariable, color = division)) + geom_line()
}
plot(statedata)

```

Below is the function I have attemoted to write to plot the county data. This is supposed to allow the user to specify which state they want to survey and choose if they want to sort values returned by largest to smallest (when top is specified) or sort by smallest to largest (when bottom is specified). This function only obtains the top or bottom x number of area names, but the default is 5.

```{r}
plot.county <- function(df, var_name = "variable", state, top or bottom) {
  differenttablecounty <- countydata %>%
    group_by(area_name) %>%
    filter(state == ...) %>%
    summarise(meanvariable=mean(get(var_name))) %>%
    arrange()
    arrange(countydata, "variable", .by_group = ) %>%
  ggplot(differenttablecounty, aes(x = year, y = get(var_name), color = Area_name)) + geom_line()
  
}

plot(countydata)
```

# Put it Together

In the section below I am running my data processing function on the two URLs already given and combining them into a single object with two data frames. I am also using the plot function on the state and county data. I am not sure why my plot.state function is not working. I did not get a plot.county function together so below is where I would be using the plot function on the county data four times following the instructions given; Once specifying the state to be "PA", the group being the top, the number looked at being 7, Once specifying the state to be "PA", the group being the bottom, the number looked at being 4, Once without specifying anything (defaults used), Once specifying the state to be "MN", the group being the top, the number looked at being 10

```{r}
puttogether <- mywrapper("https://www4.stat.ncsu.edu/~online/datasets/EDU01a.csv")

puttogether2 <- mywrapper("https://www4.stat.ncsu.edu/~online/datasets/EDU01b.csv")

list(countydata=bind_rows(puttogether$countydata,puttogether2$countydata),statedata=bind_rows(puttogether$statedata,puttogether2$statedata))

finalputtogether <- combinedata(puttogether, puttogether2)

FPTplot <- plot.state(statedata, var_name = "enrollment")

FPTcounty <- plot.county(finalputtogether)
```

## Last Steps

Below I am running my data processing function on the four data sets using the URLs given and combining them into one object with two data frames using my combine function multiple times. Then I would be using the plot function for the state data frame and the plot function for the county data frame. However I cannot get my plot.state function to work and never finished the plot.county function.

```{r}
data_1_a <-mywrapper("https://www4.stat.ncsu.edu/~online/datasets/PST01a.csv")

data_1_b <- mywrapper("https://www4.stat.ncsu.edu/~online/datasets/PST01b.csv")

data_1_c <- mywrapper("https://www4.stat.ncsu.edu/~online/datasets/PST01c.csv")

data_1_d <- mywrapper("https://www4.stat.ncsu.edu/~online/datasets/PST01d.csv")

data_1_ab <- combinedata(data_1_a, data_1_b)

data_1abc <- combinedata(data_1_ab,data_1_c)

data_1abcd <- combinedata(data_1abc, data_1_d)

print(data_1abcd)

plot.state(data_1abcd, var_name = "enrollment")

plot.county()


```
