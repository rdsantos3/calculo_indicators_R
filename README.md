

**SCL Data - Data Ecosystem Working Group**

[![IDB Logo](https://scldata.iadb.org/assets/iadb-7779368a000004449beca0d4fc6f116cc0617572d549edf2ae491e9a17f63778.png)](https://scldata.iadb.org)

# Welcome to idb social indicators
 
The objective of this repository is to compute all the indicators of the social sector, in a clean and organized way. 

## Contribute

To start contributing to this repository the first step is to clone it. 
Then we will work with two main branches 1. main and 2. development. 

The development branch will be the most up-to-date branch. A person contributing should pull this branch and construct a second branch to perform a given task. For instance, if a person is reviewing the geographical disaggregations she/he will pull the development branch and create a personal branch "geographic_dis_scl". 

The person will work on these tasks and then commit and push the changes to the development branch. Finally making a pull request so that another person can review it and accept it as the new version of development.

The next table has all the tasks in the project. A person who wants to work on a to-do has two assign it in the issue tab so that everyone knows who is dealing with each task and we work most effectively.


 
| Task | Objective | Status |
| :---: | :--- | :--- | 
| If census and surveys | Add an if condition to read census and surveys depending on the user |To do | 
| Add strata/UPM | When available consider strata and UPM to compute indicators | To do | 
| Add the rest of the indicators type | Add the other functions needed to compute an indicators scl_mean, scl_rate, scl_gini | To do | 
| Geographic desagreggation| Separate geographic disaggregation from the rest | To do | 
| Improve performance SOC.R | Row names functions make the code slow at the beginning | To do |
| Complete the intermediate variables - EDU | Make sure all intermediate variables are in the code | To do |
| Complete the intermediate variables - GDI | Make sure all intermediate variables are in the code | To do |
| Complete the intermediate variables - MIG| Make sure all intermediate variables are in the code | To do |
| Complete the intermediate variables - LMK | Make sure all intermediate variables are in the code | To do |
| Complete the intermediate variables - SCL | Make sure all intermediate variables are in the code | To do |
| Include wash indicators | Include all the WASH intermediate vars and indicators | To do | 
| Make a master code | Construct a code that only needs the input collection year and country and runs | To do | 
| Complete indicators csv | Make sure all the indicators are in the csv with their disaggregations | To do | 


If there is a to-do you think is missing from the list please add it. 

## Structure 

The code has four main parts.

1. Intermediate variables. There is one code per division, and each code has all the necessary variables to complete the indicators for said division. Each division must be certain that its code contains sufficient variables. 

2. Inputs. The code takes the dictionary of the surveys and censuses collections to filter or limit the data frame when computing the indicators. There are many variables that aren't needed when performing these tasks; thus, removing them makes the code more efficient. 

3. scl_indicators.R Here are the functions to compute the indicators.

4. master. Not done yet, but this will run the scl_indicators and each of the intermediate codes, only taking as an input year and country.
