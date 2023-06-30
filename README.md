
# SCL Data - Data Ecosystem Working Group

[![IDB Logo](https://scldata.iadb.org/assets/iadb-7779368a000004449beca0d4fc6f116cc0617572d549edf2ae491e9a17f63778.png)](https://scldata.iadb.org)

## Welcome to IDB Social Indicators

This repository aims to provide a framework for computing social sector indicators in a clean, organized and repeatable manner. 

## Getting Started

To start contributing, first clone this repository. Then, we primarily work with two branches: 1) `main` and 2) `development`.

The `development` branch is the most current one. When contributing, pull from this branch and create a new personal branch for the specific task you are working on. For instance, if you are reviewing geographical disaggregations, create a branch named "geographic_dis_scl".

Work on the assigned tasks, commit and push the changes to the `development` branch. Make a pull request so that another team member can review it and accept it as the new version of `development`.

## Repository Structure 

This repository consists of three main parts.

1. **Intermediate Variables**: One script per division, each containing all necessary variables for computing the indicators of the corresponding division.

2. **Indicator Definitions (`idef.csv`)**: This file controls the computation of indicators. It contains the definition of each indicator.

3. **Running Scripts**: `scl_indicators.R` runs the function. `runningScript_loop.R` runs `scl_indicators.R` for a batch of countries.

## How to Contribute

1. Fork the repository to your GitHub account.

2. Clone the forked repository to your local machine.

3. Create a new branch for your tasks.

4. Make your changes and commit them to your branch.

5. Push the branch to your GitHub repository.

6. From the GitHub page of your forked repository, open a pull request to the `development` branch of the main repository.




