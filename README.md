# Code for: "Analysis and Reliability Prediction of Degradation Processes with Imperfect Maintenance"

![License](https://img.shields.io/badge/license-MIT-blue.svg)

## 📖 Description

This repository contains the R code and data necessary to reproduce the analyses and results presented in the academic paper:

> **Reliability Analysis of Repairable Systems Using the Arithmetic Reduction of Degradation with Memory One (ARD$_1$) Model: An Application to Industrial Bag Filter Data**


The codebase provides a framework for modeling system degradation using **Wiener Processes** under a scheme of **imperfect maintenance**. The primary functionalities include:
1.  **Data Preprocessing**: A function to clean, filter, and sample raw time-series data.
2.  **Parameter Estimation**: Maximum Likelihood Estimators (MLE) for the model parameters (drift μ, variance σ², and maintenance effectiveness ρ).
3.  **Reliability Prediction**: Calculation and visualization of survival probability curves based on the estimated parameters.

## 📁 Project Structure

The repository is organized as follows:

```
/
├── dataset/
│   └── bagfilter_dataset.xlsx  # Location for the raw data file
├── functions.R                 # Script with all helper functions
├── main.R                      # Main script that runs the full analysis
└── README.md                   # This documentation file
```

## Installation and Setup

### Prerequisites
- **R**: Version 4.0 or higher.
- **RStudio**: Recommended for the best user experience.

### Steps

1.  **Clone the repository:**
    ```bash
    git clone [https://github.com/GeorgeSantos1/bag-filter-reliability.git](https://github.com/GeorgeSantos1/bag-filter-reliability.git)
    cd bag-filter-reliability
    ```

3.  **Install R Packages**:
    Open the project in RStudio. The required packages will be **automatically installed and loaded** the first time you run the `main.R` script (via `source("functions.R")`). The required packages are:
    - `readxl`
    - `dplyr`
    - `lubridate`
    - `tidyr`
    - `purrr`
    - `ggplot2`
    - `statmod`

## How to Use

The main analysis workflow is contained in the `main.R` script.

1.  **Open the Project**: Open the `.Rproj` file (if one exists) or the project folder in RStudio.
2.  **Configure Parameters**: Open the `main.R` file. You can adjust parameters in the script's sections, such as:
    - The data file path in the `preprocess_bag_filter_data` function call.
    - The simulation parameters in Section 1 (if you want to test the validation).
    - The failure threshold (`thresholds`) in Section 3 for the reliability analysis.
3.  **Run the Analysis**: Execute the entire script in the R console:
    ```r
    source("main.R")
    ```
4.  **Check the Output**:
    - The estimated parameters (μ, σ², ρ) will be printed to the console.
    - The reliability curve plot will be generated and displayed in the "Plots" tab in RStudio.

## Author

- **George Anderson A. dos Santos** - *Development and Analysis* - [https://github.com/GeorgeSantos1](https://github.com/GeorgeSantos1)
