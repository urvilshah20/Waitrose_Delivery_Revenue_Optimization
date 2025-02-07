# Waitrose_Delivery_Revenue_Optimization

### **Project Overview**

A revenue management system developed to optimize Waitrose's grocery delivery pricing strategies. The project implements dynamic pricing models and subscription pass analysis using R, focusing on peak/off-peak pricing optimization and customer willingness-to-pay analysis.

### **Business Context**

Waitrose, a premium British supermarket chain, faces increasing competition in the online grocery delivery space. With 329 stores and a 14% share of online deliveries among 13.7M total customers, there was a clear opportunity to optimize delivery pricing to improve revenue and market position.

### **Key Features**

- Dynamic pricing model for delivery slots based on peak/off-peak hours
- Willingness-to-pay (WTP) analysis using simulated customer data
- Development of MyWaitrose Delivery Pass subscription model
- Revenue projection and scaling calculations
- Comparative competitor analysis

### **Technologies Used**

- R
- Libraries:

  - ggplot2: Data visualization
  - dplyr & tidyverse: Data manipulation
  - nloptr: Optimization algorithms
  - stargazer: Statistical reporting



### **Methodology**

**1. Price Optimization**

- Generated sample data for 1000 consumers with normally distributed WTPs
- Implemented non-linear demand function incorporating mean and standard deviation
- Differentiated between peak and non-peak hours
- Separate analysis for weekday and weekend pricing

**2. Delivery Pass Initiative**

- Developed two subscription types:

  - Anytime Pass (7-day access)
  - Midweek Pass (Tuesday-Thursday access)

- Three subscription durations:

  - 1 month
  - 6 months
  - 12 months

- Applied simultaneous pricing strategy with premium positioning considerations

### **Getting Started**

**Prerequisites**

- R (version 4.0 or higher)
- RStudio (recommended)

**Installation**

1) Clone the repository
```shell 
git clone https://github.com/yourusername/Waitrose_Delivery_Revenue_Optimization.git
```

2. Install required R packages
```r
install.packages(c("ggplot2", "dplyr", "tidyverse", "nloptr", "stargazer"))
```

**Usage**

1. Open the R project in RStudio
2. Set your working directory to the project root
3. Run the main analysis script:
```
source("src/WaitroseAnalysis.R")
```

**Documentation**

- Detailed methodology and analysis can be found in `WaitroseAnalysis.pdf`
- Code is thoroughly commented with explanations of key functions and algorithms
