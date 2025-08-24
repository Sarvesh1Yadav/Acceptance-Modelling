# 🎓 Applicant Ranking & Merit Score Calculator (R Project)

## 📌 Project Overview
This project is an **Applicant Ranking & Merit Score Calculator** built in **R**.  
It processes admission-related data, applies feature engineering, and calculates a **Merit Score Index (MSI)** for each applicant.  

The final ranked list helps universities/admission teams in:
- Prioritizing candidate **shortlisting**
- Identifying applicants for **outreach**
- Allocating **scholarships** based on merit

---

## 🛠 Features
- **Data Cleaning & Preprocessing**: Handles missing values, normalizes GPA and ACT scores, and prepares the dataset.
- **Feature Engineering**: Creates custom flags and derived variables for better applicant profiling.
- **Merit Score Calculation**: Assigns weights to academic and non-academic factors to compute the MSI.
- **Ranking**: Generates a sorted list of applicants from highest to lowest merit.
- **Weekly Report Generation**: Outputs CSV/Excel files with updated applicant rankings for decision-making.

---


---

## 🚀 How to Run the Project

### Prerequisites
- Install [R](https://cran.r-project.org/) (≥ 4.0)
- Recommended IDE: [RStudio](https://posit.co/download/rstudio-desktop/)

### Required Packages
Install the following R packages:
```R
install.packages(c("dplyr", "readr", "xgboost"))
## Clone the repo
git clone https://github.com/Sarvesh1Yadav/Acceptance-Modelling.git
cd Accpetance-Modelling

