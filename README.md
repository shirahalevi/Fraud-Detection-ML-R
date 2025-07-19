# Fraud Detection with Machine Learning (R)

This project focuses on detecting financial fraud using supervised machine learning algorithms, based on real-world transaction data with over 140,000 records.

## 📊 Project Overview

We built and compared multiple classification models to detect fraudulent transactions. The data underwent a full preprocessing pipeline including:

- Date/time conversion to POSIXct for time-based analysis
- Categorical to factor conversion
- NA value treatment and outlier handling
- Feature engineering: distance between customer and merchant, and derived ratios
- Data balancing via oversampling (from 2% to 50%)

## 🧠 Models Trained

We trained and evaluated the following models:

- Logistic Regression
- XGBoost
- SVM
- AdaBoost
- Decision Tree

> Best performance was achieved with **XGBoost**, with F1 score of **99.4%** and AUC of **1.0**

Models like Naive Bayes and KNN were excluded due to poor performance with highly categorical and imbalanced data.

## 💡 Key Findings

- Fraud typically occurs around 23:00 and in specific regions.
- High-value transactions are not necessarily fraudulent.
- Certain merchant categories (e.g., `shopping_net`, `grocery_pos`) are more prone to fraud.
- Feature importance analysis showed clear patterns in behavior.

## 💰 Economic Impact

A cost-benefit model was developed to estimate ROI:
- Estimated annual fraud cases: ~2,889
- Detected by system: ~2,456 (85%)
- Estimated savings: **$2.45M/year**
- Operational cost: $20K → **Net annual benefit: $2.43M**

## 📂 Files

- `fraud_detection.R` – Main analysis and modeling code
- `fraud_report.pdf` – Full project report with methodology and results

## 📧 Contact

Project by **Shira Halevi** | Industrial & Management Engineering

---

