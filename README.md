# Problem Set 3 --- Predicting Housing Prices (Big Data & Machine Learning)

**Universidad de los Andes --- PEG BDML 2022‑2**

This repository contains the solution to **Problem Set 3** for the Big
Data and Machine Learning course at Universidad de los Andes (2022‑2).
The goal of the assignment is to develop predictive models for real
estate listing prices and generate optimized price predictions for
properties in Cali, Colombia.

------------------------------------------------------------------------

## 🎯 Objective

The goal of this project is to **predict housing listing prices** using
structural property characteristics, spatial predictors, and
text-derived features.

The strategic objective is to: - Minimize total predicted spending\
- Avoid strong overpricing\
- Avoid underpricing by more than COP 40 million

------------------------------------------------------------------------

## 📁 Repository Structure

    ├── .Rproj.user/  
    ├── Script/  
    ├── scripts/  
    ├── stores/  
    ├── .RData  
    ├── .Rhistory  
    ├── .gitignore  
    ├── Datos_espaciales_(J).R  
    ├── Documento.docx  
    ├── Documento2.docx  
    ├── LICENSE  
    ├── Objetos.R  
    ├── ProblemSet3-BDML-Uniandes.Rproj  
    ├── Problem_Set_3.pdf  
    ├── README.md  
    ├── Rplot.png  
    ├── datos espaciales cali.R  
    ├── datos espaciales mejor.R  
    ├── mapa2.R  
    ├── ob_es_limpios_m_b.R

------------------------------------------------------------------------

## 🧩 Overview of Scripts

### 🧼 Data Preparation & Cleaning

-   `ob_es_limpios_m_b.R` --- Baseline data cleaning and merging\
-   `Objetos.R` --- Defines key objects used across scripts

### 🌍 Spatial Features

-   `Datos_espaciales_(J).R` --- Spatial predictor extraction\
-   `datos espaciales mejor.R` --- Improved spatial dataset creation\
-   `datos espaciales cali.R` --- Spatial predictors tailored for Cali

### 📊 Mapping & Visualization

-   `mapa2.R` --- Generates maps and spatial visualizations

### 🧠 Modeling

-   Scripts inside `scripts/` handle feature engineering, model
    training, and prediction generation

------------------------------------------------------------------------

## 📤 Final Output

-   **Predictions:** Generated through modeling scripts and saved inside
    `stores/` according to defined output paths

------------------------------------------------------------------------

📦 Main R Packages Used
	•	tidyverse
	•	caret
	•	randomForest
	•	xgboost
	•	sf
	•	ggplot2
	•	text mining libraries

------------------------------------------------------------------------

## 📜 License

MIT License
