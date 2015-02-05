Outline of the data processing workflow
=======================================

1. Use `PNC-processing.R` to process most recent standard version of PNC 
    a. Adds corrected demographic information
    b. Excludes under-18 and non-white
    c. Excludes Selkirk stop words
    d. Lobanov normalization & Mel transform
    e. Excludes speakers with < 5 tokens per short-a category
        => shorta-old.csv

#2. Use `clusters-processing.R` to do analysis of sC clusters
# is this actually being used now? think this gets taken care of in #3

3. Use `exceptions-check.R` to analyze the rest of the old aeBR class
    a. Exclude speakers w/o traditional system
    b. Mahalanobis distance recoding analysis -> aeBR.mahal.csv
    c. Update shorta.py rules & re-run corpus
        => shorta-new-10-03-14.csv

4. Use `newa-check.R` to look for residual misclassifications
    a. Mahalanobis distance recoding analysis -> aeBR.mahal2.txt
    b. Updata shorta.py rules & re-run corpus
        => shorta-new-10-07-14.csv

5. Use `PNC-processing.R` again to get final analysis-ready data
    a. PROCESS.NEW <- TRUE
        => shorta-new.csv
