# czi

Changing Cities Research Lab 
Stanford University

Data Documentation Checklist
March 2, 2022

The underlying principle behind doing data analysis in R/Python is to ensure that all analysis is replicable and reproducible! While working on any project, keep in mind that the task at hand is not only to create the required outputs but also to ensure that the data analysis pipeline is recorded and replicable.

raw data → data processing code → processed data (→ analysis code → outputs)

Broadly, every stage in this process should be replicable. 

Datasets
Does the Readme clearly describe each dataset?
What variables are included in the dataset?
Are variable names and naming conventions evident?
What does each observation/row refer to?
Was this dataset downloaded from the internet or processed by a script?
Whenever possible, include explicit instructions for locating datasets.
If missing values are relevant, 

Code & Scripts
Is the script clean, readable, and appropriately formatted?
Is the script thoroughly commented?
Are all input files readily accessible on Github? For files not stored on Github, is the source/location sufficiently described in the Readme?
In case of several scripts that are dependent on one another, is the run order of these scripts described?

Other
Is the folder structure logical and/or described in the readme?
If the data was processed, are decisions documented in the Readme?
Missing values treatment & imputations?
Manual merges & imputations?
