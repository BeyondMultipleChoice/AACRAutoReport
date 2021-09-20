# AACRAutoReport
AACR AutoReport Web App Suite
# AACR AutoReport

The Automated Analysis of Constructed Response (AACR) AutoReport Web App suite provides a set of tools for the use predictive models to automatically score constructed response items. The AutoReport system builds predictive models using an ensemble of supervised machine learning classification algorithms. These models are then stored for recall allong with asscoiated performance metrics to automatically score new written reposnses to the correspoding constructed response item. When new data is scored the system generates a fully-interactive report with the resultant scoring along with statistical analyses of the response set. These are implemented at the group website: beyondmultiplechoice.org .

## The Web Apps

AutoReport is made of three Shiny apps that assist in the development and testing of scoring models, score new written responses, and generate AACR Instructor Feedbacks Reports.

### AACR AutoReport

The primary AutoReport app provides an interactive environment for the automated scoring of student writing and AACR Instructor Feedback report generation and consumption. AutoReport provides interactive web-based reports, downloadable PDF documents, and raw scored data.

### AACR Confirmatory Analysis

The Confirmatory Analysis app provides a tool for the development of machine scored constructed response items. This app performs a cross validation analysis using a set of hand scored responses to estimate performance of the scoring model and provide metrics useful for refining item rubrics.  Developed based on RTextTools.  
Timothy P. Jurka, Loren Collingwood, Amber E. Boydstun, Emiliano Grossman and Wouter van Atteveldt (2012). RTextTools: Automatic Text Classification via Supervised Learning. R package version 1.3.9. http://CRAN.R-project.org/package=RTextTools 

### AACR Question Setup

The Question Seutp app trains scoring models for new constructed response items and records the metadata necessary for AutoReport to generate Instructor Feedback reports.


### Acknowledgements
This material is based upon work supported by the National Science Foundation (Grants 1323162, 1347740, 1561159, 1660643). Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the supporting agencies. 
