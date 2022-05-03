# Nor-Solidarity

This is the GitHub repository for the Nor-Solidarity trial.

Update 3 May 2022:

Testing how to reproduce the results.

1. Downloaded the correct set of data from Viedoc. I downloaded the set from 04 Jan 2021 00:30 UCT. Note the "Standard export for R ICO" should also work. This includes all subjects, all events, all forms, all types of data including event dates and medical coding, he output format is CSV, group data by form and 1 row per activity. The download resulted in a folder called "ous_20210104_003034". This corresponds to the VIEDOC_EXPORT_NAME in the Makefile, so this sounds good. 

2. Get the Antibody data and the viral load datasets "2011202_Remdesivir_study_Antibody_data.xlsx" and "ORO_data_til_Inge.xlsx" and put them in folder data/raw/misc/

3. First I opened the Rproject "Nor-Solidarity" and then I had to activate the project in renv using renv::activate(). This process also ran renv::restore()

4. The renv::restore() failed so I try to just run using the System Library

5. Had to make a small change in the stata call in make_rdvl_sg.R script because of a change in how the stata results are exported. 

6. It is running. Had to run both "make" and "make main_report1"




