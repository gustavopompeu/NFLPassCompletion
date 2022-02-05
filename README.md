# NFLPassCompletion

Note: Each of the 6 files can be reproduced independently. 

The raw original data to use in File 1 can be accessed for download at https://www.kaggle.com/c/nfl-big-data-bowl-2021/data (Last access: Feb 5, 2022).

The .rda and .csv files necessary to reproduce Files 2-6 are available for download at Google Drive: https://drive.google.com/drive/folders/1lsJdv8g6hOMRbcbU3eMAf2PmehOc3m5A?usp=sharing  (Last access: Feb 5, 2022).

Files description

"1_Data_reading.R": Initial reading of the original data and merging it into one big database. 

"2_Database_construction.R": Construction of the necessary databases for the project.

"3_Target_prediction.R": Creation of the probabilities and weights necessary for P(T=i). Also includes the plots of the weights.

"4_Pass_completion_prob.R": Creation of the probabilities of pass completion P(C|T=i) and P(C) and the cross-validation of models. Also includes the plot of the ROC curve.

"5_Animating_plays.R": Creation of the animations of the plays and the plots of the improbable pass completions comparing our model to the Next Gen Stats probabilities.

"6_Other_plots.R": Creation of the other plots presented in the work, such as the exploratory analysis of the created variables and the final results of our model for the whole database.
