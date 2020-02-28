README.md

This folder stores the models an data frames for the models.

The script createModels.R should run seperatly to create the files ...Data.rds and ...Model.rds

The Data.rds and Model.rds files are loaded in the r-shiny application.
Loading the data and models instead of genereting the models, gives much better performance on the r-shiny server.

The script contains checks if the stored objects are the same as the generated objects (identical).



