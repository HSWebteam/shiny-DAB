# DAB r-shiny app

- DABmodel.R
  _this file contains ananlysis functions, these are copy paste from orginal_analysis/Lion1_Sample.R and original_analysis/Monkey_Sample.R_
- dataPrep.R
  _this file contains functions which prepare the data for displaying in the ui and report._
  
- models/
  - createModels.R _With this file the *.rds files are created. These files store the statistical analysis and the data which is used to run the statistiacal analysis. This is done for performance. The *.rds files are much faster loaded than the statistical analysis are run._
  - *.rds _files generated with createModels.R_
- original_analysis/ _Contains the original statistical analysis and the data on which the analysis is based. Content is copy paste to the createModels.R and DABmodel.R files. These files can be used to manually check if the outcome in these files are the same as the outcome in the r-shiny app._
- plots.R _Functions with plots, these functions are used in the UI and in the download report_
- report.Rmd _download report template file_
- server.R _default r-shiny file_
- ui.R _default r-shiny file_


