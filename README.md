# iHist - Interactive histograms
Shiny app to import a data file, choose a dependent variable, and display a histogram that you can parameterize and explore in an interactive way.

If you use the app, please cite this article (in French):
Audibert, N. (2024). iHist et iScatter, outils en ligne d’exploration interactive de données : application aux valeurs aberrantes de f0 et de formants. Actes des 35èmes Journées d'Études sur la Parole (JEP 2024), Juillet 2024, Toulouse, France, pp.598-607. ⟨hal-04623107⟩
https://inria.hal.science/hal-04623107v1/document

Try the app online:
- English version: https://shiny.laboratoirephonetiquephonologie.fr/iHist/
- French version: https://shiny.laboratoirephonetiquephonologie.fr/iHist_fr/

## Installation and execution instructions (local)
You will need R (https://cran.r-project.org/) and RStudio (https://posit.co/download/rstudio-desktop/).
1) Download files: clone the repository, or use Code / Download ZIP and unzip the archive you downloaded
2) Open app.R in RStudio
3) If you don't already have all required packages installed, you will be prompted to install them
4) Use the Run App button on the topright corner of the code to run the app in a browser

## Allowing the use of larger data files
The default max file size upload is set to 1MB. To allow the use of larger input data files, change the value of maxUploadSizeMB in app.R.

For instance, to set it to 30MB use the following:
maxUploadSizeMB <- 30

## Set language
TL;DR: To use the English version of the app, you don't need to do anything.

The app uses files iHist.html and iHist.labels.txt to define the text displayed.
These files correspond to files in the 'localization' subfolder named iHist.[Language].html and iHist.[Language].labels.txt

Files iHist.html and iHist.labels.txt included in the root folder repository are duplicated of files iHist.English.html and iHist.English.labels.txt to ensure compatibility with any operating system.

To use another language, replace iHist.html and iHist.labels.txt with copies of localization files in another language, or use symbolic links to localization files (recommanded). To define localization symbolic links on Linux or MacOS, you may use the shell script set_localization_symlinks_iHist.sh with the target language as an argument.
