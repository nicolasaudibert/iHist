# load required libraries
require(tools)
require(shiny)
require(shinythemes)
require(tidyverse)
require(rhandsontable)
require(MASS)
require(boot)
require(readxl)

# General parameters
defaultTheme = "spacelab"
htmlHeaderFile = "iHist.html"
labelsFile = "iHist.labels.txt"
nRowsDisplayedInFilePreview = 20
defaultBinsNumber = 10
nStepsInScaleSettings = 10
availableColumnSeparatorsDisplay = c("tabulation (\\t)", "espace", "virgule (,)" ,"point-virgule (;)")
availableColumnSeparators = c("\t", " ", ",", ";")
availableEncodingsDisplay = c("Unicode (UTF-8)","Windows (latin-1)", "Windows (latin-9)", "Mac OS (MacRoman)", "inconnu")
availableEncodings = c("UTF-8","latin-1", "latin-9", "macintosh", "unknown")
availableOutputFormats = c("PNG","PDF", "SVG", "EPS")
availableOutputFormatExtensions = c(".png", ".pdf", ".svg", ".eps")
availableUnitsForImageExport = c("cm","in","mm")
availableDataFileOutputFormats = c("XLSX", "TSV", "CSV")
availableDataFileOutputFormatExtensions = c(".xlsx", ".tsv", ".csv")
textSize = 14
textSizeAxes = 12
exportedImagesUnit = "cm"
defaultExportedPlotWidth = 30
defaultExportedPlotHeight = 20
figureDisplayRefreshRateMilliseconds = 200
maxUploadSizeMB = 1
histogramBarsColor = "#75AADB"
histogramSelectedBinColor = "#299AB3"
overlaidDensityCurveColor = "blue"
overlaidNormalCurveColor = "red"
overlaidCurvesSize = 1
meanAndCIlinesColor = "darkmagenta"
medianAndQuantilesLinesColor = "gray47"
overlaidVerticalLinesSize = 1
overlaidVerticalLinesTypeMain = "solid"
overlaidVerticalLinesTypeSecondary = "dashed"

# debug parameters
debugFlag = F
interactiveDebugFlag = F

getLabelOrPrompt <- function(code, labelsAndPromptsReference) {
  selectedRow = labelsAndPromptsReference[labelsAndPromptsReference$entry==code,]
  if(nrow(selectedRow)>0) {
    return(selectedRow$displayedLabel[1])
  } else {
    return("-- unknown label or prompt --")
  }
}
displayedLabelsAndPrompts = read.table(file = labelsFile, sep = "\t", header = T, encoding = "UTF-8", quote = "", stringsAsFactors = F)

myApp <- shinyApp(
  # UI side of the app: define layout
  ui = fluidPage(
    theme = shinytheme(defaultTheme),
    # includeCSS(cssFile),
    # Page title
    titlePanel("", windowTitle = getLabelOrPrompt("windowTitle", displayedLabelsAndPrompts)),
    # General HTML description on top of the page (loaded from an external headerless HTML file)
    htmlOutput("introText"),
    # Data file selection
    fileInput(
      "datafile",
      getLabelOrPrompt("datafileInputLabel", displayedLabelsAndPrompts),
      accept=c(
        'text/csv',
        'text/tsv',
        'text/tab-separated-values,text/plain',
        'application/vnd.ms-excel',
        # 'application/vnd.oasis.opendocument.spreadsheet',
        'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
        )
      ),
    # Panel with controls for variables and filter selection displayed only when a data file is loaded
    conditionalPanel(
      condition="output.fileUploaded",
      p(getLabelOrPrompt("generalInstructionsOnTabDisplay", displayedLabelsAndPrompts)),
      tabsetPanel(
        tabPanel(getLabelOrPrompt("variablesChoiceTabLabel", displayedLabelsAndPrompts), 
          uiOutput("nonNumericColsIncludedInDependentVariableChoiceFlag"),
          # These column selectors are dynamically created when the file is loaded
          fluidRow(
            column(3, uiOutput("depVar"), align="center"),
            column(3, uiOutput("filterVar"), align="center"),
            column(4, rHandsontableOutput("filterValTable", height = 120)),
            column(2, 
                   uiOutput("selectAllButton", align="center"),
                   uiOutput("unselectAllButton", align="center"),
                   uiOutput("revertSelectionButton", align="center")
            )
          )
        ),
        tabPanel(getLabelOrPrompt("generalDisplayParametersTabLabel", displayedLabelsAndPrompts),
                 # Panel displayed only when a dependent variable is set, and there is at least 2 data points to be displayed
                 conditionalPanel(
                   condition="output.dependentVariableSelected",
                   # Checkbox: display frequencies instead of counts in the histogram
                   uiOutput("displayAsFrequenciesFlag"),
                   # Numeric inputs for limits values, and dropdown menus to set the mode for limits and bins number adjustment to auto or manual
                   fluidRow(
                     column(2, numericInput("histogramLowLimit", getLabelOrPrompt("histogramLowLimitLabel", displayedLabelsAndPrompts),NA), align="center"),
                     column(2, numericInput("histogramHighLimit", getLabelOrPrompt("histogramHighLimitLabel", displayedLabelsAndPrompts),NA), align="center"),
                     column(3, uiOutput("histogramLimitsMode"), align="center"),
                     column(3, uiOutput("nBinsUpdateOnDataChange"), align="center")
                   ),
                   # Slider input for bin number selection
                   sliderInput(inputId = "bins",
                               label = getLabelOrPrompt("sliderInputLabel", displayedLabelsAndPrompts),
                               min = 5,
                               max = 100,
                               value = defaultBinsNumber,
                               width="100%"),
                   # Formatted text: display bin width
                   htmlOutput("binWidthDisplay")
                 )
        ),
        tabPanel(getLabelOrPrompt("overlaidCurvesTabLabel", displayedLabelsAndPrompts),
          # Panel displayed only when a dependent variable is set, and there is at least 2 data points to be displayed
          conditionalPanel(
            condition="output.dependentVariableSelected",
            # Checkboxes: curves overlaid on the histogram
            uiOutput("displayDensityFlag"),
            uiOutput("displayNormFlag")
          )
        ),
        tabPanel(getLabelOrPrompt("overlaidLinesTabLabel", displayedLabelsAndPrompts),
           # Panel displayed only when a dependent variable is set, and there is at least 2 data points to be displayed
           conditionalPanel(
            condition="output.dependentVariableSelected",
            fluidRow(
              # Checkbox: display median and confidence interval as vertical lines
              column(5, uiOutput("displayMeanAndConfidenceIntervalFlag"), align="center", style = "margin-top: 25px;"),
              column(2, numericInput("confidenceIntervalProb", getLabelOrPrompt("confidenceIntervalProbLabel", displayedLabelsAndPrompts), 0.95, min = 0.68, max = 0.999, step = 0.001), align="center"),
              column(3, uiOutput("confidenceIntervalComputationMethod"), align="center"),
              column(2, numericInput("nReplicationsForBootstrapCIcomputation", getLabelOrPrompt("nReplicationsForBootstrapCIcomputationLabel", displayedLabelsAndPrompts), 500, min = 200, max = 1000, step = 100), align="center")
            ),
            fluidRow(
              # Checkbox: display median and selected percentiles as vertical lines
              column(5, uiOutput("displayMedianAndQuantilesFlag"), align="center", style = "margin-top: 25px;"),
              column(2, numericInput("lowDisplayedQuantilePercentage", getLabelOrPrompt("lowDisplayedQuantilePercentageLabel", displayedLabelsAndPrompts), 0.05, min = 0.001, max = 0.999, step = 0.001), align="center"),
              column(2, numericInput("highDisplayedQuantilePercentage", getLabelOrPrompt("highDisplayedQuantilePercentageLabel", displayedLabelsAndPrompts), 0.95, min = 0.001, max = 0.999, step = 0.001), align="center"),
              column(3, uiOutput("keepDisplayedQuantilesSymetricFlag"), align="center", style = "margin-top: 25px;")
            )
          )
        ),
        tabPanel(getLabelOrPrompt("descriptiveStatisticsTabLabel", displayedLabelsAndPrompts),
                 # Panel displayed only when a dependent variable is set, and there is at least 2 data points to be displayed
                 conditionalPanel(
                   condition="output.dependentVariableSelected",
                   htmlOutput("descriptiveStatisticsInfo"),
                   fluidRow(
                     # Numeric input: let the user adjust the number of decimals displayed
                     # Label display separately to have it on the same line
                     column(7, p(strong(getLabelOrPrompt("nDecimalPlacesInputLabel", displayedLabelsAndPrompts)))),
                     column(2, numericInput("nDecimalPlaces", NULL, NA, min = 0, step = 1))
                   )
                 )
        ),
        tabPanel(getLabelOrPrompt("figureExportTabLabel", displayedLabelsAndPrompts),
                 # Panel displayed only when a dependent variable is set, and there is at least 2 data points to be displayed
                 conditionalPanel(
                   condition="output.dependentVariableSelected",
                   # Controls for figure export as image
                   fluidRow(
                     column(3, uiOutput("plotDownloadFormat"), align="center"),
                     column(2, uiOutput("exportedPlotWidth"), align="center"),
                     column(2, uiOutput("exportedPlotHeight"), align="center"),
                     column(2, uiOutput("exportedPlotUnits"), align="center") #,
                   ),
                   fluidRow(
                     column(12, downloadButton("downloadPlot", label = getLabelOrPrompt("figureDownloadButtonLabel", displayedLabelsAndPrompts)), align="center")
                   )
                 ),
                 value = "figureExportTabLabel"
        )
      ),
      conditionalPanel(
        condition="output.dependentVariableSelected",
        # Histogram display area
        plotOutput("plotHist", click = "plot_click"),
        # Formatted text: descriptive statistics display
        # Control for figure export as image
        # fluidRow(
        #   column(3, uiOutput("plotDownloadFormat"), align="center"),
        #   column(2, uiOutput("exportedPlotWidth"), align="center"),
        #   column(2, uiOutput("exportedPlotHeight"), align="center"),
        #   column(2, uiOutput("exportedPlotUnits"), align="center"),
        #   column(2, downloadButton("downloadPlot"), align="center", style = "margin-top: 25px;")
        # ),
        # Formatted text: information on the selected bin
        htmlOutput("clickedBinInfo"),
        # Checkbox to let the user activate display of selected bin detail in a table
        uiOutput("displayClassesDetailInTableFlag"),
        # Table for the display of selected bin detail
        # DT::dataTableOutput("selectedDataDetailsTable", width = "100%")
        rHandsontableOutput("selectedDataDetailsTable", height = 150)
      ),
      # Conditional panel to let the the user export selected data
      conditionalPanel(
        condition="output.selectedDataDetailsDisplayed",
        fluidRow(
          column(3, htmlOutput("selectedDataDownloadInfo"), align="right", style = "margin-top: 25px;"),
          column(2, uiOutput("selectedDataDownloadFormat"), align="center"),
          column(2, downloadButton("downloadSelectedData"), align="left", style = "margin-top: 25px;")
        )
      )
    )
  ),
  
  # Server side of the app: define behaviour
  server = function(input, output, session) {
    # Set the file upload limit
    base::options(shiny.maxRequestSize = maxUploadSizeMB * 1024^2)
    
    # Define a list of reactive values to store various parameters
    values <- reactiveValues(
       selectedFile = NULL,
       loadedFileType = NULL,
       columnSeparator = "\t",
       selectedFileEncoding = "UTF-8",
       availableSheetNames = NULL,
       selectedSheetIndex = 1,
       nLinesDisplayedInPreview = nRowsDisplayedInFilePreview,
       associatedMessage = NULL,
       filedata = NULL,
       filedataColnames = NULL,
       filteredFiledata = NULL,
       histogramData = NULL,
       currentDatasetHistogram = NULL,
       clickedBinIndex = NULL,
       clickedSubset = NULL,
       clickedBinLowLimit = NULL,
       clickedBinHighLimit = NULL,
       filterVariable = NULL,
       filterValues = NULL,
       NAinFilterValues = FALSE,
       nonNumericColsIncludedInDependentVariableChoiceFlag = FALSE,
       dependentVariable = NULL,
       nBinsUpdateOnDataChange = getLabelOrPrompt("nBinsUpdateOnDataChangeChoiceAuto", displayedLabelsAndPrompts),
       histogramLimitsMode = getLabelOrPrompt("histogramLimitsModeChoiceAuto", displayedLabelsAndPrompts),
       histogramLowLimit = NA,
       histogramHighLimit = NA,
       nBins = defaultBinsNumber,
       displayAsFrequenciesFlag = 0,
       displayDensityFlag = 0,
       displayNormFlag = 0,
       displayMeanAndConfidenceIntervalFlag = 0,
       confidenceIntervalProb = 0,
       confidenceIntervalComputationMethod = NULL,
       nReplicationsForBootstrapCIcomputation = 500,
       displayMedianAndQuantilesFlag = 0,
       lowDisplayedQuantilePercentage = 0.05,
       highDisplayedQuantilePercentage = 0.95,
       keepDisplayedQuantilesSymetricFlag = 1,
       displayClassesDetailInTableFlag = 0,
       nDecimalPlaces = 0,
       descriptiveStatisticsInfoText = "",
       clickedBinInfoText = "",
       lastParameterChangeTime = Sys.time()
      )
    
    # Dialog box displayed when a text file is loaded to let the user choose the column separator
    displayLoadedFilePreviewDialog <- function(selectedFile, defaultMainParameterValue) {
      # output$inputFilePreview <- renderRHandsontable({NULL})
      output$inputFilePreview <- renderTable(NULL)
      output$inputFilePreviewMessage <- renderText(NULL)
      output$loadedFilePreviewMainParameterSelector <- renderUI({
        if(values$loadedFileType == "txt")
          selectInput("loadedFilePreviewMainParameterSelector", getLabelOrPrompt("columnSeparatorSelectorLabel", displayedLabelsAndPrompts), availableColumnSeparatorsDisplay, selected = defaultMainParameterValue)
        else {
          if(values$loadedFileType == "xlsx") {
            # wb <- loadWorkbook(selectedFile)
            # values$availableSheetNames <- names(getSheets(wb))
            values$availableSheetNames <- excel_sheets(selectedFile)
          # } else if(values$loadedFileType == "xls") {
          #   # values$availableSheetNames <- sheetNames(selectedFile)
          #   values$availableSheetNames <- excel_sheets(selectedFile)
          # } else if(values$loadedFileType == "ods") {
          #   values$availableSheetNames <- ods_sheets(selectedFile)
          } else
            values$availableSheetNames <- NULL

          selectInput("loadedFilePreviewMainParameterSelector", getLabelOrPrompt("sheetSelectorLabel", displayedLabelsAndPrompts), values$availableSheetNames)
        }
      })
      output$fileEncoding <- renderUI({
        selectInput("fileEncoding", getLabelOrPrompt("fileEncodingLabel", displayedLabelsAndPrompts),availableEncodingsDisplay)
      })
      output$selectedDataDownloadInfo <- renderUI({
        HTML(str_c("<b>", getLabelOrPrompt("selectedDataDownloadInfo", displayedLabelsAndPrompts), "</b>"))
      })

      showModal(
        modalDialog(
          size = "l",
          # includeCSS(cssFile),
          fluidRow(
            column(6, uiOutput("loadedFilePreviewMainParameterSelector")),
            column(6, uiOutput("fileEncoding"))
          ),
          numericInput("nLinesDisplayedInPreview", label = getLabelOrPrompt("nLinesDisplayedInPreviewLabel", displayedLabelsAndPrompts), value = nRowsDisplayedInFilePreview, min = 5, step = 5),
          htmlOutput("inputFilePreviewMessage"),
          tableOutput("inputFilePreview"),
          title = getLabelOrPrompt("loadedFilePreviewDialogLabel", displayedLabelsAndPrompts),
          easyClose = FALSE,
          footer = tagList(
            actionButton("cancelColumnSeparatorChoiceButton","Cancel"),
            actionButton("validateSelectedColumnSeparatorButton", "OK")
          )
        )
      )
      
      updateLoadedTextFilePreviewDisplay()
    }
    
    # Update the data file preview
    updateLoadedTextFilePreviewDisplay <- function() {
      df = loadDataFile(values$nLinesDisplayedInPreview)
      if(!is.null(df)) {
        output$inputFilePreview <- renderTable(
          df,
          spacing = 'xs',
          bordered = TRUE,
          striped = TRUE,
          align = "c",
          na = ""
        )
      } else {
        output$inputFilePreview <- renderTable(NULL)
      }
      if(!is.null(values$associatedMessage)) {
        output$inputFilePreviewMessage <- renderText(HTML(paste0("<p>", values$associatedMessage, "</p>")))
      } else
        output$inputFilePreviewMessage <- renderText(NULL)
    }
    
    # Load the data file as an Excel spreadsheet or as a text file
    # Return a data structure with the resulting data frame (or NULL in case of failure) and an optional error or warning message
    loadDataFile <- function(nLoadedRows) {
      values$associatedMessage = NULL
      df <- tryCatch({
        withCallingHandlers({
          if(values$loadedFileType == "xlsx") {
            if(is.null(nLoadedRows))
              # extractedRowIndices = NULL
              nMaxRows <- Inf
            else  
              # extractedRowIndices = 1:(nLoadedRows+1)
              nMaxRows <- nLoadedRows+1
            # df = read.xlsx(
            #   values$selectedFile,
            #   values$selectedSheetIndex,
            #   encoding = values$selectedFileEncoding,
            #   check.names = F,
            #   rowIndex = extractedRowIndices
            # )
            df <- read_excel(
              path = values$selectedFile,
              sheet = values$selectedSheetIndex,
              # encoding = values$selectedFileEncoding,
              # check.names = F,
              # rowIndex = extractedRowIndices
              n_max = nMaxRows
            )
          # } else if(values$loadedFileType == "ods") {
          #   # if(is.null(nLoadedRows))
          #   #   extractedRowIndices = NULL
          #   # else  
          #   #   extractedRowIndices =  paste0("R1:R",nLoadedRows)
          #   df = read_ods(
          #     values$selectedFile,
          #     values$selectedSheetIndex
          #     #range = extractedRowIndices
          #     )
          } else if(values$loadedFileType == "txt") {
            if(is.null(nLoadedRows))
              nLoadedRows = -1
              df = read.table(
                values$selectedFile,
                sep=values$columnSeparator,
                header=T,
                fileEncoding = values$selectedFileEncoding,
                check.names = F,
                nrows = nLoadedRows,
                comment.char = ""
              )
          } else {
            df = NULL
          }
        }, warning = function(war) {
          values$associatedMessage = getLabelOrPrompt("loadedFilePreviewWarningMessage", displayedLabelsAndPrompts)
          invokeRestart("muffleWarning")
        })
      }, error = function(err) {
        df = NULL
        values$associatedMessage = getLabelOrPrompt("loadedFilePreviewErrorMessage", displayedLabelsAndPrompts)
      })
      if(!is.data.frame(df))
        df = NULL
      return(df)
    }
    
    # Event observers for the column separator selection dialog
    observeEvent(input$loadedFilePreviewMainParameterSelector, {
      if(!is.null(input$loadedFilePreviewMainParameterSelector)) {
        if(values$loadedFileType == "txt" && (values$columnSeparator != availableColumnSeparators[which(availableColumnSeparatorsDisplay == input$loadedFilePreviewMainParameterSelector)])) {
          values$columnSeparator = availableColumnSeparators[which(availableColumnSeparatorsDisplay == input$loadedFilePreviewMainParameterSelector)]
          updateLoadedTextFilePreviewDisplay()
        }
        else if(values$loadedFileType != "txt" && !is.null(values$availableSheetNames) && (values$selectedSheetIndex != which(input$loadedFilePreviewMainParameterSelector == values$availableSheetNames))) {
          values$selectedSheetIndex = which(input$loadedFilePreviewMainParameterSelector == values$availableSheetNames)
          updateLoadedTextFilePreviewDisplay()
        }
      }
    })
    observeEvent(input$nLinesDisplayedInPreview, {
      if(values$nLinesDisplayedInPreview != input$nLinesDisplayedInPreview) {
        values$nLinesDisplayedInPreview = input$nLinesDisplayedInPreview
        updateLoadedTextFilePreviewDisplay()
      }
    })
    observeEvent(input$fileEncoding, {
      if(!is.null(input$fileEncoding)) {
        if(values$selectedFileEncoding != availableEncodings[which(availableEncodingsDisplay == input$fileEncoding)]) {
          values$selectedFileEncoding = availableEncodings[which(availableEncodingsDisplay == input$fileEncoding)]
          updateLoadedTextFilePreviewDisplay()
        }
      }
    })
    observeEvent(input$validateSelectedColumnSeparatorButton, {
      removeModal()
      df = loadDataFile(NULL)
      postProcessLoadedFile(df)
    })
    observeEvent(input$cancelColumnSeparatorChoiceButton, {
      removeModal()
    })
    
    # Load the selected file
    observeEvent(input$datafile, {
      values$filedata <- NULL
      values$filedataColnames <- NULL
      
      infile <- input$datafile
      if (!is.null(infile)) {
        values$selectedFile = infile$datapath
        # Adapt loading method according to the file extension
        inputFileExtension = file_ext(infile$datapath)
        
        if(inputFileExtension == "xlsx" || inputFileExtension == "XLSX" || inputFileExtension == "xls" || inputFileExtension == "XLS") {
          
          values$loadedFileType = "xlsx"
          displayLoadedFilePreviewDialog(values$selectedFile, NULL)

        } else if(inputFileExtension == "csv" || inputFileExtension == "CSV") {

          values$loadedFileType = "txt"
          displayLoadedFilePreviewDialog(values$selectedFile, ",")
          
        } else {

          values$loadedFileType = "txt"
          displayLoadedFilePreviewDialog(values$selectedFile, "\t")
          
        }
      }
    })
    
    postProcessLoadedFile <- function(df) {
      if(!is.null(df)) {
        # If some columns have no variable name, discard them
        dfColnames = colnames(df)
        values$filedataColnames = dfColnames
        emptyColnamesIndices = which(dfColnames == "")
        if(length(emptyColnamesIndices)>0) {
          df <- df[,-emptyColnamesIndices]
        }
        values$filedata <- df
        # Get the column names
        values$filedataColnames = colnames(df)
        if(debugFlag) {
          cat(file=stderr(), "values$filedataColnames = ", paste(values$filedataColnames,sep=" "),"\n")
          if(interactiveDebugFlag) browser()
        }
        
        # Set back most reactive values to default (keep the value of data-independent parameters)
        values$dependentVariable <- NULL
        values$histogramData <- NULL
        values$currentDatasetHistogram <- NULL
        values$clickedBinIndex <- NULL
        values$clickedSubset <- NULL
        values$clickedBinLowLimit <- NULL
        values$clickedBinHighLimit <- NULL
        values$clickedBinIndex <- NULL
        values$filterVariable <- NULL
        values$filterValues <- NULL
        values$NAinFilterValues <- FALSE
        values$dependentVariable <- NULL
        values$nBins <- defaultBinsNumber
        values$nDecimalPlaces = 0
        values$descriptiveStatisticsInfoText = ""
        values$clickedBinInfoText = ""
        values$filteredFiledata = filterData()
        values$lastParameterChangeTime = Sys.time()
      }
    }
    
    # Function to apply the current filter (if any) to the full dataset
    filterData <- function(){
      df <- values$filedata
      if(debugFlag) {
        cat(file = stderr(), "filterData()")
        if(is.null(df)) {
          cat(file = stderr(), " df IS NULL\n")
        } else {
          cat(file = stderr(), " df NOT NULL\n")
        }
      }
      if (is.null(df)) {
         return(NULL)
      }
      if(!is.null(values$filterVariable) && values$filterVariable!="\u00A0") {
        if(!is.null(values$filterValues)) {
          # Apply filter
          if(length(values$filterValues) > 0) {
              filteredData <- df %>% 
                filter(!!sym(values$filterVariable) %in% values$filterValues)
              # If empty cells (NA values when file read as .xlsx or .ods) are part of the filter, make sure to include them
              if(values$NAinFilterValues) {
                filteredData = rbind(filteredData, df %>% filter(is.na(!!sym(values$filterVariable))))
              }
              return(filteredData)
          } else {
            return(NULL)
          }
        } else {
          return(NULL)
        }
      } else {
        # No filter variable defined: return raw data frame
        return(df)
      }
    }
    
    # Detect when a file has been uploaded (required for conditional display of variables selection)
    output$fileUploaded <- reactive({
      return(!is.null(values$filedata))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    # Detect when a valid dependent variable has been selected and there's either no filter set, or a minimum of 
    # 2 data points remaining in the filter (required for conditional display of statistics and figure)
    output$dependentVariableSelected <- reactive({
      return(!is.null(input$datafile) && !is.null(values$filedata) && !is.null(values$dependentVariable) && values$dependentVariable!="\u00A0" && values$dependentVariable!=0 && (is.null(values$filterValues) || length(values$filterValues)>0) && !is.null(values$histogramData) && length(values$histogramData)>1)
    })
    outputOptions(output, 'dependentVariableSelected', suspendWhenHidden=FALSE)
    
    output$selectedDataDetailsDisplayed <- reactive({
      return(values$displayClassesDetailInTableFlag!=0 && !is.null(values$clickedSubset) && nrow(values$clickedSubset)>0)
    })
    outputOptions(output, 'selectedDataDetailsDisplayed', suspendWhenHidden=FALSE)
    
    # HTML displayed on top of the page (contents loaded from file)
    output$introText <- renderUI({
      displayedHTML = readLines(htmlHeaderFile)
      HTML(paste(displayedHTML))
    })
    
    # Checkbox to force inclusion of variables not detected as numeric in the dependent variable menu
    output$nonNumericColsIncludedInDependentVariableChoiceFlag <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      checkboxInput("nonNumericColsIncludedInDependentVariableChoiceFlag", getLabelOrPrompt("nonNumericColsIncludedInDependentVariableChoiceLabel", displayedLabelsAndPrompts), FALSE, width="100%")
    })
    
    # Update list of available dependent variables when the state of the checkbox for inclusion of non-numeric variables changes
    observeEvent(input$nonNumericColsIncludedInDependentVariableChoiceFlag, {
      if(values$nonNumericColsIncludedInDependentVariableChoiceFlag != input$nonNumericColsIncludedInDependentVariableChoiceFlag) {
        if(debugFlag) cat(file=stderr(), "observeEvent(input$nonNumericColsIncludedInDependentVariableChoiceFlag)", " values$dependentVariable = ", values$dependentVariable, " nrow(values$filedata) = ", nrow(values$filedata), " length(values$filedataColnames) = ", length(values$filedataColnames), "\n")
        if(!is.null(values$filedata)) {
          values$nonNumericColsIncludedInDependentVariableChoiceFlag = input$nonNumericColsIncludedInDependentVariableChoiceFlag
            if(values$nonNumericColsIncludedInDependentVariableChoiceFlag!=0) {
              items=c("\u00A0",values$filedataColnames)
              names(items)=items
            } else {
              numericColumnIndices <- sapply(values$filedata, is.numeric)
              if(!is.null(numericColumnIndices) && length(numericColumnIndices[numericColumnIndices]) > 0) {
                items=c("\u00A0",names(numericColumnIndices[numericColumnIndices]))
                names(items)=items
              } else {
                items = 0
                names(items) = getLabelOrPrompt("noNumericVariableWarning", displayedLabelsAndPrompts)
              }
            }
            if(!is.null(values$dependentVariable) && values$dependentVariable!="" && values$dependentVariable %in% items) {
              selectedItem = values$dependentVariable
            } else {
              selectedItem = "\u00A0"
            }
            updateSelectInput(session, "depVar", label = NULL, choices = items, selected = values$dependentVariable)
        }
      }
    })
    
    # Dropdown menu: dependent variable (any numeric variable)
    output$depVar <- renderUI({
      df <-values$filedata
      if(debugFlag) {
        cat(file=stderr(), "output$depVar start", "values$dependentVariable = ", values$dependentVariable, "values$filterVariable = ", values$filterVariable, "\n")
        if(interactiveDebugFlag) browser()
      }
      if (is.null(df)) return(NULL)
      
      # Show only numeric columns, unless the corresponding options is checked
      if(values$nonNumericColsIncludedInDependentVariableChoiceFlag!=0) {
        items=c("\u00A0",values$filedataColnames)
        names(items)=items
      } else {
        numericColumnIndices <- sapply(df, is.numeric)
        if(!is.null(numericColumnIndices) && length(numericColumnIndices[numericColumnIndices]) > 0) {
          items=c("\u00A0",names(numericColumnIndices[numericColumnIndices]))
          names(items)=items
        } else {
          items = 0
          names(items) = getLabelOrPrompt("noNumericVariableWarning", displayedLabelsAndPrompts)
        }
      }
      selectInput("depVar", getLabelOrPrompt("dependentVariableInputLabel", displayedLabelsAndPrompts), items, selected = items[1])
    })
    
    # Update variables, plot and table when the dependent variable has changed
    observeEvent(input$depVar, {
      if(is.null(values$dependentVariable) || values$dependentVariable != input$depVar) {
        if(debugFlag) {
          cat(file=stderr(), "observeEvent(input$depVar) start", "values$dependentVariable = ", values$dependentVariable, "values$filterVariable = ", values$filterVariable, "\n")
          if(interactiveDebugFlag) browser()
        }
        values$lastParameterChangeTime = Sys.time()
        output$selectedDataDetailsTable <- renderRHandsontable({NULL})
        values$dependentVariable <- input$depVar
        if(is.null(values$filterVariable) || (values$filterVariable!="\u00A0" && values$dependentVariable==values$filterVariable)) {
          values$filterVariable = "\u00A0"
          values$filterValTable = NULL
        }
        if(!is.null(values$dependentVariable) && values$dependentVariable!="\u00A0" && values$dependentVariable!=0  && values$dependentVariable!="" && !is.null(values$filteredFiledata)) {
          values$histogramData = getHistogramData()
          if(length(values$histogramData) > 0) {
            # Compute the appropriate number of decimal places for rounding from the range in values and update the corresponding input
            rangeInCurrentDataset = max(values$histogramData) - min(values$histogramData)
            nDecimalPlaces = -(floor(log10(rangeInCurrentDataset)) - 3)
            if(nDecimalPlaces<0) {
              nDecimalPlaces = 0
            }
            updateNumericInput(session, "nDecimalPlaces", value = nDecimalPlaces)
            values$nDecimalPlaces = nDecimalPlaces
            
            # Update figure and descriptive statistics display
            values$descriptiveStatisticsInfoText <- descriptiveStatText()
            output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
            values$clickedBinIndex <- NULL
            values$clickedSubset <- NULL
            values$clickedBinLowLimit <- NULL
            values$clickedBinHighLimit <- NULL
            values$currentDatasetHistogram <- plottedFigure()
            output$plotHist <- renderPlot({values$currentDatasetHistogram})
            values$clickedBinInfoText <- clickedBinInfoBaseText()
            output$clickedBinInfo <- renderUI({HTML(values$clickedBinInfoText)})
            
            filterChoices = c("\u00A0",values$filedataColnames[-which(values$filedataColnames == values$dependentVariable)])
          } else {
            filterChoices = c("\u00A0",values$filedataColnames)
          }
        } else {
          filterChoices = c("\u00A0",values$filedataColnames)
        }
        names(filterChoices)=filterChoices
        updateSelectInput(session, "filterVar", getLabelOrPrompt("filterVariableInputLabel", displayedLabelsAndPrompts), choices = filterChoices, selected = values$filterVariable)
      }
    })
    
    # Dropdown menu to set limit values in the histogram automatically or manually
    output$histogramLimitsMode <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      items = c(getLabelOrPrompt("histogramLimitsModeChoiceAuto", displayedLabelsAndPrompts), getLabelOrPrompt("histogramLimitsModeChoiceFixed", displayedLabelsAndPrompts))
      names(items) = items
      selectInput("histogramLimitsMode", getLabelOrPrompt("histogramLimitsModeLabel", displayedLabelsAndPrompts), items, selected = items[1])
    })
    
    # When limit values of the histogram turn back to automatic mode, update figure display
    observeEvent(input$histogramLimitsMode, {
      if(values$histogramLimitsMode != input$histogramLimitsMode) {
        values$histogramLimitsMode = input$histogramLimitsMode
        if(values$histogramLimitsMode == getLabelOrPrompt("histogramLimitsModeChoiceAuto", displayedLabelsAndPrompts)) {
          currentDataset = getHistogramData()
          values$histogramLowLimit = min(currentDataset)
          values$histogramHighLimit = max(currentDataset)
          dataRange = max(currentDataset) - min(currentDataset)
          limitsInputStep = 10^round(log10(dataRange/nStepsInScaleSettings))
          updateNumericInput(session, "histogramLowLimit", value = values$histogramLowLimit, step = limitsInputStep)
          updateNumericInput(session, "histogramHighLimit", value = values$histogramHighLimit, step = limitsInputStep)
          values$currentDatasetHistogram <- plottedFigure()
          output$plotHist <- renderPlot({values$currentDatasetHistogram})
        }
      }
    })
    
    # When limit values of the histogram are adjusted, update figure display
    observeEvent(input$histogramLowLimit, {
      if(is.na(values$histogramLowLimit) || values$histogramLowLimit != input$histogramLowLimit) {
        values$histogramLowLimit = input$histogramLowLimit
        values$currentDatasetHistogram <- plottedFigure()
        output$plotHist <- renderPlot({values$currentDatasetHistogram})
        updateBinWidthText()
      }
    })
    observeEvent(input$histogramHighLimit, {
      if(is.na(values$histogramHighLimit) || values$histogramHighLimit != input$histogramHighLimit) {
        values$histogramHighLimit = input$histogramHighLimit
        values$currentDatasetHistogram <- plottedFigure()
        output$plotHist <- renderPlot({values$currentDatasetHistogram})
        updateBinWidthText()
      }
    })
    
    # Dropdown menu to set the number of bins in the histogram automatically or manually
    output$nBinsUpdateOnDataChange <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      items = c(getLabelOrPrompt("nBinsUpdateOnDataChangeChoiceAuto", displayedLabelsAndPrompts), getLabelOrPrompt("nBinsUpdateOnDataChangeChoiceSet", displayedLabelsAndPrompts))
      names(items) = items
      selectInput("nBinsUpdateOnDataChange", getLabelOrPrompt("nBinsUpdateOnDataChangeLabel", displayedLabelsAndPrompts), items, selected = items[1])
    })
    
    # When the computation of the number of bins turn back to automatic mode, update figure display
    observeEvent(input$nBinsUpdateOnDataChange, {
      if(is.null(values$nBinsUpdateOnDataChange) || values$nBinsUpdateOnDataChange!= input$nBinsUpdateOnDataChange) {
        values$nBinsUpdateOnDataChange = input$nBinsUpdateOnDataChange
        if(values$nBinsUpdateOnDataChange == getLabelOrPrompt("nBinsUpdateOnDataChangeChoiceAuto", displayedLabelsAndPrompts)) {
          currentDataset = getHistogramData()
          updateNBinsSlider(currentDataset)
          values$currentDatasetHistogram <- plottedFigure()
          output$plotHist <- renderPlot({values$currentDatasetHistogram})
        }
      }
    })
    
    # When the number of bins in the histogram is adjusted, update figure and text display
    observeEvent(input$bins, {
      if(values$nBins != input$bins) {
        values$lastParameterChangeTime = Sys.time()
        output$selectedDataDetailsTable <- renderRHandsontable({NULL})
        values$nBins <- input$bins
        values$clickedBinIndex <- NULL
        values$clickedSubset <- NULL
        values$clickedBinLowLimit <- NULL
        values$clickedBinHighLimit <- NULL
        values$currentDatasetHistogram <- plottedFigure()
        output$plotHist <- renderPlot({values$currentDatasetHistogram})
        updateBinWidthText()
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
        values$clickedBinInfoText <- clickedBinInfoBaseText()
        output$clickedBinInfo <- renderUI({HTML(values$clickedBinInfoText)})
      }
    })
    
    # Dropdown menu: filtering variable (any variable except dependent variable)
    output$filterVar <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      
      # Include all columns except the selected dependent variable
      datatsetColnames = colnames(df)
      if (length(datatsetColnames)>0) {
        datatsetColnames = values$filedataColnames
        items = datatsetColnames[-which(datatsetColnames==values$dependentVariable)]
        items=c("\u00A0",datatsetColnames)
        names(items)=items
        selectInput("filterVar", getLabelOrPrompt("filterVariableInputLabel", displayedLabelsAndPrompts), choices = items, selected = values$filterVariable)
      } else {
        return("")
      }
    })
    
    # When a new filter variable is set, update text and figure display, and display filter modalities with counts in a table
    observeEvent(input$filterVar, {
      if(!is.null(values$filterVariable) && values$filterVariable != input$filterVar) {
        if(debugFlag) {
          cat(file=stderr(), "observeEvent(input$filterVar) start", "values$dependentVariable = ", values$dependentVariable, "input$filterVar = ", input$filterVar, "values$filterVariable = ", values$filterVariable, "\n")
          if(is.null(values$filterVariable)) cat(file=stderr(), "values$filterVariable==NULL", "\n")
          if(!is.null(values$filterVariable) && length(values$filterVariable)==0) cat(file=stderr(), "length(values$filterVariable)==0", "\n")
          if(!is.null(values$filterVariable) && values$filterVariable=="\u00A0") cat(file=stderr(), "values$filterVariable==\" \"", "\n")
          if(interactiveDebugFlag) browser()
        }
        if(!is.null(input$filterVar) && input$filterVar!="\u00A0") {
          
          values$filterVariable = input$filterVar
          values$filterValues = ""
          
          # Get and display filter variable modalities with counts
          filterDF = getFilterTable()
          if(!is.null(filterDF)) {
            output$filterValTable <- renderRHandsontable(
              rhandsontable(filterDF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
                hot_col(col = 1, readOnly = TRUE) %>%
                hot_col(col = 2, readOnly = TRUE)  %>%
                hot_col(col = 3, type = "checkbox") %>%
                hot_cols(columnSorting = TRUE)
            )
            output$selectAllButton <- renderUI(
              actionButton("selectAllButton", paste0(getLabelOrPrompt("selectAllButtonLabel", displayedLabelsAndPrompts), " (", nrow(filterDF), ")"))
              )
            output$unselectAllButton <- renderUI(
              actionButton("unselectAllButton", getLabelOrPrompt("unselectAllButtonLabel", displayedLabelsAndPrompts))
              )
            output$revertSelectionButton <- renderUI(
              actionButton("revertSelectionButton", getLabelOrPrompt("revertSelectionButtonLabel", displayedLabelsAndPrompts))
              )
          } else {
            output$filterValTable <- renderRHandsontable({NULL})
            output$selectAllButton <- renderUI({NULL})
            output$unselectAllButton <- renderUI({NULL})
            output$revertSelectionButton <- renderUI({NULL})
          }
        } else {
          output$filterValTable <- renderRHandsontable({NULL})
          output$selectAllButton <- renderUI({NULL})
          output$unselectAllButton <- renderUI({NULL})
          output$revertSelectionButton <- renderUI({NULL})
        }
      }
    })
    
    # Get filter variable modalities with counts, assigning a special value to empty cells if any
    getFilterTable <- function(){
      df <-values$filedata
      if (is.null(df) || is.null(values$filterVariable) || values$filterVariable == "\u00A0") return(NULL)
      
      charVector <- df %>% pull(all_of(values$filterVariable)) %>% as.character()
      charVector[is.na(charVector)] = getLabelOrPrompt("emptyCellsLabel", displayedLabelsAndPrompts)
      
      # If a filter column is selected, include all unique values
      filterVarCounts = table(charVector)
      # # Discard categories where count = 1 (not suitable for histogram drawing)
      # filterVarCounts = filterVarCounts[filterVarCounts>1]

      subgroupsDF <- data.frame(
        nValues = filterVarCounts,
        selected = rep(TRUE, length(filterVarCounts))
      )
      colnames(subgroupsDF) = c(getLabelOrPrompt("groupColInFilterTable", displayedLabelsAndPrompts), getLabelOrPrompt("countsColInFilterTable", displayedLabelsAndPrompts), getLabelOrPrompt("includeColInFilterTable", displayedLabelsAndPrompts))
      return(subgroupsDF)
    }
    
    # Dropdown menu: value of the filter variable to be processed (all unique values of the filter variable, displayed with corresponding counts)
    output$filterValTable <- renderRHandsontable({
      
      subgroupsDF <- getFilterTable()
      if(!is.null(subgroupsDF)) {
        rhandsontable(subgroupsDF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
          hot_col(col = 1, readOnly = TRUE) %>%
          hot_col(col = 2, readOnly = TRUE)  %>%
          hot_col(col = 3, type = "checkbox") %>%
          hot_cols(columnSorting = TRUE)
      } else {
        NULL
      }
    })
    
    # When filter modalities are checked or uncheck in the filter table, update data and display (figure and text)
    observeEvent(input$filterValTable, {
      if(debugFlag){
        cat(file=stderr(), "observeEvent(input$filterValTable) start", "values$dependentVariable = ", values$dependentVariable, "values$filterVariable = ", values$filterVariable, "\n")
        if(interactiveDebugFlag) browser()
      }
      # Get the contents of the rhandsontable as data frame
      filterDF = hot_to_r(input$filterValTable)
      # Get all checked values
      filterValuesTmp = filterDF[filterDF[,3]==TRUE,1]
      if(length(filterValuesTmp)==0)
        filterValuesTmp = NULL
      if(is.null(values$filterValues) || (length(values$filterValues)==1 && values$filterValues=="")) {
        values$filterValues = filterValuesTmp
      } else if(!setequal(values$filterValues, filterValuesTmp)) {
        values$filterValues = filterValuesTmp
      }
      values$lastParameterChangeTime = Sys.time()
      # Check if empty cells are included in the filter
      values$NAinFilterValues = getLabelOrPrompt("emptyCellsLabel", displayedLabelsAndPrompts) %in% values$filterValues
      # Update data and display
      values$filteredFiledata <- filterData()
      values$histogramData <- getHistogramData()
      values$currentDatasetHistogram <- plottedFigure()
      output$plotHist <- renderPlot({values$currentDatasetHistogram})
      values$descriptiveStatisticsInfoText <- descriptiveStatText()
      output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
      values$clickedBinInfoText <- clickedBinInfoBaseText()
      output$clickedBinInfo <- renderUI({HTML(values$clickedBinInfoText)})
    })
    
    # Action buttons for multiple modalities (un)checking in the filter table
    output$selectAllButton <- renderUI({
      if(!is.null(values$filterVariable) && values$filterVariable!="\u00A0") {
        actionButton("selectAllButton", getLabelOrPrompt("selectAllButtonLabel", displayedLabelsAndPrompts))
      }
    })
    output$unselectAllButton <- renderUI({
      if(!is.null(values$filterVariable) && values$filterVariable!="\u00A0") {
        actionButton("unselectAllButton", getLabelOrPrompt("unselectAllButtonLabel", displayedLabelsAndPrompts))
      }
    })
    output$revertSelectionButton <- renderUI({
      if(!is.null(values$filterVariable) && values$filterVariable!="\u00A0") {
        actionButton("revertSelectionButton", getLabelOrPrompt("revertSelectionButtonLabel", displayedLabelsAndPrompts))
      }
    })
    
    # Update filter table and values when the user hits the 'select all' button
    observeEvent(input$selectAllButton, {
      # output$selectedDataDetailsTable <- renderRHandsontable({NULL})
      # values$filteredFiledata <- NULL
      # values$histogramData <- NULL
      # values$currentDatasetHistogram <- NULL
      # values$clickedBinIndex <- NULL
      # values$clickedSubset <- NULL
      # values$clickedBinLowLimit <- NULL
      # values$clickedBinHighLimit <- NULL
      # Get the contents of the rhandsontable as data frame
      filterDF = hot_to_r(input$filterValTable)
      if(!setequal(filterDF[,3], rep(TRUE, nrow(filterDF)))) {
        values$lastParameterChangeTime = Sys.time()
        # Set all modalities as checked
        filterDF[,3] = TRUE
        values$filterValues = filterDF[,1]
        # Update table display
        output$filterValTable <- renderRHandsontable(
          rhandsontable(filterDF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
            hot_col(col = 1, readOnly = TRUE) %>%
            hot_col(col = 2, readOnly = TRUE)  %>%
            hot_col(col = 3, type = "checkbox") %>%
            hot_cols(columnSorting = TRUE)
        )
        # Update data and display
        values$filteredFiledata <- filterData()
        values$histogramData <- getHistogramData()
        values$currentDatasetHistogram <- plottedFigure()
        output$plotHist <- renderPlot({values$currentDatasetHistogram})
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
        values$clickedBinInfoText <- clickedBinInfoBaseText()
        output$clickedBinInfo <- renderUI({HTML(values$clickedBinInfoText)})
      }
    })
    
    # Update filter table and values when the user hits the 'unselect all' button
    observeEvent(input$unselectAllButton, {
      # output$selectedDataDetailsTable <- renderRHandsontable({NULL})
      # values$filteredFiledata <- NULL
      # values$histogramData <- NULL
      # values$currentDatasetHistogram <- NULL
      # values$clickedBinIndex <- NULL
      # values$clickedSubset <- NULL
      # values$clickedBinLowLimit <- NULL
      # values$clickedBinHighLimit <- NULL
      # Get the contents of the rhandsontable as data frame
      filterDF = hot_to_r(input$filterValTable)
      if(!setequal(filterDF[,3], rep(FALSE, nrow(filterDF)))) {
        values$lastParameterChangeTime = Sys.time()
        # Set all modalities as unchecked
        filterDF[,3] = FALSE
        values$filterValues = filterDF[NULL,1]
        # Update table display
        output$filterValTable <- renderRHandsontable(
          rhandsontable(filterDF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
            hot_col(col = 1, readOnly = TRUE) %>%
            hot_col(col = 2, readOnly = TRUE)  %>%
            hot_col(col = 3, type = "checkbox") %>%
            hot_cols(columnSorting = TRUE)
        )
        # Update data and display
        values$filteredFiledata <- filterData()
        values$histogramData <- getHistogramData()
        values$currentDatasetHistogram <- plottedFigure()
        output$plotHist <- renderPlot({values$currentDatasetHistogram})
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
        values$clickedBinInfoText <- clickedBinInfoBaseText()
        output$clickedBinInfo <- renderUI({HTML(values$clickedBinInfoText)})
      }
    })
    
    # Update filter table and values when the user hits the 'revert selection' button
    observeEvent(input$revertSelectionButton, {
      values$lastParameterChangeTime = Sys.time()
      # output$selectedDataDetailsTable <- renderRHandsontable({NULL})
      # values$filteredFiledata <- NULL
      # values$histogramData <- NULL
      # values$currentDatasetHistogram <- NULL
      # values$clickedBinIndex <- NULL
      # values$clickedSubset <- NULL
      # values$clickedBinLowLimit <- NULL
      # values$clickedBinHighLimit <- NULL
      # Get the contents of the rhandsontable as data frame
      filterDF = hot_to_r(input$filterValTable)
      # Set checked modalities as unchecked and conversely
      filterDF[,3] = !as.logical(filterDF[,3])
      values$filterValues = filterDF[filterDF[,3]==TRUE,1]
      # Update table display
      output$filterValTable <- renderRHandsontable(
        rhandsontable(filterDF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
          hot_col(col = 1, readOnly = TRUE) %>%
          hot_col(col = 2, readOnly = TRUE)  %>%
          hot_col(col = 3, type = "checkbox") %>%
          hot_cols(columnSorting = TRUE)
      )
      # Update data and display
      values$filteredFiledata <- filterData()
      values$histogramData <- getHistogramData()
      values$currentDatasetHistogram <- plottedFigure()
      output$plotHist <- renderPlot({values$currentDatasetHistogram})
      values$descriptiveStatisticsInfoText <- descriptiveStatText()
      output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
      values$clickedBinInfoText <- clickedBinInfoBaseText()
      output$clickedBinInfo <- renderUI({HTML(values$clickedBinInfoText)})
    })
    
    # Checkbox: display mean and confidence interval over the histogram
    output$displayMeanAndConfidenceIntervalFlag <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      checkboxInput("displayMeanAndConfidenceIntervalFlag", getLabelOrPrompt("displayMeanAndConfidenceIntervalFlagLabel", displayedLabelsAndPrompts), FALSE, width="100%")
    })
    
    # Redraw figure on change of the checkbox state
    observeEvent(input$displayMeanAndConfidenceIntervalFlag, {
      if(values$displayMeanAndConfidenceIntervalFlag != input$displayMeanAndConfidenceIntervalFlag) {
        values$displayMeanAndConfidenceIntervalFlag = input$displayMeanAndConfidenceIntervalFlag
        values$currentDatasetHistogram <- plottedFigure()
        output$plotHist <- renderPlot({values$currentDatasetHistogram})
      }
    })
    
    # Redraw figure on change of the confidence interval probability, if mean and CI display is active
    observeEvent(input$confidenceIntervalProb, {
      if(values$confidenceIntervalProb != input$confidenceIntervalProb) {
        values$confidenceIntervalProb = input$confidenceIntervalProb
        if(!is.null(values$displayMeanAndConfidenceIntervalFlag) && values$displayMeanAndConfidenceIntervalFlag!=0) {
          values$currentDatasetHistogram <- plottedFigure()
          output$plotHist <- renderPlot({values$currentDatasetHistogram})
        }
      }
    })
    
    # Dropdown menu: selection of a method for confidence interval computation
    output$confidenceIntervalComputationMethod <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      items = c(getLabelOrPrompt("confidenceIntervalComputationMethodParametricLabel", displayedLabelsAndPrompts), getLabelOrPrompt("confidenceIntervalComputationMethodBootstrapPercentileLabel", displayedLabelsAndPrompts))
      names(items) = items
      selectInput("confidenceIntervalComputationMethod", getLabelOrPrompt("confidenceIntervalComputationMethodLabel", displayedLabelsAndPrompts), items, selected = items[1])
    })
    
    # Redraw figure on change of the confidence interval computation method, if mean and CI display is active
    observeEvent(input$confidenceIntervalComputationMethod, {
      if(is.null(values$confidenceIntervalComputationMethod) || values$confidenceIntervalComputationMethod != input$confidenceIntervalComputationMethod) {
        values$confidenceIntervalComputationMethod = input$confidenceIntervalComputationMethod
        if(!is.null(values$displayMeanAndConfidenceIntervalFlag) && values$displayMeanAndConfidenceIntervalFlag!=0) {
          values$currentDatasetHistogram <- plottedFigure()
          output$plotHist <- renderPlot({values$currentDatasetHistogram})
        }
      }
    })
    
    # Redraw figure on change of the number of replications for confidence interval computation, if mean and CI display is active and the bootstrap method is selected
    observeEvent(input$nReplicationsForBootstrapCIcomputation, {
      if(values$nReplicationsForBootstrapCIcomputation != input$nReplicationsForBootstrapCIcomputation) {
        values$nReplicationsForBootstrapCIcomputation = input$nReplicationsForBootstrapCIcomputation
        if(!is.null(values$displayMeanAndConfidenceIntervalFlag) && values$displayMeanAndConfidenceIntervalFlag!=0 && !is.null(values$confidenceIntervalComputationMethod) && values$confidenceIntervalComputationMethod!=getLabelOrPrompt("confidenceIntervalComputationMethodParametricLabel", displayedLabelsAndPrompts)) {
          values$currentDatasetHistogram <- plottedFigure()
          output$plotHist <- renderPlot({values$currentDatasetHistogram})
        }
      }
    })
    
    # Checkbox: display median and selected quantiles over the histogram
    output$displayMedianAndQuantilesFlag <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      checkboxInput("displayMedianAndQuantilesFlag", getLabelOrPrompt("displayMedianAndQuantilesFlagLabel", displayedLabelsAndPrompts), FALSE, width="100%")
    })
    
    # Redraw figure on change of the checkbox state
    observeEvent(input$displayMedianAndQuantilesFlag, {
      if(values$displayMedianAndQuantilesFlag != input$displayMedianAndQuantilesFlag) {
        values$displayMedianAndQuantilesFlag = input$displayMedianAndQuantilesFlag
        values$currentDatasetHistogram <- plottedFigure()
        output$plotHist <- renderPlot({values$currentDatasetHistogram})
      }
    })
    
    # Redraw figure on change of the low displayed quantile percentage, if median and quantiles display is active
    # If the 'keep quantiles symetric' flag is active, adjust the high quantile percentage accordingly before updating figure
    observeEvent(input$lowDisplayedQuantilePercentage, {
      if(values$lowDisplayedQuantilePercentage != input$lowDisplayedQuantilePercentage) {
        values$lowDisplayedQuantilePercentage = input$lowDisplayedQuantilePercentage
        if(!is.null(values$keepDisplayedQuantilesSymetricFlag) && values$keepDisplayedQuantilesSymetricFlag!=0) {
          values$highDisplayedQuantilePercentage = 1 - values$lowDisplayedQuantilePercentage
          updateNumericInput(session, "highDisplayedQuantilePercentage", value = values$highDisplayedQuantilePercentage)
        }
        if(!is.null(values$displayMedianAndQuantilesFlag) && values$displayMedianAndQuantilesFlag!=0) {
          values$currentDatasetHistogram <- plottedFigure()
          output$plotHist <- renderPlot({values$currentDatasetHistogram})
        }
      }
    })
    
    # Redraw figure on change of the high displayed quantile percentage, if median and quantiles display is active
    # If the 'keep quantiles symetric' flag is active, adjust the low quantile percentage accordingly before updating figure
    observeEvent(input$highDisplayedQuantilePercentage, {
      if(values$highDisplayedQuantilePercentage != input$highDisplayedQuantilePercentage) {
        values$highDisplayedQuantilePercentage = input$highDisplayedQuantilePercentage
        if(!is.null(values$keepDisplayedQuantilesSymetricFlag) && values$keepDisplayedQuantilesSymetricFlag!=0) {
          values$lowDisplayedQuantilePercentage = 1 - values$highDisplayedQuantilePercentage
          updateNumericInput(session, "lowDisplayedQuantilePercentage", value = values$lowDisplayedQuantilePercentage)
        }
        if(!is.null(values$displayMedianAndQuantilesFlag) && values$displayMedianAndQuantilesFlag!=0) {
          values$currentDatasetHistogram <- plottedFigure()
          output$plotHist <- renderPlot({values$currentDatasetHistogram})
        }
      }
    })
    
    # Checkbox: keep quantiles symetric (respective to the median value), when one is changed, change the other percentage accordingly
    output$keepDisplayedQuantilesSymetricFlag <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      checkboxInput("keepDisplayedQuantilesSymetricFlag", getLabelOrPrompt("keepDisplayedQuantilesSymetricFlagLabel", displayedLabelsAndPrompts), TRUE, width="100%")
    })
    
    # Upadate reactive values when 'keep quantiles symetric' is checked or unchecked
    observeEvent(input$keepDisplayedQuantilesSymetricFlag, {
      values$keepDisplayedQuantilesSymetricFlag = input$keepDisplayedQuantilesSymetricFlag
    })
    
    # Update reactive value and displayed text when the number of decimal values to be displayed changes
    observeEvent(input$nDecimalPlaces, {
      if(is.na(input$nDecimalPlaces) || values$nDecimalPlaces != input$nDecimalPlaces) {
        values$nDecimalPlaces = input$nDecimalPlaces
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
        values$clickedBinInfoText <- getClickedBinInfoText()
        output$clickedBinInfo <- renderUI({HTML(values$clickedBinInfoText)})
      }
    })
    
    # Checkbox: display frequencies (percentage of the total number of values) instead of counts
    output$displayAsFrequenciesFlag <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      checkboxInput("displayAsFrequenciesFlag", getLabelOrPrompt("displayAsFrequenciesFlagLabel", displayedLabelsAndPrompts), FALSE, width="100%")
    })
    
    # If the histogram type setting (frequencies or counts) has changed, update plot
    observeEvent(input$displayAsFrequenciesFlag, {
      if(values$displayAsFrequenciesFlag != input$displayAsFrequenciesFlag) {
        values$lastParameterChangeTime = Sys.time()
        values$displayAsFrequenciesFlag <- input$displayAsFrequenciesFlag
        values$currentDatasetHistogram <- plottedFigure()
        output$plotHist <- renderPlot({values$currentDatasetHistogram})
      }
    })
    
    # Checkbox: overlay density curve
    output$displayDensityFlag <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      checkboxInput("displayDensityFlag", getLabelOrPrompt("displayDensityFlagLabel", displayedLabelsAndPrompts), FALSE, width="100%")
    })
    
    # If the overlayed density curve display setting has changed, update plot
    observeEvent(input$displayDensityFlag, {
      if(values$displayDensityFlag != input$displayDensityFlag) {
        values$lastParameterChangeTime = Sys.time()
        values$displayDensityFlag <- input$displayDensityFlag
        values$currentDatasetHistogram <- plottedFigure()
        output$plotHist <- renderPlot({values$currentDatasetHistogram})
      }
    })
    
    # Checkbox: overlay normal distribution with the same mean and standard deviation
    output$displayNormFlag <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      checkboxInput("displayNormFlag", getLabelOrPrompt("displayNormFlagLabel", displayedLabelsAndPrompts), FALSE, width="100%")
    })
    
    # If the overlayed normal curve display setting has changed, update plot
    observeEvent(input$displayNormFlag, {
      if(values$displayNormFlag != input$displayNormFlag) {
        values$lastParameterChangeTime = Sys.time()
        values$displayNormFlag <- input$displayNormFlag
        values$currentDatasetHistogram <- plottedFigure()
        output$plotHist <- renderPlot({values$currentDatasetHistogram})
      }
    })
    
    # Checkbox: display the details of the values in the clicked bin in a table
    output$displayClassesDetailInTableFlag <- renderUI({
      df <- values$filedata
      if (is.null(df)) return(NULL)
      checkboxInput("displayClassesDetailInTableFlag", getLabelOrPrompt("displayClassesDetailInTableFlagLabel", displayedLabelsAndPrompts), TRUE, width="100%")
    })
    
    # Get a numeric vector with data to be plotted in the histogram, and update the bins number slider according to the vector length and values range
    getHistogramData <- function(){
      if(debugFlag){
        cat(file=stderr(), "getHistogramData() start", "values$dependentVariable = ", values$dependentVariable, "values$filterVariable = ", values$filterVariable, "\n")
        if(interactiveDebugFlag) browser()
      }
      
      if (is.null(input$datafile)) {
        return(NULL)
      }
      # Get the filtered data frame
      df=values$filteredFiledata
      
      # if (is.null(df) || is.null(values$filterVariable) || is.null(values$filterValues) || length(values$filterValues)==0) {
      if (is.null(df) || is.null(input$depVar) || is.null(values$dependentVariable) || values$dependentVariable=="\u00A0" || values$dependentVariable==0) {  
        return(NULL)
      }
      else {
        if(values$dependentVariable != "") {
          # And extract the column with the chosen dependent variable, discarding possible NA values
          allValues = unlist(df[values$dependentVariable])
          if(values$nonNumericColsIncludedInDependentVariableChoiceFlag!=0 && !is.numeric(allValues)) {
            allValues = suppressWarnings(as.numeric(as.character(allValues)))
          }
          finiteValues = allValues[is.finite(allValues)]
          if(length(finiteValues)==0) {
            return(NULL)
          } else {
            # Update the min and max values if set to automatic mode
            if(is.na(values$histogramLowLimit) || (!is.null(values$histogramLimitsMode) && values$histogramLimitsMode==getLabelOrPrompt("histogramLimitsModeChoiceAuto", displayedLabelsAndPrompts))) {
              minVal = min(finiteValues)
              maxVal = max(finiteValues)
              dataRange = maxVal - minVal
              limitsInputStep = 10^round(log10(dataRange/nStepsInScaleSettings))
              updateNumericInput(session, "histogramLowLimit", value = minVal, step = limitsInputStep)
              updateNumericInput(session, "histogramHighLimit", value = maxVal, step = limitsInputStep)
            }
            
            # Update the slider for bins number selection if set to automatic mode
            if(!is.null(values$nBinsUpdateOnDataChange) && values$nBinsUpdateOnDataChange==getLabelOrPrompt("nBinsUpdateOnDataChangeChoiceAuto", displayedLabelsAndPrompts)) {
              updateNBinsSlider(finiteValues)
            }
            
            return(finiteValues)
          }
        } else {
          return(NULL)
        }
      }
    }
    
    # Update the limits and default value of the slide for bins number selection according to data count
    updateNBinsSlider <- function(dataVector) {
      sliderMaxValue = length(dataVector)
      sliderDefaultValue = ceiling(log2(sliderMaxValue) + 1)
      # sliderDefaultValue = round(sqrt(sliderMaxValue))
      if(sliderDefaultValue>=2) {
        sliderMinValue = 2
      } else {
        sliderMinValue = 1
      }
      if(sliderDefaultValue < 3) {
        sliderDefaultValue = min(sliderMaxValue, 3)
      }
      updateSliderInput(session, "bins",
                        label = getLabelOrPrompt("sliderInputLabel", displayedLabelsAndPrompts),
                        value = sliderDefaultValue,
                        min = sliderMinValue,
                        max = sliderMaxValue,
                        step = 1
      )
      values$nBins = sliderDefaultValue
    }
    
    # Get the breaks used in histogram plotting from the values
    getHistogramBreaks <- function() {
      if (is.null(input$datafile) || is.null(values$histogramData)) {
        return(NULL)
      } else {
        currentDataset = values$histogramData
        if(length(currentDataset)>1) {
          # If limits have not been set yet, set them according to the min and max values in the data
          if(is.na(values$histogramLowLimit) || is.na(values$histogramHighLimit)) {
            values$histogramLowLimit = min(currentDataset)
            values$histogramHighLimit = max(currentDataset)
          }
          return(seq(values$histogramLowLimit, values$histogramHighLimit, length.out = values$nBins + 1))
        } else {
          return(NULL)
        }
      }
    }
    
    # When the flag for clicked bin detail display is changed, update table display
    observeEvent(input$displayClassesDetailInTableFlag, {
      if(values$displayClassesDetailInTableFlag != input$displayClassesDetailInTableFlag) {
        values$lastParameterChangeTime = Sys.time()
        values$displayClassesDetailInTableFlag <- input$displayClassesDetailInTableFlag
        if(values$displayClassesDetailInTableFlag!=0 && !is.null(values$clickedSubset) && nrow(values$clickedSubset)>0) {
          output$selectedDataDetailsTable <- renderRHandsontable({
            rhandsontable(values$clickedSubset, selectCallback = FALSE, readOnly = TRUE, stretchH = "all", rowHeaderWidth = 0) %>%
              hot_cols(columnSorting = TRUE)
          })
        } else {
          output$selectedDataDetailsTable <- renderRHandsontable({NULL})
        }
      }
    })
    
    # Set the image format of the figure to be exported
    output$plotDownloadFormat <- renderUI({
      selectInput("plotDownloadFormat", getLabelOrPrompt("plotDownloadFormatLabel", displayedLabelsAndPrompts),availableOutputFormats)
    })
    
    # Set the file format for selected data export
    output$selectedDataDownloadFormat <- renderUI({
      selectInput("selectedDataDownloadFormat", getLabelOrPrompt("selectedDataDownloadFormatLabel", displayedLabelsAndPrompts),availableDataFileOutputFormats)
    })
    
    # Set the width of the figure when exporting to image file
    output$exportedPlotWidth <- renderUI({
      numericInput("exportedPlotWidth", getLabelOrPrompt("exportedPlotWidthLabel", displayedLabelsAndPrompts), value = defaultExportedPlotWidth)
    })
    
    # Set the height of the figure when exporting to image file
    output$exportedPlotHeight <- renderUI({
      numericInput("exportedPlotHeight", getLabelOrPrompt("exportedPlotHeightLabel", displayedLabelsAndPrompts), value = defaultExportedPlotHeight)
    })
    
    # Set the units for figure export as image
    output$exportedPlotUnits <- renderUI({
      selectInput("exportedPlotUnits", getLabelOrPrompt("exportedPlotUnitsLabel", displayedLabelsAndPrompts),availableUnitsForImageExport)
    })
    
    # Allow the user to download the figure as an image file
    output$downloadPlot <- downloadHandler(
      filename = function() {
        # Get the default image file name using dataset file name and bins number
        str_c(tools::file_path_sans_ext(input$datafile),
               getLabelOrPrompt("histogramSubstringInExportedImageName", displayedLabelsAndPrompts),
               values$nBins,
               getLabelOrPrompt("binsSubstringInExportedImageName", displayedLabelsAndPrompts),
               availableOutputFormatExtensions[which(availableOutputFormats == input$plotDownloadFormat)])
      },
      # Export the image with ggsave, using format and size parameters picked by the user
      content = function(file) {
        ggsave(file, plot = plottedFigure(), device = tolower(input$plotDownloadFormat), width = input$exportedPlotWidth, height = input$exportedPlotHeight, units = exportedImagesUnit)
      }
    )
    
    # Allow the user to download selected data
    output$downloadSelectedData <- downloadHandler(
      filename = function() {
        # Get the default image file name using dataset file name and bins number
        str_c(tools::file_path_sans_ext(input$datafile),
               getLabelOrPrompt("defaultSuffixInExportedDataFileName", displayedLabelsAndPrompts),
              availableDataFileOutputFormatExtensions[which(availableDataFileOutputFormats == input$selectedDataDownloadFormat)])
      },
      # Export selected data, using format picked by the user
      content = function(file) {
        if(!is.null(input$selectedDataDownloadFormat) & !is.null(values$clickedSubset) && nrow(values$clickedSubset)>0) {
          if(input$selectedDataDownloadFormat=="XLSX") {
            write_xlsx(values$clickedSubset, path = file, format_headers = F)
          } else if(input$selectedDataDownloadFormat=="TSV") {
            write_tsv(values$clickedSubset, file = file)
          } else if(input$selectedDataDownloadFormat=="CSV") {
            write_csv(values$clickedSubset, file = file)
          }
        }
      }
    )
    
    # Get descriptive statistics on the current data subset and format information in a characer string
    descriptiveStatText <- function() {
      if(!is.null(input$datafile)) {
        currentDataset = values$histogramData
        # currentDatasetHistogram = values$currentDatasetHistogram
        # if(!is.null(currentDataset) && !is.null(currentDatasetHistogram)) {
        if(!is.null(currentDataset)) {
          # Compute mode and range not directly given by base functions
          uniqueVals = unique(currentDataset)
          modeInCurrentDataset = uniqueVals[which.max(tabulate(match(currentDataset, uniqueVals)))]
          rangeInCurrentDataset = max(currentDataset) - min(currentDataset)
          nDecimalPlaces = values$nDecimalPlaces
          
          # HTML text formatting
          return(paste0(
            "<p><b>", getLabelOrPrompt("descriptiveStatisticsGeneralInfo", displayedLabelsAndPrompts), "</b><br>",
            getLabelOrPrompt("nValuesPrompt", displayedLabelsAndPrompts), length(currentDataset), " — ",
            getLabelOrPrompt("meanValuePrompt", displayedLabelsAndPrompts), round(mean(currentDataset), digits = nDecimalPlaces), " — ",
            getLabelOrPrompt("sdValuePrompt", displayedLabelsAndPrompts), round(sd(currentDataset), digits = nDecimalPlaces), "<br>",
            getLabelOrPrompt("medianValuePrompt", displayedLabelsAndPrompts), round(median(currentDataset), digits = nDecimalPlaces), " — ",
            getLabelOrPrompt("firstQuartileValuePrompt", displayedLabelsAndPrompts), round(quantile(currentDataset,.25), digits = nDecimalPlaces), " — ",
            getLabelOrPrompt("thirdQuartileValuePrompt", displayedLabelsAndPrompts), round(quantile(currentDataset,.75), digits = nDecimalPlaces), " — ",
            getLabelOrPrompt("interquartileRangeValuePrompt", displayedLabelsAndPrompts), round(quantile(currentDataset,.75) - quantile(currentDataset,.25), digits = nDecimalPlaces), "<br>",
            getLabelOrPrompt("minValuePrompt", displayedLabelsAndPrompts), round(min(currentDataset), digits = nDecimalPlaces), " — ",
            getLabelOrPrompt("maxValuePrompt", displayedLabelsAndPrompts), round(max(currentDataset), digits = nDecimalPlaces), " — ",
            getLabelOrPrompt("rangeValuePrompt", displayedLabelsAndPrompts), round(rangeInCurrentDataset, digits = nDecimalPlaces), " — ",
            getLabelOrPrompt("modeValuePrompt", displayedLabelsAndPrompts), round(modeInCurrentDataset, digits = nDecimalPlaces),
            "</p>"
          ))
        } else {
          return("")
        }
      } else {
        return("")
      }
    }
    
    updateBinWidthText <- function() {
      if(!is.null(input$datafile)) {
        currentDataset = values$histogramData
        if(!is.null(currentDataset)) {
          displayedRange = values$histogramHighLimit - values$histogramLowLimit
          displayedText = paste0("<p>", getLabelOrPrompt("binwidthPrompt", displayedLabelsAndPrompts), round(displayedRange / values$nBins, digits = values$nDecimalPlaces), "</p>")
        } else
          displayedText = NULL
      } else
        displayedText = NULL
      output$binWidthDisplay <- renderUI({HTML(displayedText)})
    }
    
    # Get a formatted version of the general information on bin selection (not specific to a specific bin)
    clickedBinInfoBaseText <- function(){
      if(!is.null(input$datafile)) {
        currentDataset = values$histogramData
        if(!is.null(currentDataset)) {
          displayedRange = values$histogramHighLimit - values$histogramLowLimit
          return(paste0(
                # Format text
                "<p><b>", getLabelOrPrompt("binClickingGeneralInfo", displayedLabelsAndPrompts), "</b><br>",
                getLabelOrPrompt("binClickingPrompt", displayedLabelsAndPrompts),
                "</p>"
          ))
        } else {
          return("")
        }
      } else {
        return("")
      }
    }
    
    # Get information on the clicked bin and format it
    getClickedBinInfoText <- function(){
      if(!is.null(values$clickedSubset)) {
        # Get the proportion of the total data and the appropriate rounding
        currentDataset = values$histogramData
        currentDatasetHistogram = values$currentDatasetHistogram
        # Get detailed info about the plot using ggplot_build
        currentDatasetHistogramInfo=ggplot_build(currentDatasetHistogram)
        currentDatasetHistogramData = currentDatasetHistogramInfo$data[[1]]
        
        # Get the ratio of the total counts in the clicked bin
        percentageTotalCountInCurrentBin = 100 * currentDatasetHistogramData$count[values$clickedBinIndex] / length(currentDataset)
        if(percentageTotalCountInCurrentBin>=1) {
          nDecimalPlacesProportionTotalCount = 2
        } else {
          nDecimalPlacesProportionTotalCount = 2 - log10(percentageTotalCountInCurrentBin)
        }
        # Append info about the clicked bin to display it in the texte field
        return(
          paste0(
            clickedBinInfoBaseText(),
            "<p>",
            getLabelOrPrompt("clickedBinPromptPart1", displayedLabelsAndPrompts), values$clickedBinIndex, "/", values$nBins, getLabelOrPrompt("clickedBinPromptPart2", displayedLabelsAndPrompts),
            round(values$clickedBinLowLimit, digits = values$nDecimalPlaces), " - ", round(values$clickedBinHighLimit, digits = values$nDecimalPlaces), "), ",
            nrow(values$clickedSubset), getLabelOrPrompt("nValuesInClickedBinPrompt", displayedLabelsAndPrompts),
            round(percentageTotalCountInCurrentBin, digits = nDecimalPlacesProportionTotalCount), getLabelOrPrompt("proportionOfTotalInClickedBinPrompt", displayedLabelsAndPrompts),
            "</p>"
          )
        )
      } else {
        return(clickedBinInfoBaseText())
      }
    }
    
    # When a bin is clicked, get the corresponding data subset, display it in the table if requested, and update text display to give information on the clicked bin
    observeEvent(input$plot_click, {
      currentDataset = values$histogramData
      currentDatasetHistogram = values$currentDatasetHistogram
      
      # Get detailed info about the plot using ggplot_build
      currentDatasetHistogramInfo=ggplot_build(currentDatasetHistogram)
      # Check that the clicked point is within limits
      currentDatasetHistogramRange <- currentDatasetHistogramInfo$layout$panel_params[[1]]
      x.range = currentDatasetHistogramRange$x.range
      y.range = currentDatasetHistogramRange$y.range
      if(!is.null(input$plot_click$x) && is.finite(input$plot_click$x) && input$plot_click$x>=x.range[1]  && input$plot_click$x<=x.range[2]) {
        # Get the index of the clicked bin and the corresponding breaks
        currentDatasetHistogramData = currentDatasetHistogramInfo$data[[1]]
        #currentDatasetHistogramBreaks = c(currentDatasetHistogramData$xmin,currentDatasetHistogramData$xmax[nrow(currentDatasetHistogramData)])
        currentDatasetHistogramBreaks = getHistogramBreaks()
        clickedBinIndex = findInterval(input$plot_click$x, currentDatasetHistogramBreaks, all.inside = TRUE)
        clickedBinLowLimit = currentDatasetHistogramBreaks[clickedBinIndex]
        clickedBinHighLimit = currentDatasetHistogramBreaks[clickedBinIndex+1]
        # If it's the first bin, make sure to include the lowest possible value
        if(clickedBinIndex==1) {
          clickedBinLowLimitExtended = -Inf
        } else {
          clickedBinLowLimitExtended = clickedBinLowLimit
        }
        
        # Extract the corresponding subset from the original dataframe
        currentDatasetFull = values$filteredFiledata
        if(values$nonNumericColsIncludedInDependentVariableChoiceFlag!=0 && !is.numeric(currentDatasetFull[values$dependentVariable])) {
          clickedSubset = currentDatasetFull[which(suppressWarnings(as.numeric(as.character(unlist(currentDatasetFull[values$dependentVariable]))))>clickedBinLowLimitExtended & suppressWarnings(as.numeric(as.character(unlist(currentDatasetFull[values$dependentVariable]))))<=clickedBinHighLimit),]
        } else {
          clickedSubset = currentDatasetFull[which(currentDatasetFull[values$dependentVariable]>clickedBinLowLimitExtended & currentDatasetFull[values$dependentVariable]<=clickedBinHighLimit),]
        }
        clickedSubset = as.data.frame(clickedSubset)
        colnames(clickedSubset) = colnames(currentDatasetFull)
        values$clickedSubset = clickedSubset
        values$clickedBinIndex = clickedBinIndex
        values$clickedBinLowLimit = clickedBinLowLimit
        values$clickedBinHighLimit = clickedBinHighLimit
        
        # Update figure to highlight selected bin
        values$currentDatasetHistogram <- plottedFigure()
        output$plotHist <- renderPlot({values$currentDatasetHistogram})
        
        # Update table display
        if(values$displayClassesDetailInTableFlag!=0 && !is.null(values$clickedSubset) && nrow(values$clickedSubset)>0) {
          output$selectedDataDetailsTable <- renderRHandsontable({rhandsontable(values$clickedSubset, selectCallback = FALSE, readOnly = TRUE, stretchH = "all", rowHeaderWidth = 0)})
        } else {
          output$selectedDataDetailsTable <- renderRHandsontable({NULL})
        }
      }
      values$clickedBinInfoText <- getClickedBinInfoText()
      output$clickedBinInfo <- renderUI({HTML(values$clickedBinInfoText)})
    })
    
    # Build the histogram with either counts of frequencies, and with overlayed curves if requested.
    # Use reactivePoll to detect changes in data or parameters which require figure update, taking last parameter change time as reference.
    plottedFigure <- reactivePoll(figureDisplayRefreshRateMilliseconds, session, checkFunc = function() {
      values$lastParameterChangeTime
    },
    valueFunc = function() {
      # Build the figure
      if(!is.null(input$datafile) && !is.null(values$filteredFiledata)) {
        currentDataset = values$histogramData
        if(!is.null(currentDataset) && !anyNA(currentDataset) && !any(!is.finite(currentDataset)) && length(currentDataset)>1) {
          # cat(file=stderr(), "plottedFigure" ,"\n")
          
          par(mar=c(5,3,2,2)+0.1) # No additional space aroud the histogram
          # Create a dataframe with the values to be plotted and the horizontal scale needed to contraol x-axis display in various conditions
          if(length(currentDataset)>1) {
            horizScale = seq(min(currentDataset), max(currentDataset), by = (max(currentDataset) - min(currentDataset))/(length(currentDataset)-1))
          } else {
            horizScale = currentDataset
          }

          # Set bar colors depending on the possible bin selection
          df = data.frame(x = currentDataset, horizScale = horizScale, barColor = histogramBarsColor)
          barsColors = rep(histogramBarsColor, values$nBins)
          if(!is.null(values$clickedBinIndex) && !is.na(values$clickedBinIndex))
            barsColors[values$clickedBinIndex] = histogramSelectedBinColor
          currentDatasetHistogram = ggplot(df, aes(x=x))
          
          if (values$displayAsFrequenciesFlag != 0) {
            # Histogram with bins height displayed as percentage of the total count
            currentDatasetHistogram = currentDatasetHistogram +
              # geom_histogram(aes(y = (..count..)/sum(..count..)), breaks = getHistogramBreaks(), color = "white", fill = barsColors) +
              geom_histogram(aes(y = after_stat(count)/sum(after_stat(count))), breaks = getHistogramBreaks(), color = "white", fill = barsColors) +
              ylab(getLabelOrPrompt("histogramVerticalAxisLabelFrequencies", displayedLabelsAndPrompts)) +
              scale_x_continuous(expand = c(0, 0)) +
              scale_y_continuous(labels=scales::percent, expand = c(0, 0))
            # Compute the multiplier to be applied to density and equivalent normal curve if requested (requires access to plot values detail using ggplot_build)
            currentDatasetHistogramDataFrame = ggplot_build(currentDatasetHistogram)$data[[1]]
            curvesDisplayMultiplier <- max(currentDatasetHistogramDataFrame$count/sum(currentDatasetHistogramDataFrame$count)) / max(currentDatasetHistogramDataFrame$density)
          } else {
            # Classical histogram with counts display
            currentDatasetHistogram = currentDatasetHistogram +
              # geom_histogram(aes(y = ..count..), breaks = getHistogramBreaks(), color = "white", fill = barsColors) +
              geom_histogram(aes(y = after_stat(count)), breaks = getHistogramBreaks(), color = "white", fill = barsColors) +
              ylab(getLabelOrPrompt("histogramVerticalAxisLabelCounts", displayedLabelsAndPrompts))
            # Compute the multiplier to be applied to density and equivalent normal curve if requested (requires access to plot values detail using ggplot_build)
            currentDatasetHistogramDataFrame = ggplot_build(currentDatasetHistogram)$data[[1]]
            curvesDisplayMultiplier <- (currentDatasetHistogramDataFrame$count / currentDatasetHistogramDataFrame$density)[1]
          }
          
          # Display overlayed density curve if requested
          if (values$displayDensityFlag != 0) {
            if (values$displayAsFrequenciesFlag != 0) {
              currentDatasetHistogram = currentDatasetHistogram + 
                # geom_line(aes_string(y = paste0("..density.. * ",curvesDisplayMultiplier)), stat="density", linewidth = overlaidCurvesSize, colour=overlaidDensityCurveColor)
                geom_line(aes(y = after_stat(density)*curvesDisplayMultiplier), stat="density", linewidth = overlaidCurvesSize, colour=overlaidDensityCurveColor)
            } else {
              currentDatasetHistogram = currentDatasetHistogram + 
                # geom_line(aes_string(y = paste0("..density.. * ",curvesDisplayMultiplier)), stat="density", linewidth = overlaidCurvesSize, colour=overlaidDensityCurveColor)
                geom_line(aes(y = after_stat(density)*curvesDisplayMultiplier), stat="density", linewidth = overlaidCurvesSize, colour=overlaidDensityCurveColor)
            }
          }

          # Display overlayed equivalent normal curve (same mean and sd as in the plotted dataset) if requested
          if (values$displayNormFlag != 0) {
            scaled_dnorm <- function(x,mean,sd) {dnorm(x,mean=mean,sd=sd) * curvesDisplayMultiplier}
            if (values$displayAsFrequenciesFlag != 0) {
              currentDatasetHistogram = currentDatasetHistogram + 
                stat_function(fun=scaled_dnorm, color=overlaidNormalCurveColor, args=list(mean=mean(currentDataset), sd=sd(currentDataset)), lwd = overlaidCurvesSize)
            } else {
              currentDatasetHistogram = currentDatasetHistogram + 
                stat_function(fun=scaled_dnorm, color=overlaidNormalCurveColor, args=list(mean=mean(currentDataset), sd=sd(currentDataset)), lwd = overlaidCurvesSize)
            }
          }
          
          # Display overlayed mean and confidence interval if requested
          if(!is.null(values$displayMeanAndConfidenceIntervalFlag) && values$displayMeanAndConfidenceIntervalFlag!=0) {
            
            # Display mean
            currentDatasetHistogram = currentDatasetHistogram +
              geom_vline(aes(xintercept=mean(currentDataset)), color=meanAndCIlinesColor, linewidth = overlaidVerticalLinesSize, linetype = overlaidVerticalLinesTypeMain)
            
            # Get confidence interval according to options
            if(values$confidenceIntervalComputationMethod!=getLabelOrPrompt("confidenceIntervalComputationMethodParametricLabel", displayedLabelsAndPrompts)) {
              currentCI = getBootstrappedConfidenceInterval(currentDataset,values$confidenceIntervalProb,values$nReplicationsForBootstrapCIcomputation)
            } else {
              currentCI = getParametricConfidenceInterval(currentDataset,values$confidenceIntervalProb)
            }
            # Display CI low limit
            currentDatasetHistogram = currentDatasetHistogram +
              geom_vline(aes(xintercept=currentCI[1]), color=meanAndCIlinesColor, linewidth = overlaidVerticalLinesSize, linetype = overlaidVerticalLinesTypeSecondary)
            # Display CI high limit
            currentDatasetHistogram = currentDatasetHistogram +
              geom_vline(aes(xintercept=currentCI[2]), color=meanAndCIlinesColor, linewidth = overlaidVerticalLinesSize, linetype = overlaidVerticalLinesTypeSecondary)
          }
          
          # Display overlayed median and quantiles if requested
          if(!is.null(values$displayMedianAndQuantilesFlag) && values$displayMedianAndQuantilesFlag!=0) {
            
            # Display median
            currentDatasetHistogram = currentDatasetHistogram +
              geom_vline(aes(xintercept=median(currentDataset)), color=medianAndQuantilesLinesColor, linewidth = overlaidVerticalLinesSize, linetype = overlaidVerticalLinesTypeMain)
            
            # Get and display low quantile value
            lowDisplayedQuantileValue = quantile(currentDataset, probs = values$lowDisplayedQuantilePercentage)
            currentDatasetHistogram = currentDatasetHistogram +
              geom_vline(aes(xintercept=lowDisplayedQuantileValue), color=medianAndQuantilesLinesColor, linewidth = overlaidVerticalLinesSize, linetype = overlaidVerticalLinesTypeSecondary)
            # Get and display high quantile value
            highDisplayedQuantileValue = quantile(currentDataset, probs = values$highDisplayedQuantilePercentage)
            currentDatasetHistogram = currentDatasetHistogram +
              geom_vline(aes(xintercept=highDisplayedQuantileValue), color=medianAndQuantilesLinesColor, linewidth = overlaidVerticalLinesSize, linetype = overlaidVerticalLinesTypeSecondary)
          }
          # Apply general graphical parameters to the plot to make it nicer
          currentDatasetHistogram = currentDatasetHistogram +
            theme_bw() +
            theme(panel.border=element_rect(fill=NA, colour = "grey")) +
            theme(text = element_text(size=textSize), axis.text.x = element_text(size=textSizeAxes), axis.text.y = element_text(size=textSizeAxes)) +
            xlab(values$dependentVariable)
          
          return(currentDatasetHistogram)
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })
    
    # Display the figure
    output$plotHist <- renderPlot({
          if(!is.null(values$currentDatasetHistogram)) {
            values$currentDatasetHistogram
          }
        # }
      # }
    })
    
    getBootstrappedConfidenceInterval <- function (dataVector, confProb, nReplications) {
      mean.fun <- function(dat, idx) mean(dat[idx], na.rm = TRUE)
      bootstrappedMean = boot(data = dataVector, statistic = mean.fun, R = nReplications)
      CIobject = boot.ci(boot.out = bootstrappedMean, conf = confProb, type = "perc")
      return((CIobject$percent[1,])[4:5])
    }
    
    getParametricConfidenceInterval <- function (dataVector, confProb) {
      meanVal = mean(dataVector)
      sdVal = sd(dataVector)
      errorWidth = qnorm(confProb)*sdVal/sqrt(length(dataVector))
      return(c(meanVal-errorWidth,meanVal+errorWidth))
    }

  })
