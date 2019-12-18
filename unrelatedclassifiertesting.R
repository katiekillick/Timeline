### K-fold cross- validation of Naive Bayes pressure ulcer model ###
### 2019xxxx Pressure Ulcer preprocessing.R (insert correct filename) Should be run first

library(magrittr) # for the pipe
library(tictoc)

#### Functions ####

removeStandardText <-
  function(selectedCol, texts = standardTextsubbed) {
    
    selectedCol <- as.character(selectedCol)
    
    for (text in texts) {
      col <- gsub(text, '', selectedCol, ignore.case = TRUE)
      selectedCol <- col
    }
    
    return(selectedCol)
    
  }


## DFM Creation function ##
createDfm <- function(data, setting, scheme) {
  if (setting == 1) {
    dfm <- 
      quanteda::dfm(data) %>%
      quanteda::dfm(stem = FALSE)%>%
      quanteda::dfm_weight(scheme = scheme)
    
    
  } else if (setting == 2) {
    dfm <- 
      quanteda::dfm(data) %>%
      quanteda::dfm(stem = TRUE)%>%
      quanteda::dfm_weight(scheme = scheme)
    
    
  } else if (setting == 3) {
    dfm <-
      quanteda::dfm(data) %>%
      quanteda::dfm(
        stem = TRUE,
        remove = quanteda::stopwords("english"),
        remove_numbers = TRUE,
        remove_punct = TRUE,
        remove_symbols = TRUE) %>%
      quanteda::dfm_weight(scheme = scheme)
    
    
  } else if (setting == 4) {
    dfm <-
      quanteda::dfm(data) %>%
      quanteda::dfm(
        stem = TRUE,
        remove = quanteda::stopwords("english"),
        remove_numbers = TRUE,
        remove_punct = TRUE,
        remove_symbols = TRUE
      ) %>%
      quanteda::dfm_trim(min_termfreq = 5, min_docfreq = 2)%>%
      quanteda::dfm_weight(scheme = scheme)
    
  } else if (setting == 5) {
    dfm <-
      quanteda::dfm(data) %>%
      quanteda::dfm(
        stem = TRUE,
        remove = quanteda::stopwords("english"),
        remove_numbers = TRUE,
        remove_punct = TRUE,
        remove_symbols = TRUE
      ) %>%
      quanteda::dfm_trim(min_termfreq = 10, min_docfreq = 5)%>%
      quanteda::dfm_weight(scheme = scheme)
    
  }
  return(dfm)
}


make_corpus <- function(dataset, textField, k = k){
  if (is.numeric(k)== FALSE){
    stop("k must be a number. This can be set outside the function")
  }
  # Step 1: shuffle the data (set seed for reproducibility)

  # shuffle the data
  shuffledData <- sample(dataset)
  num_docs <- nrow(shuffledData)

  # create a vector of letters as a variable for assigning folds
  alphvec <- letters[1:k]
  alphvec <- rep_len(alphvec, num_docs)
  shuffledData$alphvec <- alphvec


  # create corpus object
  corp <- quanteda::corpus(shuffledData, text_field = textField)

  return(corp)
}


# cross-validation function
xvalidate <- function(corp, settingChoice, schemeChoice, priorChoice, class = "classBinPU", k = 5){
  if (is.numeric(k)== FALSE){
    stop("k must be a number. This can be set outside the function")
  }
  if (settingChoice > 5 | settingChoice < 1) {
    stop("setting must be between 1 and 5")
  }
  
  actualClassList <- list()
  predictedClassList <- list()
  
  for (i in letters[1:k]) {
    
    # Step 3: remove one set, then apply pre-processing (do not apply pre-processing first)
    # this is potentially not so important with text but might as well stick to the rules- works
    # quite well with quanteda's workflow as well, where preprocessing is applied at DFM creation point
    # select only the current fold for testing
    trainData <- quanteda::corpus_subset(corp, alphvec != i) 
    testData  <- quanteda::corpus_subset(corp, alphvec == i) 
     
    # trainData <- corp %>% dplyr::filter(alphvec != i)
    # Apply preprocessing here
    # Standard text removal should have been done in data preparation- this will affect the accuracy of the classifier
    # Create training a DFM
    trainDfm <- createDfm(trainData, setting = settingChoice, scheme = schemeChoice)
    
    
    # Create testing DFM
    testDfm <- createDfm(testData, setting = settingChoice, scheme = schemeChoice)
    
    # Train the model
    tmod_nb <- quanteda::textmodel_nb(trainDfm, quanteda::docvars(trainDfm, class), prior = priorChoice)
    # docfreq prior was selected as it seemed sensible. It improved results in testing (as opposed to uniform prior)
    # termfreq prior not tested as of yet
    
    # match the DFMs so both have the same features
    dfmat_matched <- quanteda::dfm_match(testDfm, features = quanteda::featnames(trainDfm))
    
    
    actual_class <- quanteda::docvars(dfmat_matched, class)
    predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
    
    # store the vectors for comparison after loop has run 
    actualClassList[[i]] <- actual_class   
    predictedClassList[[i]] <- predicted_class
    
    
  }
  
  
  # store results in a list
  results <- list()
  # will need to extract the "byClass" attribute to calculate average performance
  justbyClass <- list()
  
  for (l in letters[1:k]) {
    # create confusion matrices
    results[[l]] <- caret::confusionMatrix(as.factor(predictedClassList[[l]]),
                                           as.factor(actualClassList[[l]]),
                                           positive = "1")
    # store the byClass attribute (contains sensitivity, specificity, F1 score etc) separately
    justbyClass[[l]] <- results[[l]]$byClass
    
  }
  
  # convert to data frame
  resultsDf <- as.data.frame(justbyClass)
  
  # average all the results
  meanResults <- rowMeans(resultsDf)
  # turn to dataframe also
  meanResults <- data.frame(meanResults)
  
  return(meanResults)
}

#### Data and cleaning ####

# the data file
fileName <- "20190830 More pressure sores for labelling.csv"

# this variable will need to be changed to the synced file location
fileLocation <-
  "C://Users//killickk//Care Quality Commission//Qualitative Analysis Team - PU Falls Classifier//PUF Data/"


#read in data
dataset <- read.csv(paste0(fileLocation, fileName))
# check colnames
colnames(dataset)

# set up class variable
dataset$classBinPU <- as.numeric(dataset$class == "pressure ulcer")

#### Clean out standard text ####

standardTexts <-
  c(
    "Nb: Refer to any people using unique identifiers or codes; do not provide names",
    "Please tell us what the service did in response to the injury",
    "Please provide complete details of the incident and ensure you consider and include:",
    "Was the person known to be at risk of this type of incident/injury?",
    "What was in place to mitigate the risk ie:",
    "risk assessment",
    "equipment to minimize the risk of a fall/injury",
    "input from a specialist falls team",
    "tissue viability input",
    "staff observations",
    "What immediate steps have you taken to mitigate further risks to the person and/or others?",
    "The Duty of Candour is a regulation under the Health and Social Care Act",
    "It tells providers they must be open and transparent with people about their care",
    "and treatment, as well as with people acting on their behalf",
    "It sets out some specific",
    "things providers must do when something goes wrong with someone's care or",
    "treatment, including telling them what has happened, giving support, giving truthful",
    "information and apologising",
    "Does the Duty of Candour apply to this incident?",
    "Have you told the people affected by the incident what has happened?",
    "No answer provided",
    "If this is a notifiable safety incident under the 'Duty of Candour'",
    "Regulated Activities Regulations 2014",
    "have you notified the 'relevant person' about this incident?",
    "Regulation 20",
    "Regulated Activities Regulations",
    "http://www.cqc.org.uk/content/regulation-20-duty-candour",
    "NA TRUE TRUE",
    "No answer provided",
    "null"
  )

standardTextsubbed <- c()

# sub in (\\s)* for the spaces as often spaces are removed when data enters the iHub- this means that e.g. "Regulation20"
# will be removed as well as "Regulation 20"
for (text in standardTexts) {
  text <- gsub(" ", "(\\\\s)*", text)
  standardTextsubbed <- c(standardTextsubbed, text)
}


dataset$FREE_TEXT <- removeStandardText(dataset$FREE_TEXT)


#### Columns ####

# text columns
columns <-
  c("FREE_TEXT",
    "ENQUIRY_SUMMARY",
    "ENQUIRY_DESC",
    "PRESSURE_SORE")


# text columns to be combined - test whether different column combinations work better than others
colList <-
  list(c(1), c(1, 2), c(1, 2, 3), c(1, 2, 3, 4), c(1, 3), c(1, 3, 4), c(1, 4))

length(colList)

# column name list- each name indicates which columns are included. Used to name the columns when they are added back to the df
colNameList <-
  c("FreeText",
    "FTEnqS",
    "FTEnqSEnqD",
    "FTEnqSEnqDPS",
    "FTEnqD",
    "FTEnqDPS",
    "FTPS")

length(colNameList)

# for loop to concatenate selected columns
for (i in 1:length(colList)) {
  colNames <- columns[colList[[i]]]
  
  print(colNames)
  
  # create a new dataset consisting of just the selected columns
  dataCol <- dataset[colNames]
  #check size
  print(dim(dataCol))
  
  # unite selected columns into one (paste did not work well but unite does)
  fC <- tidyr::unite(dataCol, fullCol, sep = " ")
  # check length
  print(nrow(fC))
  
  # put columns back into dataset under the appropriate name from the colNameList
  dataset[colNameList[i]] <- fC
  
  
}
# rename X as id_numeric for later
dataset$id_numeric <- dataset$X
# check
colnames(dataset)

# length of dataset
datalength <- nrow(dataset)


#### Prep for X Validation ####

#instantiate results df
resultsFrame <- data.frame()

# tell the model where your class data is (should be the name of the class column in dataset)
#class <- "classBinPU"

# set number of folds
#k <- 5

# these are the function defaults now

# set up results lists for saving results in the for loop
#actualClassList<- list()
#predictedClassList <- list()
# not needed now it is a function?

schemes <- c( "prop",  "logcount", "boolean", "logave","count")

priors <- c("uniform", "docfreq", "termfreq")



#### start for loop ####
set.seed(56)

tic()
# loop through weighting schemes (this goes first because of its tendency to fail)


# create 7 corpuses?
corpList <- list()

count <- 1
for (name in colNameList){
  corpus <- make_corpus(dataset = dataset, textField = name, k = 5)
  corpList[[count]] <- corpus
  count <- count + 1
  
}

#ugh

#make dataframe of the various variables you want to loop through
# may not be needed because of inability to use map
settingFrame <- expand.grid(setting = c(1:5), prior = priors, scheme = schemes)

# set up list for results
resultList <- list()

# TODO: test this horrible loop

# set dummies for testing?

count <- 1
#xvalidate <- function(corp, setting, scheme, prior, class = parent.frame()$class, k = k)
for (i in seq_along(corpList)){
  for (s in 1:5) { # settings
    for (sch in schemes){
      for (p in priors){
  val <- xvalidate(corpList[[i]], s, sch, p)
  res <- list(val, i, s, sch, p)  # I think this could be a data frame
  resultList[count] <- res # here is where we would append things to the dataframe, but note that using 
  # rbind in a loop is considered not a good thing to do
  count <- count + 1  
}
  }
  }
}
# pmap would not work with a corpus as the first argument as a corpus has length 4
#test <- purrr::pmap(list(corpList[[1]], settingFrame$setting, settingFrame$scheme, settingFrame$prior), xvalidate)

# pmap_dfr returns dataframe
# returns meanResults
# purrr pmap
# create dataframe of objects rep
# length 7*5*5*3
# named lists
# future pmap- can parallellise # might not work becasue of computer settings

# I don't think this bit will be needed but check
      # print results to screen
      print(meanResults)
      print(paste("setting: ", j ,"scheme: ", "prop", "prior: ", prior, "text field: ", textField))
      
      # make df of resutls
      res <- data.frame(meanResults, j,  prior, textField) # add s back in if it ever works
      resultsFrame <- rbind(resultsFrame, res)
      
      # append results to list (not sure how this will work out)
      resultsList <- append(resultsList, meanResults)
      
      
      
 
toc()


write.csv(resultsFrame, paste0(Sys.Date(), "XValidation_Results.csv"))

# TODO: add force = TRUE to createDfm if it works
# did not work
# TODO: sort out weighting if possible
# test out whether weighting works in cross-validation with a single dfm setting
