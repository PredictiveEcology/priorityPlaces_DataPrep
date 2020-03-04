## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "priorityPlaces_DataPrep",
  description = paste0("This module has been designed to prepare data to create a raster of priority places for",
                       " conservation using spatial optimization"),
  keywords = c("priority places", "multispecies"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Alex", "Chubaty", email = "achubaty@for-cast.ca", role = "aut")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.9", priorityPlaces_DataPrep = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "priorityPlaces_DataPrep.Rmd")),
  reqdPkgs = list("googledrive", "tati-micheletti/usefun"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "birdPrediction", objectClass = "list", 
                 desc = "List per year of the bird species predicted rasters", sourceURL = NA),
    expectsInput(objectName = "predictedPresenceProbability", objectClass = "list", 
                 desc = "List of rasters per year, indicating the probability of presence of Caribous", sourceURL = NA),
    expectsInput(objectName = "protectedAreas", objectClass = "RasterLayer | shapefile", 
                  desc = paste0("Raster of protected areas, it will filter for non-na values (i.e. all but protected areas need",
                                "to be NA"), sourceURL = NA),
    expectsInput(objectName = "importantAreas", objectClass = "RasterLayer | shapefile", 
                 desc = paste0("Raster of areas that are of importance for one or more species, ",
                               "(i.e. coming from Indigenous knowldge)",
                               " planningUnit id correspond to penalize solutions that chose these",
                               "This will be filtered for non-na values (i.e. important are = 1,",
                               "non-important areas need to be 0"), sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "planningUnit", objectClass = "RasterLayer | data.frame", 
                 desc = paste0("Planning unit is the spatial area (study area) that should be", 
                               "either a raster or data.frame. If the last, calculations",
                               " are faster. If the last, each row in the planning unit table must",
                               " correspond to a different planning unit. The table must also have ",
                               " an 'id' column to provide a unique integer identifier for each",
                               " planning unit (i.e. pixelID -- used as `pu` in featuresData. see below),",
                               " and it must also have columns wit xloc, yloc, and one that",
                               " indicates the cost of each planning unit ('cost'). If the first, the module", 
                               " will convert it to data.frame with the necessary adjustments")),
    createsOutput(objectName = "featuresID", objectClass = "rasterStack | data.frame", 
                 desc = paste0("This is the rasterStack or relative data.frame of the features to be ",
                               "assessed: caribouRSF, specific birds density, species richness, etc",
                               "If a data.frame, feature data must have an 'id' column containing ", 
                               "a unique identifier (i.e. matching 'species' in featuresData), and `name`", 
                               " character name for each feature."),
    createsOutput(objectName = "protectedAreas", objectClass = "RasterLayer", 
                 desc = paste0("Raster of protected areas, it will filter for non-na values (i.e. all but protected areas need",
                               "to be NA"),
    createsOutput(objectName = "importantAreas", objectClass = "RasterLayer", 
                 desc = paste0("Raster of areas that are of importance for one or more species, ",
                               "(i.e. coming from Indigenous knowldge)",
                               " planningUnit id correspond to penalize solutions that chose these",
                               "This will be filtered for non-na values (i.e. important are = 1,",
                               "non-important areas need to be 0"),
                 sourceURL = NA)
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.priorityPlaces_DataPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # 1. Check that birdPrediction and predictedPresenceProbability stack. If not, postProcess one of these
      
      # 2. Get the importantAreas and protectedAreas. If shapefile, convert to raster using predictedPresenceProbability. 
      # If raster, postProcess if it doesn't stack with the other layers: predictedPresenceProbability

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "priorityPlaces_DataPrep", "assignStream")
    },
    assignStream = {

      # 1. Get the names of the birdPrediction and allocate these into streams
      "https://drive.google.com/file/d/17OiIWC5oJcP2Y0cXMJH_KUWmYJrhR-dn/view?usp=sharing" # ==> streams file
      speciesWeHaveAll <- drive_ls(as_id(GDriveFolder), recursive = FALSE)
      speciesWeHave <- usefun::substrBoth(grepMulti(speciesWeHaveAll$name, patterns = "brt6.R"), howManyCharacters = 4, fromEnd = FALSE)

    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot(sim$object)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
