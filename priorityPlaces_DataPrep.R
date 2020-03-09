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
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter("stepInterval", "numeric", 10, NA, NA,
                    "Interval between predictions. Normally, 20 for birds and caribou. Both birds and caribou NEED to match!"),
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
                               " character name for each feature.")),
    createsOutput(objectName = "protectedAreas", objectClass = "RasterLayer", 
                 desc = paste0("Raster of protected areas, it will filter for non-na values (i.e. all but protected areas need",
                               "to be NA")),
    createsOutput(objectName = "importantAreas", objectClass = "RasterLayer", 
                 desc = paste0("Raster of areas that are of importance for one or more species, ",
                               "(i.e. coming from Indigenous knowldge)",
                               " planningUnit id correspond to penalize solutions that chose these",
                               "This will be filtered for non-na values (i.e. important are = 1,",
                               "non-important areas need to be 0"),
                 sourceURL = NA),
    createsOutput(objectName = "speciesStreams", objectClass = "data.table", 
                  desc = paste0("Table of species and the streams they belong to.", 
                                "This table will allocate each species to its stream stack (bird diversity).",
                                " These bird streams + caribou (stream 1) will compose the featuresID")),
    createsOutput(objectName = "stream1", objectClass = "list", 
                  desc = paste0("List of species that belong to stream 1 -- higher priority conservation")),
    createsOutput(objectName = "stream2", objectClass = "list", 
                  desc = paste0("List of species that belong to stream 2 -- medium-higher priority conservation")),
    createsOutput(objectName = "stream3", objectClass = "list", 
                  desc = paste0("List of species that belong to stream 3 -- medium-lower priority conservation")),
    createsOutput(objectName = "stream4", objectClass = "list", 
                  desc = paste0("List of species that belong to stream 4 -- lower priority conservation")),
    createsOutput(objectName = "speciesStreamsList", objectClass = "list", 
                  desc = paste0("List of the rasters list of stream, from stream1:4"))
  )
))

## event types
#   - type `init` is required for initialization

doEvent.priorityPlaces_DataPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # 1. Check that birdPrediction and predictedPresenceProbability stack. If not, postProcess one of these ( [ FIX ] to be added later) 
      # TODO 
      tryCatch({
        stkBirs <- raster::stack(unlist(sim$birdPrediction))
        stkBoo <- stack(unlist(lapply(sim$predictedPresenceProbability, function(x) head(x, 1))))
        stk <- stack(stkBirs, stkBoo)
        
      }, error = function(e)
        {
        browser()
        stop("sim$birdPrediction and sim$predictedPresenceProbability do not align. Please debug.")
      })
      
      # 2. Get the importantAreas and protectedAreas. 
      # If raster, postProcess if it doesn't stack with the other layers: predictedPresenceProbability
      tryCatch({ # importantAreas
        stkBoo <- stack(unlist(lapply(sim$predictedPresenceProbability, function(x) head(x, 1))))
        stk <- stack(stkBirs[[1]], stkBoo)
      }, error = function(e)
        {
        message("sim$importantAreas and sim$predictedPresenceProbability do not align. 
                Will try to postprocess sim$importantAreas.")
        tryCatch({
          importantAreas <- postProcess(x = sim$importantAreas, 
                                        rasterToMatch = sim$predictedPresenceProbability[[1]][[1]])
          sim$importantAreas <- importantAreas
        }, error = function(e) stop("PostProcessing was not able to align the rasters. Please debug."))
      })
      
      tryCatch({ #protectedAreas
        stkBoo <- stack(unlist(lapply(sim$predictedPresenceProbability, function(x) head(x, 1))))
        stk <- stack(stkBirs[[1]], stkBoo)
      }, error = function(e)
        {
        message("sim$protectedAreas and sim$predictedPresenceProbability do not align. 
                Will try to postprocess sim$protectedAreas.")
        tryCatch({
          importantAreas <- postProcess(x = sim$protectedAreas,
                                        rasterToMatch = sim$predictedPresenceProbability[[1]][[1]])
          sim$protectedAreas <- protectedAreas
        }, error = function(e) stop("PostProcessing was not able to align the rasters. Please debug."))
      })
      
      sim$speciesStreamsList <- sim$stream1 <- sim$stream2 <- sim$stream3 <- sim$stream4 <- list()
    
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "priorityPlaces_DataPrep", "assignStream")
      sim <- scheduleEvent(sim, time(sim), "priorityPlaces_DataPrep", "prepreStreamStack")
      sim <- scheduleEvent(sim, time(sim), "priorityPlaces_DataPrep", "calculateStreamDiversity")
    },
    assignStream = {

      # 1. Get the names of the birdPrediction and allocate these into streams
      speciesWeWant <- Cache(prepInputs, url = "https://drive.google.com/file/d/17OiIWC5oJcP2Y0cXMJH_KUWmYJrhR-dn/view?usp=sharing",
                             destinationPath = dataPath(sim), fun = "readRDS") # ==> streams file
      speciesWeHaveAll <- Cache(drive_ls, as_id("1DD2lfSsVEOfHoob3fKaTvqOjwVG0ZByQ"), recursive = FALSE)
      speciesWeHave <- usefun::substrBoth(grepMulti(speciesWeHaveAll$name, patterns = "brt6.R"), howManyCharacters = 4, fromEnd = FALSE)
      
      speciesWeWant_Vec <- speciesWeWant$SPEC
      commonSp <- speciesWeWant_Vec[speciesWeWant_Vec %in% speciesWeHave]
      sim$speciesStreams <- speciesWeWant[SPEC %in% commonSp, c("SPEC", "Management Stream")] 
      
    },
    prepreStreamStack = {
      
      # 2. For the specific year, grab all the birds layers and assign them to a list of streams
      thisYearsBirds <- sim$birdPrediction[[paste0("Year", time(sim))]]
      birdSpecies <- names(thisYearsBirds)
      lapply(birdSpecies, function(BIRD){
        birdRas <- thisYearsBirds[[BIRD]]
        stream <- as.numeric(sim$speciesStreams[SPEC == BIRD, "Management Stream"])
        names(birdRas) <- paste0(BIRD, "_", time(sim))
        if (stream == 1){
          sim$stream1[[BIRD]] <- birdRas 
        } else {
          if (stream == 2){
            sim$stream2[[BIRD]] <- birdRas            
          } else {
            if (stream == 3){
              sim$stream3[[BIRD]] <- birdRas
            } else {
              if (stream == 4){
                sim$stream4[[BIRD]] <- birdRas
              }
            }
          }
        }
      })
      browser()
      
      # names(birdPrediction[[time(sim)]])[!names(birdPrediction[[time(sim)]])) %in% names(thisYearsBirds)]
      
      # ADD STREAM 5: all other migratory birds we have models for
      # 
      sim$speciesStreamsList[[paste0("Year", time(sim))]] <- list(stream1 = sim$stream1, stream2 = sim$stream2, 
                                                            stream3 = sim$stream3, stream4 = sim$stream4)
      sim$speciesStreamsList[[paste0("Year", time(sim))]] <- sim$speciesStreamsList[[paste0("Year", time(sim))]][lengths(sim$speciesStreamsList[[paste0("Year", time(sim))]]) != 0] # TO REMOVE THE EMPTY LISTS AFTERWARDS IF ANY
      
      # Schedule future events
      sim <- scheduleEvent(sim, time(sim) + P(sim)$stepInterval, "priorityPlaces_DataPrep", "prepreStreamStack")
    },
    calculateStreamDiversity = {
      browser()
      # 2. For the specific year, calculate stream diversity
      thisYearsDiversity <- lapply(names(sim$speciesStreamsList[[paste0("Year", time(sim))]]), function(stream){
        thisYearIndices <- diversityIndices(birdStreamList = sim$speciesStreamsList[[paste0("Year", time(sim))]][[stream]], 
                                            pathOutput = dataPath(sim), currentTime = time(sim), stream = stream)
      })

      # Schedule future events
      sim <- scheduleEvent(sim, time(sim) + P(sim)$stepInterval, "priorityPlaces_DataPrep", "calculateStreamDiversity")
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
