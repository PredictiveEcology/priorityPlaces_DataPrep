defineModule(sim, list(
  name = "priorityPlaces_DataPrep",
  description = paste0("This module has been designed to prepare data to create a raster of priority places for",
                       " conservation using spatial optimization"),
  keywords = c("priority places", "multispecies"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Alex", "Chubaty", email = "achubaty@for-cast.ca", role = "aut")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.9", pemisc = "0.0.2.9000",
                 priorityPlaces_DataPrep = "0.0.2"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "priorityPlaces_DataPrep.Rmd")),
  reqdPkgs = list("assertthat", "crayon", "data.table", "googledrive", "raster",
                  "PredictiveEcology/pemisc@development",
                  "tati-micheletti/usefulFuns", "vegan"),
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
                          "and time are not relevant")),
    defineParameter("diversityIndex", "character", "shannon", NA, NA,
                    paste("Can also use 'simpson' (which is the inverted version, i.e. more diversity, higher",
                          "value), but not both. The function diversityIndices can, however, deal with both of them.")),
    defineParameter("featureStreams", "numeric", 1:2, NA, NA,
                    paste("Only used for typeOfAnalysis == biodiversity.",
                          "Numeric vector of the streams that should composes the features",
                          "All others will compose the planningUnit as costs. Default to streams 1 and 2")),
    defineParameter("normalizeRasters", "logical", TRUE, NA, NA,
                    "Should the rasters of each stream be normalized?"),
    defineParameter("typeOfAnalysis", "character", "standard", NA, NA,
                    paste("Monetary cost analysis: 'standard' (default);",
                          "Biodiversity loss cost analysis: 'biodiversity'.")),
    defineParameter("weights", "data.table", NA, NA, NA,
                    paste("Only used for typeOfAnalysis == biodiversity.",
                          "Data.table with colunm 'stream' = c('stream3', 'stream4', 'stream5') and",
                          "'weight' of each stream that composes the cost",
                          "of the planningUnit. Default to NA, which ignores weights for all layers")),
    defineParameter("predictionYears", "numeric", c(2031, 2051, 2071), NA, NA,
                    paste0("Years for which this module should run. ",
                           "These years should match the data ",
                           "(streams) are available"))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "anthropogenicLayer_PP", objectClass = "RasterLayer",
                 desc = "Raster with road buffered disturbances. Can be replaced by a more complete layer",
                 sourceURL = NA),
    expectsInput(objectName = "birdPrediction", objectClass = "list",
                 desc = "List per year of the bird species predicted rasters", sourceURL = NA),
    expectsInput(objectName = "importantAreas", objectClass = "RasterLayer",
                 desc = paste0("Raster of areas that are of importance for one or more species, ",
                               "(i.e. coming from Indigenous knowldge)",
                               " planningUnit id correspond to penalize solutions that chose these",
                               "This will be filtered for non-na values (i.e. important are = 1,",
                               "non-important areas need to be 0"), sourceURL = NA),
    expectsInput(objectName = "planningUnit", objectClass = "RasterLayer",
                 desc = paste0("List of rasters (one for each time unit). Planning unit is the spatial area (study area) that should be",
                               " a raster."), sourceURL = NA),
    expectsInput(objectName = "predictedPresenceProbability", objectClass = "list",
                 desc =  paste0("List of rasters per year, indicating habitat quality ",
                                "index for presence of Caribous"), sourceURL = NA),
    expectsInput(objectName = "protectedAreas", objectClass = "RasterLayer | shapefile",
                 desc = paste0("Raster of protected areas, it will filter for non-na values",
                               " (i.e. all but protected areas need to be NA"), sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "featuresID", objectClass = "rasterStack",
                 desc = paste0("This is the rasterStack of the features to be ",
                               "assessed: caribouRSF, specific birds density, species richness, etc")),
    createsOutput(objectName = "importantAreas", objectClass = "RasterLayer",
                  desc = paste0("Raster of areas that are of importance for one or more species, ",
                                "(i.e. coming from Indigenous knowldge)",
                                " planningUnit id correspond to penalize solutions that chose these",
                                "This will be filtered for non-na values (i.e. important are = 1,",
                                "non-important areas need to be 0"),
                  sourceURL = NA),
    createsOutput(objectName = "latestYearsDiversity", objectClass = "list",
                  desc = paste0("List of the diversity rasters for each stream2:5")),
    createsOutput(objectName = "planningUnit", objectClass = "list",
                  desc = paste0("List of rasters (one for each time unit). Planning unit is the spatial area (study area) that should be",
                                " a raster.")),
    createsOutput(objectName = "protectedAreas", objectClass = "RasterLayer",
                 desc = paste0("Raster of protected areas, it will filter for non-na values (i.e. all but protected areas need",
                               "to be NA")),
    createsOutput(objectName = "speciesStreams", objectClass = "data.table",
                  desc = paste0("Table of species and the streams they belong to.",
                                "This table will allocate each species to its stream stack (bird diversity).",
                                " These bird streams + caribou (stream 1) will compose the featuresID")),
    createsOutput(objectName = "speciesStreamsList", objectClass = "list",
                  desc = paste0("List of the rasters list of stream, from stream2:5")),
    createsOutput(objectName = "stream1", objectClass = "list",
                  desc = paste0("List of species that belong to stream 1 -- higher priority conservation")),
    createsOutput(objectName = "stream2", objectClass = "list",
                  desc = paste0("List of species that belong to stream 2 -- medium-higher priority conservation")),
    createsOutput(objectName = "stream3", objectClass = "list",
                  desc = paste0("List of species that belong to stream 3 -- medium-lower priority conservation")),
    createsOutput(objectName = "stream4", objectClass = "list",
                  desc = paste0("List of species that belong to stream 4 -- lower priority conservation")),
    createsOutput(objectName = "stream5", objectClass = "list",
                  desc = paste0("List of species that belong to stream 5 -- all others (i.e. migratory birds)"))
  )
))

## event types
#   - type `init` is required for initialization

doEvent.priorityPlaces_DataPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # 1. Check that birdPrediction and predictedPresenceProbability stack.
      # If not, postProcess one of these ( [ FIX ] to be added later)
      # TODO
      tryCatch({
        stkBirs <- raster::stack(unlist(sim$birdPrediction))
        stkBoo <- stack(unlist(lapply(sim$predictedPresenceProbability, function(x) head(x, 1))))
        stk <- stack(stkBirs, stkBoo)

      }, error = function(e) {
        message(crayon::red("sim$birdPrediction and sim$predictedPresenceProbability do not align.",
                            "Will attempt postProcessing caribou layers using bird layers as RTM."))
        booPrediction <- lapply(names(sim$predictedPresenceProbability), function(year) {
          booPrediction <- lapply(names(sim$predictedPresenceProbability[[year]]), function(TYPE) {
            booPrediction <- Cache(postProcess, x = sim$predictedPresenceProbability[[year]][[TYPE]],
                                   destinationPath = dataPath(sim),
                                   rasterToMatch = sim$birdPrediction[[1]][[1]])
          })
          names(booPrediction) <- names(sim$predictedPresenceProbability[[year]])
          return(booPrediction)
        })
        names(booPrediction) <- names(sim$predictedPresenceProbability)
        sim$predictedPresenceProbability <- booPrediction
      })
      tryCatch({
        stkBirs <- raster::stack(unlist(sim$birdPrediction))
        stkBoo <- stack(unlist(lapply(sim$predictedPresenceProbability, function(x) head(x, 1))))
        stk <- stack(stkBirs, stkBoo)
      }, error = function(e){
        stop("postProcessing did not work for your layers. Please debug")
      })
      # 2. Get the importantAreas and protectedAreas.
      # If raster, postProcess if it doesn't stack with the other layers: predictedPresenceProbability
      if (!is.null(sim$importantAreas)){
        tryCatch({ # importantAreas
            stkBoo <- stack(unlist(lapply(sim$predictedPresenceProbability, function(x) head(x, 1))))
            stk <- stack(sim$importantAreas, stkBoo)
          }, error = function(e) {
            message("sim$importantAreas and sim$predictedPresenceProbability do not align.
                    Will try to postprocess sim$importantAreas.")
            tryCatch({
              importantAreas <- postProcess(x = sim$importantAreas, filename2 = NULL,
                                            rasterToMatch = sim$predictedPresenceProbability[[1]][[1]])
              sim$importantAreas <- importantAreas
            }, error = function(e) stop("PostProcessing was not able to align the rasters. Please debug."))
          }
        )
      }
      if (!is.null(sim$protectedAreas)) {
        tryCatch({ #protectedAreas
          stkBoo <- stack(unlist(lapply(sim$predictedPresenceProbability, function(x) head(x, 1))))
          stk <- stack(sim$protectedAreas, stkBoo)
        }, error = function(e) {
          message("sim$protectedAreas and sim$predictedPresenceProbability do not align.
                  Will try to postprocess sim$protectedAreas.")
          tryCatch({
            importantAreas <- postProcess(x = sim$protectedAreas, filename2 = NULL,
                                          rasterToMatch = sim$predictedPresenceProbability[[1]][[1]])
            sim$protectedAreas <- protectedAreas
          }, error = function(e) stop("PostProcessing was not able to align the rasters. Please debug."))
        })
      }

      # Create the list placeholders
      sim$speciesStreamsList <- sim$stream1 <- sim$stream2 <- sim$stream3 <-
        sim$stream4 <- sim$stream5 <- sim$featuresID <- list()

      # Assertion:
      if (length(P(sim)$diversityIndex) < 1)
        stop("You have to provide at least one index to be calculated:
             'shannon', 'simpson' (i.e. simpson is the inverted version)")

      # Create counter
      mod$.schedulingCounter <- 1

      # schedule future event(s)
      sim <- scheduleEvent(sim, dyears(P(sim)$predictionYears[mod$.schedulingCounter]),
                           "priorityPlaces_DataPrep", "assignStream", eventPriority = .first())
      sim <- scheduleEvent(sim, dyears(P(sim)$predictionYears[mod$.schedulingCounter]),
                           "priorityPlaces_DataPrep", "prepStreamStack", eventPriority = .first())
      sim <- scheduleEvent(sim, dyears(P(sim)$predictionYears[mod$.schedulingCounter]),
                           "priorityPlaces_DataPrep", "calculateStreamDiversity", eventPriority = .first())
      sim <- scheduleEvent(sim, dyears(P(sim)$predictionYears[mod$.schedulingCounter]),
                           "priorityPlaces_DataPrep", "addMissingStreams", eventPriority = .first())
      if (any(P(sim)$normalizeRasters, P(sim)$typeOfAnalysis == "biodiversity"))
        sim <- scheduleEvent(sim, dyears(P(sim)$predictionYears[mod$.schedulingCounter]),
                             "priorityPlaces_DataPrep", "normalizingFeatures", eventPriority = .first())
      if (any(!is.na(P(sim)$weights)))
        sim <- scheduleEvent(sim, dyears(P(sim)$predictionYears[mod$.schedulingCounter]),
                             "priorityPlaces_DataPrep", "updateFeatureWeights", eventPriority = .first())
      sim <- scheduleEvent(sim, dyears(P(sim)$predictionYears[mod$.schedulingCounter]),
                           "priorityPlaces_DataPrep", "updateScheduler", eventPriority = .last())
    },
    assignStream = {
      # 1. Get the names of the birdPrediction and allocate these into streams
      url <- "https://drive.google.com/file/d/17OiIWC5oJcP2Y0cXMJH_KUWmYJrhR-dn/view?usp=sharing"
      message("Loading species streams...")
      speciesWeWant <- Cache(prepInputs, url = url, filename2 = NULL,
                             destinationPath = dataPath(sim), fun = "readRDS") # ==> streams file
      speciesWeHave <- unique(unlist(lapply(sim$birdPrediction, names)))
      sim$speciesStreams <- speciesWeWant[SPEC %in% speciesWeHave, c("SPEC", "Management Stream")]
      lastStream <- data.table("SPEC" = setdiff(speciesWeHave, sim$speciesStreams[["SPEC"]]),
                               "Management Stream" = max(sim$speciesStreams[["Management Stream"]])+1)
      sim$speciesStreams <- rbind(sim$speciesStreams, lastStream)
    },
    prepStreamStack = {
      # 2. For the specific year, grab all the birds layers and assign them to a list of streams
      thisYearsBirds <- sim$birdPrediction[[paste0("Year", time(sim))]]
      birdSpecies <- names(thisYearsBirds)
      lapply(birdSpecies, function(BIRD){
        birdRas <- thisYearsBirds[[BIRD]]
        stream <- as.numeric(sim$speciesStreams[SPEC == BIRD, "Management Stream"])
        if (is.na(stream))
          stream <- max(sim$speciesStreams[["Management Stream"]], na.rm = TRUE) + 1
        names(birdRas) <- paste0(BIRD, "_", time(sim))
        if (stream == 1){
          birdRas <- list(birdRas)
          names(birdRas) <- BIRD
          sim$stream1[[paste0("Year", time(sim))]] <- c(sim$stream1[[paste0("Year", time(sim))]], birdRas)
        } else {
          if (stream == 2){
            birdRas <- list(birdRas)
            names(birdRas) <- BIRD
            sim$stream2[[paste0("Year", time(sim))]] <- c(sim$stream2[[paste0("Year", time(sim))]], birdRas)
          } else {
            if (stream == 3){
              birdRas <- list(birdRas)
              names(birdRas) <- BIRD
              sim$stream3[[paste0("Year", time(sim))]] <- c(sim$stream3[[paste0("Year", time(sim))]], birdRas)
            } else {
              if (stream == 4){
                birdRas <- list(birdRas)
                names(birdRas) <- BIRD
                sim$stream4[[paste0("Year", time(sim))]] <- c(sim$stream4[[paste0("Year", time(sim))]], birdRas)
              } else { # If the bird is not in the list, we put to stream5, migratory birds by default
                birdRas <- list(birdRas)
                names(birdRas) <- BIRD
                sim$stream5[[paste0("Year", time(sim))]] <- c(sim$stream5[[paste0("Year", time(sim))]], birdRas)
              }
            }
          }
        }
      })
      sim$speciesStreamsList[[paste0("Year", time(sim))]] <- list(
        stream1 = sim$stream1[[paste0("Year", time(sim))]],
        stream2 = sim$stream2[[paste0("Year", time(sim))]],
        stream3 = sim$stream3[[paste0("Year", time(sim))]],
        stream4 = sim$stream4[[paste0("Year", time(sim))]],
        stream5 = sim$stream5[[paste0("Year", time(sim))]]
      )
      sim$speciesStreamsList[[paste0("Year", time(sim))]] <- sim$speciesStreamsList[[paste0("Year", time(sim))]][
        lengths(sim$speciesStreamsList[[paste0("Year", time(sim))]]) != 0] # TO REMOVE THE EMPTY LISTS AFTERWARDS IF ANY

      # Schedule future events
      sim <- scheduleEvent(sim, dyears(P(sim)$predictionYears[mod$.schedulingCounter + 1]),
                           "priorityPlaces_DataPrep", "prepStreamStack")
    },
    calculateStreamDiversity = {
      # 2. For the specific year, calculate stream diversity
      sim$latestYearsDiversity <- lapply(names(sim$speciesStreamsList[[paste0("Year", time(sim))]]), function(stream) {
        thisYearIndices <- diversityIndices(birdStreamList = sim$speciesStreamsList[[paste0("Year", time(sim))]][[stream]],
                                            pathOutput = Paths[["outputPath"]],
                                            currentTime = time(sim), stream = stream)
        # TODO Potentially replace this function with the SpadeR::Diversity().
        return(thisYearIndices)
      })
      names(sim$latestYearsDiversity) <- names(sim$speciesStreamsList[[paste0("Year", time(sim))]])
      # Schedule future events
      sim <- scheduleEvent(sim, dyears(P(sim)$predictionYears[mod$.schedulingCounter + 1]),
                           "priorityPlaces_DataPrep", "calculateStreamDiversity")
    },
    addMissingStreams = {
      # 3. For the specific year, add the missing stream 1 (i.e. caribou)
      thisYearCaribou <- sim$predictedPresenceProbability[[paste0("Year", time(sim))]]
      caribouRSFuncertain <- grepMulti(names(thisYearCaribou), patterns = "Uncertain")
      caribouRSFname <- setdiff(names(thisYearCaribou), caribouRSFuncertain)
      # TODO: Test with adding caribou to a stream that has one bird already
      # First, add stream1
      nms <- names(sim$stream1[[paste0("Year", time(sim))]])
      sim$stream1[[paste0("Year", time(sim))]] <- c(sim$stream1[[paste0("Year", time(sim))]],
                                                    thisYearCaribou[[caribouRSFname]])
      names(sim$stream1[[paste0("Year", time(sim))]]) <- c(nms, "stream1")
      sim$latestYearsDiversity <- c(sim$stream1[[paste0("Year", time(sim))]], sim$latestYearsDiversity)
      # Then check for missing streams
      missingStreams <- setdiff(paste0("stream", 1:5), names(sim$latestYearsDiversity))
      if (!is.null(missingStreams)) {
        missingRas <- lapply(missingStreams, function(mssStr) {
          zeroedRas <- raster::setValues(sim$stream1[[paste0("Year", time(sim))]][[1]], 0)
          message("Making zeroed layer, Cached object is fine here")
          ras <- Cache(postProcess, x = zeroedRas, # It is zeroed so it doesn't add anything to the features, but can be passed
                       rasterToMatch =  sim$stream1[[paste0("Year", time(sim))]][[1]],
                       maskWithRTM = TRUE, filename2 = NULL,
                       userTags = c("module:priorityPlaces_DataPrep",
                                    "zeroedStreams",
                                    paste0("missingStream:", mssStr)),
                       omitArgs = "useCache") # Caribou is used as template here
          names(ras) <- mssStr
          return(ras)
        })
        names(missingRas) <- missingStreams
      }
      # Here I expect to have all stream layers, from 1 to 5.
      # If there is one I don't have originally, it should be here as zero
      nm <- c(names(sim$latestYearsDiversity), names(missingRas))
      stk <- raster::stack(c(unlist(sim$latestYearsDiversity), missingRas))
      names(stk) <- nm
      matched <- match(paste0("stream", 1:5), names(stk))
      if (P(sim)$typeOfAnalysis == "standard") {
        sim$featuresID[[paste0("Year", time(sim))]] <- raster::subset(stk, matched)
      } else {
        if (P(sim)$typeOfAnalysis == "biodiversity") {
          matched <- paste0("stream", P(sim)$featureStreams)
          streamsCost <- setdiff(names(stk), matched)
          if (any(!is.na(P(sim)$weights))){
            testthat::expect_true(NROW(P(sim)$weights) == length(streamsCost),
                                  label = paste0("Please provide weights for all layers.",
                                                 " Currently, weights are being provided for ",
                                                 paste(P(sim)$weights[["stream"]], collapse = ", "),
                                                 " but the following",
                                                 " layers are expected: ", paste(streamsCost, collapse = ", "),
                                                 ". Matching weights and stream layers"))
          }
          sim$featuresID[[paste0("Year", time(sim))]] <- raster::subset(stk, matched)
          sim$planningUnit[[paste0("Year", time(sim))]] <- raster::subset(stk, streamsCost)
        } else {
          stop("Currenty only 'standard' or 'biodiversity' are accepted as 'typeOfAnalysis'")
        }
      }

      # Schedule future events
      sim <- scheduleEvent(sim, dyears(P(sim)$predictionYears[mod$.schedulingCounter + 1]),
                           "priorityPlaces_DataPrep", "addMissingStreams")
    },
    normalizingFeatures = {
      # 4. Normalizing rasters
      if (P(sim)$normalizeRasters){
        rs <- raster::stack(sim$featuresID[[paste0("Year", time(sim))]])
        sim$featuresID[[paste0("Year", time(sim))]] <- normalizeStackTM(rs)
      }
      if (P(sim)$typeOfAnalysis == "biodiversity") {
        # 1. Normalize cost layers so I can apply the weight
        normalized <- normalizeStackTM(raster::stack(sim$planningUnit[[paste0("Year", time(sim))]]))
        # 2. Apply the weight and sum all
        if (any(!is.na(P(sim)$weights))){
          if (is(P(sim)$weights, "data.table")) {
            weights <- P(sim)$weights
            normalized <- raster::stack(lapply(weights[, stream], function(st) {
              normWeighted <- normalized[[st]] * weights[stream == st, weight]
              return(normWeighted)
            }))
          } else {
           stop("Weight needs to be provided as a data.table")
          }
        }
        normalized <- raster::calc(normalized, fun = sum)
        # 3. Normalize again
        normalized <- normalizeStack(normalized)
        names(normalized) <- paste0("Year", time(sim))
        # 4. Subtract from 1: Here we assume that the cost increases with decresed
        # species diversity (the higher the diversity, the lower the cost to conservation)
        sim$planningUnit[[paste0("Year", time(sim))]] <- 1 - normalized[[paste0("Year", time(sim))]]
        # Remove from planningUnit the anthropogenic disturbance
        if (is(sim$anthropogenicLayer_PP, "RasterLayer")){
          # assertion
          assertthat::assert_that(is(raster::stack(sim$anthropogenicLayer_PP,
                                                   sim$planningUnit[[paste0("Year", time(sim))]]), "RasterStack"),
                                  msg = "planningUnit and anthropogenicLayer do not align. Please debug")
          sim$planningUnit[[paste0("Year", time(sim))]][!is.na(sim$anthropogenicLayer_PP[])] <- NA
        }
      }

      # Schedule future events
      sim <- scheduleEvent(sim, dyears(P(sim)$predictionYears[mod$.schedulingCounter + 1]),
                           "priorityPlaces_DataPrep", "normalizingFeatures")
    },
    updateFeatureWeights = {
        if (is(P(sim)$weights, "data.table")) {
          weights <- P(sim)$weights
          normalized <- raster::stack(lapply(weights[, stream], function(st) {
            weightedStream <- sim$featuresID[[paste0("Year", time(sim))]][[st]] * weights[stream == st, weight]
            return(weightedStream)
          }))
        } else {
          stop("Weight needs to be provided as a data.table")
        }
        addUnchangingStream <- setdiff(names(sim$featuresID[[paste0("Year", time(sim))]]), names(normalized))
        sim$featuresID[[paste0("Year", time(sim))]] <- raster::stack(sim$featuresID[[paste0("Year", time(sim))]][[addUnchangingStream]],
                                                                     normalized)
        # Schedule future events
      sim <- scheduleEvent(sim, dyears(P(sim)$predictionYears[mod$.schedulingCounter + 1]),
                           "priorityPlaces_DataPrep", "updateFeatureWeights")
    },
    updateScheduler = {
      # First you schedule the event as the previous ones
      sim <- scheduleEvent(sim, dyears(P(sim)$predictionYears[mod$.schedulingCounter + 1]),
                          "priorityPlaces_DataPrep", "updateScheduler")
      # Then you update the counter
      mod$.schedulingCounter <- mod$.schedulingCounter +1
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  if (!suppliedElsewhere("birdPrediction", sim)) {
    message(crayon::red("No bird layers provided. Using DUMMY data"))
    sim$birdPrediction <- list(
      Year2001 = list(
        BBWA = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                          url = "https://drive.google.com/open?id=1X8O89Yem6WjcwCTPvj9OJowXLCsJMAAO"),
        CAWA = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                          url = "https://drive.google.com/open?id=18jjCr8bKN6ftX8M_EdrRA5y8RB8KPuZI"),
        BLBW = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                          url = "https://drive.google.com/open?id=13ElhFF1q5NvdQvVb3in8jdP5mWTJLXNq"),
        CCSP = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                          url = "https://drive.google.com/open?id=1Is7SvDVka-dq6KBzs_YMM81wOl0pfvhA"),
        BCCH = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                          url = "https://drive.google.com/open?id=1MVzhzEDh0UAYD3_94ZlkDUQkgDOKk1yL"),
        AMCR = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                          url = "https://drive.google.com/open?id=13YZ-gKXtDf3w6900BR4f2jxndcUOj3w9")
      ),
      Year2100 = list(
        BBWA = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                          url = "https://drive.google.com/open?id=1WcfPMD7j_-Nfad7koz-rfm3S0-IP2Jsr"),
        BLBW = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                          url = "https://drive.google.com/open?id=1tBjED-qKtqPFVWAcLKW2_GLF6SSWbhsQ"),
        CAWA = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                          url = "https://drive.google.com/open?id=1OGGroI187s5yP17G91qZ2zLEhQe9YSTe"),
        CCSP = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                          url = "https://drive.google.com/open?id=1VQOBUHARR6HWGMHshJ-9iQ9l2tsVEa4I"),
        AMCR = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                          url = "https://drive.google.com/open?id=1-RSMtl5HhejBGO1RrTFRAp-wNjYP_BpI"),
        BCCH = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                          url = "https://drive.google.com/open?id=1fPBkC99KYI9vxOVIDUiceerIYxFusfhY")
      )
    )
}
    if (!suppliedElsewhere("predictedPresenceProbability", sim)){
      message(crayon::red("No caribou layers provided. Using DUMMY data"))
      sim$predictedPresenceProbability <- list(
        Year2001 = list(
          rasterOfAverage = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                                       url ="https://drive.google.com/open?id=1Dhk7fYHysrnb6kXCA4KlcFp73ohAtMJ2"),
          rasterOfUncertain = prepInputs(destinationPath = Paths$inputPath,filename2 = NULL,
                                         url ="https://drive.google.com/open?id=1NZ9T2DwrbKn_bvDmiBnZ1mowBR-lqJZk")
        ),
        Year2100 = list(
          rasterOfAverage = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                                       url ="https://drive.google.com/open?id=1ONJM8ITMP6A9_WTU2K2yIWZ0VFJWXTyg"),
          rasterOfUncertain = prepInputs(destinationPath = Paths$inputPath, filename2 = NULL,
                                         url ="https://drive.google.com/open?id=1Y_Ij-I44sq3QvjTLukEDKlUuH6HDXgGx")
        )
      )
    }

    if (!suppliedElsewhere("planningUnit", sim)) {
      if (P(sim)$typeOfAnalysis == "standard"){
        message(crayon::red("No planningUnit layer provided.",
                            "Basing the planning unit on the caribou layer",
                            "(the whole are, excl. water bodies)"))
        booBasedPU <- sim$predictedPresenceProbability[[1]][[1]]
        booBasedPU[!is.na(booBasedPU)] <- 0
        sim$planningUnit <- list(booBasedPU)
        names(sim$planningUnit) <- paste0("Year", start(sim))
        names(sim$planningUnit[[paste0("Year", start(sim))]]) <- "planningUnit"
      } else {
        if (P(sim)$typeOfAnalysis == "biodiversity"){
          sim$planningUnit <- list()
        } else {
          stop("Currenty only 'standard' or 'biodiversity' are accepted as 'typeOfAnalysis'")        }
      }
    }

  if (!suppliedElsewhere("anthropogenicLayer_PP", sim)){
    sim$anthropogenicLayer_PP <- NA
  }

  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  return(invisible(sim))
}
