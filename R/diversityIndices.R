diversityIndices <- function(index = "shannon", # can also do simpson (i.e. inverted simpson)
                                 birdStreamList,
                                 pathOutput,
                                 stream,
                                 currentTime = time(sim),
                                 overwriteDiversityIndices = FALSE){

  if (any(is.null(index), is.na(index)))
    stop("You have to provide at least one index to be calculated: 'shannon', 'simpson' (i.e. simpson is the inverted version)")
  haveDiversityRasters <- lapply(paste0(index, "Raster"), function(rasName){
    fileExists <- file.exists(file.path(pathOutput, paste0(rasName, "_", stream, "_", currentTime, ".tif")))
  })
  if (overwriteDiversityIndices || !all(unlist(haveDiversityRasters))){
    message(crayon::yellow("Not all diversity rasters exist or overwriteDiversityIndices is TRUE. Creating diversity rasters."))
    stk <- raster::stack(birdStreamList)
    # cellSizeHA <- prod(res(stk))/10000
    bird.abun <- lapply(X = birdStreamList, function(eachRas){
      if (is(eachRas, "character"))
        eachRas <- raster(eachRas)
      vect <- raster::getValues(x = eachRas)
      return(vect)
    })
    bird.abun <- data.table::data.table(do.call(cbind, bird.abun))
    bird.abun[, Sum := rowSums(bird.abun, na.rm = TRUE)]
    cols <- names(birdStreamList)
    p <- bird.abun[, lapply(.SD, function(sp){sp/Sum}), .SDcols = cols]
    p$pixelID <- 1:NROW(p)
    p2 <- na.omit(p, cols = names(p)[names(p) != "pixelID"])
    p2_index <- p2$pixelID
    p2[, pixelID := NULL]
    if ("shannon" %in% index){
      # Shannon Diversity 
      Shannon <- apply(X = p2, MARGIN = 1, index = "shannon", FUN = vegan::diversity)
      shannonTable <- data.table(shannonIndex = Shannon, pixelID = p2_index)
      shannonTableReady <- merge(shannonTable, p[, "pixelID"], by = "pixelID", all.y = TRUE)
      # Put the estimates back to rasters
      shannonRaster <- setValues(x = raster(birdStreamList[[1]]), values = shannonTableReady$shannonIndex)
      names(shannonRaster) <- "shannonRaster"
    } else shannonRaster <- NULL
    if ("simpson" %in% index){
      # Inverted Simpson Diversity
      SimpsonInv <- apply(X = p2, MARGIN = 1, index = "invsimpson", FUN = vegan::diversity)
      simpsonTable <- data.table(invSimpsonIndex = SimpsonInv, pixelID = p2_index)
      simpsonTableReady <- merge(simpsonTable, p[, "pixelID"], by = "pixelID", all.y = TRUE)
      simpsonRaster <- setValues(x = raster(birdStreamList[[1]]), values = simpsonTableReady$invSimpsonIndex)
      names(simpsonRaster) <- "simpsonRaster"
    } else simpsonRaster <- NULL
    currentDiversityRasters <- stack(shannonRaster, simpsonRaster)
    diversityRasters <- lapply(names(currentDiversityRasters), function(rasName){
      writeRaster(x = currentDiversityRasters[[rasName]], 
                  filename = file.path(pathOutput, paste0(rasName, "_", stream, "_", currentTime)),
                  overwrite = TRUE,
                  format = "GTiff")
      ras <- raster::raster(file.path(pathOutput, paste0(rasName, "_", stream, "_", currentTime, ".tif")))
      return(ras)
    })
    rastersNames <- unlist(lapply(diversityRasters, names))
    names(diversityRasters) <- rastersNames
    return(diversityRasters)
  } else {
    message(crayon::green(paste0("All diversity rasters exist for ", stream,
                                 " year ", currentTime,
                                 " and overwriteDiversityIndices is FALSE. ",
                                 "Returning existing diversity rasters.")))
    diversityRasters <- lapply(paste0(index, "Raster"), function(rasName){
      ras <- raster::raster(file.path(pathOutput, paste0(rasName, "_", stream, "_", currentTime, ".tif")))
      return(ras)
    })
    rastersNames <- unlist(lapply(diversityRasters, names))
    names(diversityRasters) <- rastersNames
    if (length(diversityRasters) == 1){ # Mechanism in place for only one index (i.e. what the module needs!), 
                                        # but the function can deal with more indices (i.e. 'invSimpson')
      diversityRasters <- diversityRasters[[1]]
    }
    return(diversityRasters)
  }
}
