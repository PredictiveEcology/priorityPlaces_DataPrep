diversityIndices <- function(index = c("shannon","simpson","richness"),
                                 birdStreamList,
                                 pathOutput,
                                 stream,
                                 currentTime = time(sim),
                                 overwriteDiversityIndices = FALSE){

  haveDiversityRasters <- lapply(c("shannonRaster","simpsonRaster","richnessRaster"), function(rasName){
    fileExists <- file.exists(file.path(pathOutput, paste0(rasName, "_", stream, "_", currentTime, ".tif")))
  })
  if (overwriteDiversityIndices || !all(unlist(haveDiversityRasters))){
    message(crayon::yellow("Not all diversity rasters exist or overwriteDiversityIndices is TRUE. Creating diversity rasters."))
    stk <- raster::stack(birdStreamList)
    cellSizeHA <- prod(res(stk))/10000
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
    Shannon <- apply(X = p2, MARGIN = 1, index = "shannon", FUN = vegan::diversity)
    shannonTable <- data.table(shannonIndex = Shannon, pixelID = p2_index)
    shannonTableReady <- merge(shannonTable, p[, "pixelID"], by = "pixelID", all.y = TRUE)
    browser()
    SimpsonInv <- apply(X = p2, MARGIN = 1, index = "invsimpson", FUN = vegan::diversity)
    
    sim$currentDiversityRasters <- stack(shannonRaster,simpsonRaster,richnessRaster)
    names(sim$currentDiversityRasters) <- c("shannonRaster","simpsonRaster","richnessRaster")
    diversityRasters <- lapply(names(sim$currentDiversityRasters), function(rasName){
      browser() #paste0(rasName, "_", stream, "_", currentTime ?
      writeRaster(x = sim$currentDiversityRasters[[rasName]], 
                  filename = file.path(pathOutput, paste0(rasName, "_", currentTime)),
                  overwrite = TRUE,
                  format = "GTiff")
      ras <- raster::raster(file.path(pathOutput, paste0(rasName, "_", currentTime, ".tif")))
      return(ras)
    }) 
    return(diversityRasters)
  } else {
    message(crayon::green("All diversity rasters exist and overwriteDiversityIndices is FALSE. Returning existing diversity rasters."))
    return() 
  }
}
