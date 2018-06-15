#Overlap Plot Function
ovlPlot <- function(animal1, animal2) {
  #' Plot the overlap between two circular data sets.
  #' 
  #' @param animal1 A numerical vector of measured points for the first species in RADIANS.
  #' @param animal2 A numerical vector of measured points for the second species in RADIANS.
  
  require("overlap")
  
  overlapPlot(
    animal1,
    animal2,
    main = paste0("Activity overlap between ", animalname1, " and ", animalname2),
    rug = TRUE
  )
  
  legend(
    "top",
    legend = c(animalname1, animalname2),
    col = c("black", "blue"),
    lty = c(1, 2)
  )
}