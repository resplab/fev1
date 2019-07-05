#' Plots FEV1 Projection
#' @param results results data frame from FEV1 Projection Model
#' @import ggplot2 plotly
#' @return patientData with prediction
#' @examples
#' patientData = samplePatients[1,]
#' results <- fev1_projection3(fev1_0 = patientData$fev1_0, 
#' int_effect = patientData$int_effect,
#' male = patientData$male, smoking = 1, age = patientData$age, 
#' weight = patientData$weight, height = patientData$height)
#' chart <- projectionChart(results)
#' chart
#' @export

projectionChart = function(results){

  color_b = "#4cdef5"
  color_a = "#330033"
  grey = '#E8E8E8'
  errorLineColor = color_b
  p <- ggplotly(ggplot(results, aes(Year, FEV1)) + geom_line(aes(y = FEV1), color=color_a, linetype=1) +
                geom_ribbon(aes(ymin=FEV1_lower, ymax=FEV1_upper), linetype=2, alpha=0.1, fill = color_b) +
                geom_line(aes(y = FEV1_lower), color=errorLineColor, linetype=2) +
                geom_line(aes(y = FEV1_upper), color=errorLineColor, linetype=2) +
                  theme(
                    panel.background = element_rect(fill = "white",
                                                    colour = "white",
                                                    size = 0.5, linetype = "solid"),
                    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                    colour = grey), 
                    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                    colour = grey))+
                theme(axis.title.x = element_text(color = color_a, size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
                theme(axis.title.y = element_text(color = color_a, size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
                annotate("text", 1.5, 3.52, label="Mean FEV1 decline", colour="black", size=4, hjust=0) +
                annotate("text", 1.75, 3.4, label="95% Coverage Interval", colour=errorLineColor, size=4, hjust=0, fontface = 'bold'))

  p$x$data[[1]]$text <- paste0("Time (years): ", results$Year, "<br />", "FEV1 (L): ", round(results$FEV1,3),
                             "<br />FEV1 lower (L): ", round(results$FEV1_lower,3), "<br />FEV1 upper (L): ",
                             round(results$FEV1_upper,3))

  p$x$data[[3]]$hoverinfo="none"
  p$x$data[[4]]$hoverinfo="none"
  p
}