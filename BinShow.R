library(ggplot2)
library(SpatialBall)
data("season2017")
data("court")


S2017 <- dplyr::filter(season2017, LOC_Y <= 449)
S2017$EVENT_TYPE <- ifelse(S2017$EVENT_TYPE == "Made Shot", 1, 0)
S2017$SHOT_TYPE <- ifelse(S2017$SHOT_TYPE == "3PT Field Goal", 3, 2)

S2017$Points <- S2017$EVENT_TYPE*S2017$SHOT_TYPE 




                                                                                          
library(ggplot2)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


p1<-  ggplot(S2017, aes(LOC_X, LOC_Y, z = Points))+ annotation_custom(court, -250, 250, -52, 418) +
  stat_summary_hex(bins = 2,
                   fun = function(z) {
                     mean(z)
                   }, alpha = 0.8) + scale_fill_gradient2(name = "PPS", midpoint = 1, low = "blue", high = "red", limits=c(0, 2)) + theme_void() + coord_fixed(xlim = c(-250, 250), ylim = c(-51, 270)) + theme(legend.position="bottom")   + guides(fill=guide_legend(title.position="top", 
                                                                                                                                                                                                                                                                         title.hjust =0.5, label.position = "bottom"))




mylegend<-g_legend(p1)

HEX4<-  ggplot(S2017, aes(LOC_X, LOC_Y, z = Points))+ annotation_custom(court, -250, 250, -52, 418) +
  stat_summary_hex(bins = 4,
                   fun = function(z) {
                     mean(z)
                   }, alpha = 0.8) + scale_fill_gradient2(midpoint = 1, low = "blue", high = "red", limits=c(0, 2)) + theme_void() + coord_fixed(xlim = c(-250, 250), ylim = c(-51, 270)) + theme(legend.position="none") + ggtitle("Bin = 4")  + theme(plot.title = element_text(hjust = 0.5))



HEX8<-  ggplot(S2017, aes(LOC_X, LOC_Y, z = Points))+ annotation_custom(court, -250, 250, -52, 418) +
  stat_summary_hex(bins = 8,
                   fun = function(z) {
                     mean(z)
                   }, alpha = 0.8) + scale_fill_gradient2(midpoint = 1, low = "blue", high = "red", limits=c(0, 2)) + theme_void() + coord_fixed(xlim = c(-250, 250), ylim = c(-51, 270)) + theme(legend.position="none") + ggtitle("Bin = 8")  + theme(plot.title = element_text(hjust = 0.5))

HEX26<-  ggplot(S2017, aes(LOC_X, LOC_Y, z = Points))+ annotation_custom(court, -250, 250, -52, 418) +
  stat_summary_hex(bins = 26,
                   fun = function(z) {
                     mean(z)
                   }, alpha = 0.8) + scale_fill_gradient2(midpoint = 1, low = "blue", high = "red", limits=c(0, 2)) + theme_void() + coord_fixed(xlim = c(-250, 250), ylim = c(-51, 270)) + theme(legend.position="none") + ggtitle("Bin = 26")  + theme(plot.title = element_text(hjust = 0.5)) 


HEX1<-  ggplot(S2017, aes(LOC_X, LOC_Y, z = Points))+ annotation_custom(court, -250, 250, -52, 418) +
  stat_summary_hex(bins = 1,
                   fun = function(z) {
                     mean(z)
                   }, alpha = 0.8) + scale_fill_gradient2(midpoint = 1, low = "blue", high = "red", limits=c(0, 2)) + theme_void() + coord_fixed(xlim = c(-250, 250), ylim = c(-51, 270)) + theme(legend.position="none") + ggtitle("Bin = 1")  + theme(plot.title = element_text(hjust = 0.5))


library(gridExtra)

grid.arrange(HEX4, HEX8, HEX26, HEX1, mylegend,  layout_matrix=rbind(c(1,1,2,2), c(1,1,2,2) ,c(3,3,4,4), c(3,3,4,4), c(5,5,5,5)))
 