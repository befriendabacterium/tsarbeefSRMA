if(exists("pico_processed")){
  ylowermax<-floor(min(c(pico_processed$yi_totalresistancedeterminants,pico_processed$yi_totaldeterminants), na.rm=T))
  yuppermax<-ceiling(max(c(pico_processed$yi_totalresistancedeterminants,pico_processed$yi_totaldeterminants), na.rm=T))
}

if(exists("pico_witheffectsizes")){
  ylowermax<-floor(min(c(pico_witheffectsizes$yi_totalresistancedeterminants,pico_witheffectsizes$yi_totaldeterminants), na.rm=T))
  yuppermax<-ceiling(max(c(pico_witheffectsizes$yi_totalresistancedeterminants,pico_witheffectsizes$yi_totaldeterminants), na.rm=T))
}

ylowermaj=-3
yuppermaj=3

# y_limits<-c(ylower,yupper)
# y_breaks<-seq(ylower,yupper,1)

y_limits<-c(ylowermaj,yuppermaj)
y_breaks<-seq(ylowermaj,yuppermaj,1)

theme_plots <- function(){
  
  font <- "Georgia"   #assign font family up front
  
  theme_bw() %+replace% 
  
    theme(
    
    axis.text.x = ggplot2::element_text(size = 10, colour ="black",
                                        hjust = 0.5,
                                        vjust = 0,
                                        angle = 0),
    axis.text.y = ggplot2::element_text(size = 10, colour ="black",
                                        hjust = 0,
                                        vjust = 0.5,
                                        angle = 0),
    axis.title.x = ggplot2::element_text(size = 10, colour ="black",
                                         hjust = 0.5,
                                         vjust = 0,
                                         angle = 0,
                                         margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(size = 10, colour ="black",
                                         hjust = 0.5,
                                         vjust = 0.5,
                                         angle=90,
                                         margin = margin(t = 0, r = 15, b = 0, l = 0)),
    legend.position = 'none'
    #legend.position = 'top.right'
    # legend.position = c(1, 0), 
    # legend.justification = c(1, 0),
    # legend.direction = "horizontal",
    # legend.background = ggplot2::element_blank(),
    # 
    )
}
