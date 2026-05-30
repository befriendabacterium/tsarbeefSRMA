# READ IN MODELS ----------------------------------------------------------------

library(metafor)

#https://www.metafor-project.org/doku.php/tips:forest_plot_with_aggregated_values

during_H2_model<-readRDS('3_models/hypothesis_2/H2_model_during_totalresistancedeterminants_timelinear.RDS')
after_H2_model<-readRDS('3_models/hypothesis_2/H2_model_after_totalresistancedeterminants_timelinear.RDS')

# PROFILE LIKELIHOOD: DURING ------------------------------------

during_H2_profileplots<-metafor::profile.rma.mv(during_H2_model)

grDevices::tiff('4_figures/hypothesis_2/H2_profileplot_during_totalresistancedeterminants.tiff', res=300, units='in', width=8, height=8)

layout(matrix(1:4, ncol=2, byrow=TRUE))
plot(during_H2_profileplots)

dev.off()

# PROFILE LIKELIHOOD: AFTER ------------------------------------

after_H2_profileplots<-metafor::profile.rma.mv(after_H2_model)

grDevices::tiff('4_figures/hypothesis_2/H2_profileplot_after_totalresistancedeterminants.tiff', res=300, units='in', width=8, height=8)

layout(matrix(1:4, ncol=2, byrow=TRUE))
plot(after_H2_profileplots)

dev.off()
