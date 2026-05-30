# READ IN MODELS ----------------------------------------------------------------

library(metafor)

#https://www.metafor-project.org/doku.php/tips:forest_plot_with_aggregated_values

during_H1_model<-readRDS('3_models/hypothesis_1/H1_model_during_totalresistancedeterminants.RDS')
during_H1_model_I2<-readRDS('3_models/hypothesis_1/H1_model_during_totalresistancedeterminants.RDS')
after_H1_model<-readRDS('3_models/hypothesis_1/H1_model_after_totalresistancedeterminants.RDS')
after_H1_model_I2<-readRDS('3_models/hypothesis_1/H1_model_after_totalresistancedeterminants.RDS')

# PROFILE LIKELIHOOD: DURING ------------------------------------

during_H1_profileplots<-metafor::profile.rma.mv(during_H1_model)

grDevices::tiff('4_figures/hypothesis_1/H1_profileplot_during_totalresistancedeterminants.tiff', res=300, units='in', width=8, height=8)

layout(matrix(1:4, ncol=2, byrow=TRUE))
plot(during_H1_profileplots)

dev.off()

# PROFILE LIKELIHOOD: AFTER ------------------------------------

after_H1_profileplots<-metafor::profile.rma.mv(after_H1_model)

grDevices::tiff('4_figures/hypothesis_1/H1_profileplot_after_totalresistancedeterminants.tiff', res=300, units='in', width=8, height=8)

layout(matrix(1:4, ncol=2, byrow=TRUE))
plot(after_H1_profileplots)

dev.off()
