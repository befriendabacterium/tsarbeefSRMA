calculate_varbarlength<-function(pico_df){
  
   #total determinantslog10: absolute difference between mean and error bar limit (up or down)
  pico_df$outcome_plot_totaldeterminantslog10_intervention_var<-abs(pico_df$outcome_plot_totaldeterminantslog10_intervention_mean-pico_df$outcome_plot_totaldeterminantslog10_intervention_errorbarlimit)
  pico_df$outcome_plot_totaldeterminantslog10_control_var<-abs(pico_df$outcome_plot_totaldeterminantslog10_control_mean-pico_df$outcome_plot_totaldeterminantslog10_control_errorbarlimit)
  
  #total resistance determinantslog10: absolute difference between mean and error bar limit (up or down)
  pico_df$outcome_plot_totalresistancedeterminantslog10_intervention_var<-abs(pico_df$outcome_plot_totalresistancedeterminantslog10_intervention_mean-pico_df$outcome_plot_totalresistancedeterminantslog10_intervention_errorbarlimit)
  pico_df$outcome_plot_totalresistancedeterminantslog10_control_var<-abs(pico_df$outcome_plot_totalresistancedeterminantslog10_control_mean-pico_df$outcome_plot_totalresistancedeterminantslog10_control_errorbarlimit)
  
  #proportion: absolute difference between mean and error bar limit (up or down)
  pico_df$outcome_plot_proportionresistancedeterminants_intervention_var<-abs(pico_df$outcome_plot_proportionresistancedeterminants_intervention_mean-pico_df$outcome_plot_proportionresistancedeterminants_intervention_errorbarlimit)
  pico_df$outcome_plot_proportionresistancedeterminants_control_var<-abs(pico_df$outcome_plot_proportionresistancedeterminants_control_mean-pico_df$outcome_plot_proportionresistancedeterminants_control_errorbarlimit)
 
  return(pico_df)
   
}

