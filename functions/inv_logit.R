#load function to calculate the inverse logit to back-transform logits
inv_logit <- function(f,adjust=0.025) {
  adjust <- (1-2*adjust)
  (adjust*(1+exp(f))+(exp(f)-1))/(2*adjust*(1+exp(f)))
}