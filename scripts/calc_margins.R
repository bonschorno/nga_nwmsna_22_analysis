calc_margins <- function(abs_frequency, total_cluster, estimate = "moe", sign_level = 0.08){
  
  raw_matrix <- Hmisc::binconf(x = abs_frequency, n = total_cluster, alpha = sign_level)
  
  if (estimate == "ci_low") {
    output <- round(raw_matrix[,2], digits = 3)
  } else if (estimate == "ci_high") {
    output <- round(raw_matrix[,3], digits = 3)
  } else {
    output <- round((raw_matrix[,3]-raw_matrix[,2])/2, digits = 3)
  } 
  
  return(output)
  
}