#' Function to plot scores for each question
#'
#' Will help to visualize what factors are influencing index
#'
plot_q_score <- function(vuln_df){
  ggplot(vuln_df, aes(Code, score))+
    geom_col()+
    geom_point(aes(y = exp), fill = "red")+
    theme_bw()+
    ylim(0,NA)
}
