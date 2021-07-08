#' Function to plot scores for each question
#'
#' Will help to visualize what factors are influencing index
#'
plot_q_score <- function(vuln_df){
  ggplot2::ggplot(vuln_df, aes(Code, score))+
    ggplot2::geom_col()+
    ggplot2::geom_point(aes(y = exp), fill = "red")+
    ggplot2::theme_classic()+
    ggplot2::ylim(0,NA)
}
