#' @export
ucCor <- function(a, b){
  tab <- table(factor(a),factor(b))
  mu <- tab/sum(tab)
  mu_suma <- colSums(mu)
  mu_sumb <- rowSums(mu)
  -sum(mu * (log(mu) - log(outer(mu_sumb, mu_suma))))/sum(mu_sumb * log(mu_sumb))
}

#' @export
cramerV <- function(a, b){
  tab <- table(factor(a),factor(b))
  unname(sqrt((chisq.test(tab))$statistic/(sum(tab) * ( min( (nlevels(factor(a)) - 1), (nlevels(factor(b)) - 1))))))
}