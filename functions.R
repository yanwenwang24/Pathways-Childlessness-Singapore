# Cross tab
ctab <- function (df, col1, col2) {
  df <- df %>% 
    tabyl({{col1}}, {{col2}}) %>% 
    adorn_percentages("col") %>% 
    adorn_pct_formatting(digits = 2) %>%
    adorn_ns() %>% 
    as.data.frame()
  df[1] <- paste(deparse(substitute(col1)), df[[1]], sep="")
  df %>% 
    rename(variable = {{col1}})
}

ctab_row <- function (df, col1, col2) {
  df <- df %>% 
    tabyl({{col1}}, {{col2}}) %>% 
    adorn_percentages("row") %>% 
    adorn_pct_formatting(digits = 2) %>%
    adorn_ns() %>% 
    as.data.frame()
  df[1] <- paste(deparse(substitute(col1)), df[[1]], sep="")
  df %>% 
    rename(variable = {{col1}})
}


# t_test between those with and without children
t_test <- function (df) {
  p_list <- c()
  for (i in 2:ncol(df)) {
    p_value <- t.test(filter(df, n_childless == 1)[i],
                      filter(df, n_childless == 0)[i])$p.value
    p_list <- c(p_list, p_value)
    i <-  i + 1
  }
  return(p_list)
}

# Obtain robust standard error
se_robust <- function(x) {
  coeftest(x, vcovHC(x, 
                     type = "HC3")
  )[, "Std. Error"]
}

# Concatenate all columns based on prefix
c_col <- function(data, prefix) {
  data %>%
    mutate("{prefix}" := paste(get(paste0(prefix, ".x")), get(paste0(prefix, ".y")), sep = " ")) %>%
    select(-matches(paste0(prefix, "\\.[xy]")))
}

# Obtain Entropy
entropy <- function(x) {
  p <- prop.table(table(x)) 
  -sum(p * log2(p), na.rm = TRUE)
}

# Obtain Entropy R2 from LCA
machine_tolerance <- sqrt(.Machine$double.eps)
entropy.R2 <- function(fit) {
  entropy <- function(p) {
    p <- p[p > machine_tolerance] # since Lim_{p->0} p log(p) = 0
    sum(-p * log(p))
  }
  error_prior <- entropy(fit$P) # Class proportions
  error_post <- mean(apply(fit$posterior, 1, entropy))
  R2_entropy <- (error_prior - error_post) / error_prior
  R2_entropy
}

# Obtain correlation matrix with stars

corstars <- function(x, 
                     method = c("pearson", "spearman"), 
                     removeTriangle = c("upper", "lower"),
                     result = c("none", "html", "latex")) {
  # Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix <- rcorr(x, type = method[1])
  R <- correlation_matrix$r # Matrix of correlation coefficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***",
                    ifelse(p < .01, "** ",
                           ifelse(p < .05, "*  ", "   ")))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[, -1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep = "")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1] == "upper") {
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1] == "lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew) - 1])
  if (result[1] == "none") return(Rnew)
  else {
    if(result[1] == "html") print(xtable(Rnew), type = "html")
    else print(xtable(Rnew), type = "latex") 
  }
} 
