#' Elsa.else Function
#'
#' 用于计算分组后除自身外的其他观测值的汇总指标：例如求和、平均值、分位数等 git版本尝试改变
#' @author Yu Li_XMU 
#' @param df_you 数据集
#' @param id_name 须生成id的列
#' @param group_name 是分组的变量,若需要group的有多个组,如city-year组，只需给新的组合city-year打上id即可
#' @param fun_name 待处理的列
#' @param fun_type 设定了6种，1.sum，3.mean，3.median，4.sd，5.quantile，并可使用prob参数；6.prod
#' @keywords df
#' @export
#' @examples
#' Elsa(df_you = df2,
#' id_name =行业,
#' group_name = newid,
#' fun_name =行业j就业人数,
#' fun_type = 1)
#' 
Elsa.else <- function(df_you, id_name, group_name,  fun_name, fun_type = 1, prob = 0.25) {
  if (!is.data.frame(df_you)) {
    stop("\"df_you\" 必须是数据框\n", "你提供的数据集是 ",
         class(df_you)[1])
  }
  library(tidyverse)
  df_you <- as.tibble(df_you)
  group_name <- enquo(group_name) 
  id_name <- enquo(id_name)
  fun_name <- enquo(fun_name)
  df_you <- df_you %>% 
    group_by(!!group_name) %>% 
    mutate(id=1:n(),id2=1:n()) %>%
    ungroup()
  x <- df_you %>%
    group_by(!!group_name) %>%
    nest()
  b <- array(0, dim = c(nrow(df_you), nrow(df_you), nrow(x)))  
  for (j in 1:nrow(x)) {
    for (i in x[[2]][[j]]$id) {
      for (value in x[[2]][[j]]$id2) {
        if (value != i) {
          b[, , j][i, value] = unlist(select(x[[2]][[j]], !!fun_name))[[value]]^k
        }
      }
    }
  }
  g <<- b
  g[which(g == 0)] = NA
  c = list()
  if (fun_type == 1) {
    for (k in 1:dim(g)[3]) {
      c[k] <- apply(g[, , k][apply(g[, , k], 1, function(x) !all(is.na(x))),], 1, sum, na.rm = TRUE) %>%
        as.tibble()
    }
    d <<- unlist(c) %>%
      as.tibble() %>%
      bind_cols(df_you, .) %>%
      rename(Else.sum = value) %>%
      select(-id2)
    view(d)
  } else if (fun_type == 2) {
    for (k in 1:dim(g)[3]) {
      c[k] <- apply(g[, , k][apply(g[, , k], 1, function(x) !all(is.na(x))),], 1, mean, na.rm = TRUE) %>%
        as.tibble()
    }
    d <<- unlist(c) %>%
      as.tibble() %>%
      bind_cols(df_you, .) %>%
      rename(Else.mean = value) %>%
      select(-id2)
    view(d)
  } else if (fun_type == 3) {
    for (k in 1:dim(g)[3]) {
      c[k] <- apply(g[, , k][apply(g[, , k], 1, function(x) !all(is.na(x))),], 1, median, na.rm = TRUE) %>%
        as.tibble()
    }
    d <<- unlist(c) %>%
      as.tibble() %>%
      bind_cols(df_you, .) %>%
      rename(Else.median = value) %>%
      select(-id2)
    view(d)
  } else if (fun_type == 4) {
    for (k in 1:dim(g)[3]) {
      c[k] <- apply(g[, , k][apply(g[, , k], 1, function(x) !all(is.na(x))),], 1, sd, na.rm = TRUE) %>%
        as.tibble()
    }
    d <<- unlist(c) %>%
      as.tibble() %>%
      bind_cols(df_you, .) %>%
      rename(Else.sd = value) %>%
      select(-id2)
    view(d)
  } else if (fun_type == 5) {
    for (k in 1:dim(g)[3]) {
      c[k] <- apply(g[, , k][apply(g[, , k], 1, function(x) !all(is.na(x))),], 1, quantile, na.rm = TRUE, probs = prob) %>%
        as.tibble()
    }
    d <<- unlist(c) %>%
      as.tibble() %>%
      bind_cols(df_you, .) %>%
      rename(Else.sd = value) %>%
      select(-id2)
    view(d)
  } else if (fun_type == 6) {
    for (k in 1:dim(g)[3]) {
      c[k] <- apply(g[, , k][apply(g[, , k], 1, function(x) !all(is.na(x))),], 1, prod, na.rm = TRUE, probs = prob) %>%
        as.tibble()
    }
    d <<- unlist(c) %>%
      as.tibble() %>%
      bind_cols(df_you, .) %>%
      rename(Else.prod = value) %>%
      select(-id2)
    view(d)
  }
}




