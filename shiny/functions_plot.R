font.family <- list(family = "Lato")

plot.format.nums <- function(format = c("percent", "nominal")) {
  if (format == "percent") {
    yscale <- scales::percent
  } else if (format == "nominal") {
    yscale <- scales::comma
  }
  return(yscale)
}

xtab.plot.bar <- function(table, format = c("percent", "nominal"), xlabel, ylabel, dttype.label) {
  yscale <- plot.format.nums(format)
  
  g <- ggplot(table, 
              aes(x = value,
                  y = result,
                  group = get(colnames(table)[1]),
                  fill = get(colnames(table)[1]),
                  text = paste(paste0(xlabel,':'), group,
                               paste0('<br>', ylabel, ':'), value,
                               paste0('<br>', dttype.label, ':'), yscale(result))
              )
  ) +
    geom_col(position = position_dodge(preserve = "single")) +
    theme_minimal() +
    labs(fill = str_wrap(xlabel, 25),
         x = ylabel,
         y = NULL) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_y_continuous(labels = yscale) +
    theme(axis.title.x = element_text(margin = margin(t=30)),
          axis.title.y = element_text(margin = margin(r=20)),
          legend.title=element_text(size=10),
          plot.margin = margin(.6, 4.5, 0, 0, "cm"))
  
  p <- ggplotly(g, tooltip = "text") %>% layout(font = font.family)
}

xtab.plot.bar.pivot <- function(table, format = c("percent", "nominal"), xlabel, ylabel, dttype.label) {
  yscale <- plot.format.nums(format)
  
  g <- ggplot(table, 
              aes(x = value,
                  y = result,
                  group = get(colnames(table)[1]),
                  fill = get(colnames(table)[1]),
                  text = paste(paste0(xlabel,':'), group,
                               paste0('<br>', ylabel, ':'), value,
                               paste0('<br>', dttype.label, ':'), yscale(result))
              )
  ) +
    geom_col(position = position_dodge(preserve = "single")) +
    theme_minimal() +
    labs(fill = xlabel,
         x = ylabel,
         y = NULL) +
    scale_y_continuous(labels = yscale) +
    theme(axis.title.x = element_text(margin = margin(t=30)),
          axis.title.y = element_text(margin = margin(r=20))) +
    coord_flip()
  
  p <- ggplotly(g, tooltip = "text") %>% layout(font = font.family)
}

# xtab.plot.bar.moe <- function(table, format = c("percent", "nominal"), xlabel, ylabel) {
#   yscale <- plot.format.nums(format)
#   
#   g <- ggplot(table, 
#               aes(x = value,
#                   y = result,
#                   group = get(colnames(table)[1]),
#                   fill = get(colnames(table)[1]),
#                   text = paste(paste0(xlabel,':'), group,
#                                paste0('<br>', ylabel, ':'), value,
#                                paste0('<br>', 'Share (min, max):'), 
#                                yscale(result), paste0("(", yscale(pmax(result-result_moe, 0)), ", ", yscale(pmin(result+result_moe, 1)), ")"))
#               )
#   ) +
#     geom_col(position = position_dodge(preserve = "single")) + 
#     geom_errorbar(aes(ymin = pmax(result - result_moe, 0), ymax = pmin(result + result_moe, 1)),
#                   alpha = .5,
#                   width = 0.2,
#                   position = position_dodge(width =  .9)) +
#     theme_minimal() +
#     labs(fill = str_wrap(xlabel, 25),
#          x = ylabel,
#          y = NULL) +
#     scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
#     scale_y_continuous(labels = yscale) +
#     theme(axis.title.x = element_text(margin = margin(t=30)),
#           axis.title.y = element_text(margin = margin(r=20)),
#           legend.title=element_text(size=10),
#           plot.margin = margin(.6, 4.5, 0, 0, "cm"))
#   
#   p <- ggplotly(g, tooltip = "text") %>% layout(font = font.family)
# }

xtab.plot.bar.moe <- function(table, format = c("percent", "nominal"), xlabel, ylabel) {
  yscale <- plot.format.nums(format)
  
  if (format == "percent") {
    f <- ggplot(table, 
                aes(x = value,
                    y = result,
                    group = get(colnames(table)[1]),
                    fill = get(colnames(table)[1]),
                    text = paste(paste0(xlabel,':'), group,
                                 paste0('<br>', ylabel, ':'), value,
                                 paste0('<br>', 'Share (min, max):'), 
                                 yscale(result), paste0("(", yscale(pmax(result-result_moe, 0)), ", ", yscale(pmin(result+result_moe, 1)), ")"))
                )
    ) +
      geom_col(position = position_dodge(preserve = "single")) + 
      geom_errorbar(aes(ymin = pmax(result - result_moe, 0), ymax = pmin(result + result_moe, 1)),
                    alpha = .5,
                    width = 0.2,
                    position = position_dodge(width =  .9))
  } else {
    f <- ggplot(table, 
                aes(x = value,
                    y = result,
                    group = get(colnames(table)[1]),
                    fill = get(colnames(table)[1]),
                    text = paste(paste0(xlabel,':'), group,
                                 paste0('<br>', ylabel, ':'), value,
                                 paste0('<br>', 'Estimate (min, max):'), 
                                 yscale(result), paste0("(", yscale(result-result_moe), ", ", yscale(result+result_moe), ")"))
                )
    ) +
      geom_col(position = position_dodge(preserve = "single")) + 
      geom_errorbar(aes(ymin = result - result_moe, ymax = result + result_moe),
                    alpha = .5,
                    width = 0.2,
                    position = position_dodge(width =  .9))
  }
  
  g <- f +
    theme_minimal() +
    labs(fill = str_wrap(xlabel, 25),
         x = ylabel,
         y = NULL) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_y_continuous(labels = yscale) +
    theme(axis.title.x = element_text(margin = margin(t=30)),
          axis.title.y = element_text(margin = margin(r=20)),
          legend.title=element_text(size=10),
          plot.margin = margin(.6, 4.5, 0, 0, "cm"))
  
  p <- ggplotly(g, tooltip = "text") %>% layout(font = font.family)
}

xtab.plot.bar.moe.pivot <- function(table, format = c("percent", "nominal"), xlabel, ylabel) {
  yscale <- plot.format.nums(format)
  
  g <- ggplot(table, 
              aes(x = value,
                  y = result,
                  group = get(colnames(table)[1]),
                  fill = get(colnames(table)[1]),
                  text = paste(paste0(xlabel,':'), group,
                               paste0('<br>', ylabel, ':'), value,
                               yscale(result), paste0("(", yscale(pmax(result-result_moe, 0)), ", ", yscale(pmin(result+result_moe, 1)), ")"))
                  # paste0('<br>', dttype.label, ':'), yscale(result))
              )
  ) +
    geom_col(position = position_dodge(preserve = "single")) + 
    geom_linerange(aes(ymin = pmax(result - result_moe, 0), ymax = pmin(result + result_moe, 1)),
                   alpha = .4,
                   size = .02,
                   position = position_dodge(width = .9)) +
    theme_minimal() +
    labs(fill = str_wrap(xlabel, 30),
         x = ylabel,
         y = NULL) +
    # scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_y_continuous(labels = yscale) +
    theme(axis.title.x = element_text(margin = margin(t=30)),
          axis.title.y = element_text(margin = margin(r=20)),
          plot.margin = margin(.4, 0, 0, 0, "cm")) +
    coord_flip()
  
  p <- ggplotly(g, tooltip = "text") %>% layout(font = font.family)
}

stab.plot.bar <- function(table, format = c("percent", "nominal"), xlabel) {
  yscale <- plot.format.nums(format)
  
  g <- ggplot(table,
              aes(x = value,
                  y = result,
                  fill = get(colnames(table)[1]),
                  text = paste(paste0(xlabel,':'), value,
                               paste0('<br>', type, ':'), yscale(result))
              )
  ) +
    geom_col(position = position_dodge(preserve = "single")) +
    theme_minimal() +
    labs(fill = NULL,
         x = xlabel,
         y = NULL) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_y_continuous(labels = yscale) +
    theme(axis.title.x = element_text(margin = margin(t=30)),
          axis.title.y = element_text(margin = margin(r=20)),
          legend.position = 'none')
  
  p <- ggplotly(g, tooltip = "text") %>% layout(font = font.family)
}

stab.plot.bar.moe <- function(table, format = c("percent", "nominal"), xlabel) {
  yscale <- plot.format.nums(format)
  
  g <- ggplot(table,
              aes(x = value,
                  y = result,
                  fill = get(colnames(table)[1]),
                  text = paste(paste0(xlabel,':'), value,
                               paste0('<br>', 'Share (min, max):'), 
                               yscale(result), paste0("(", yscale(pmax(result-result_moe, 0)), ", ", yscale(pmin(result+result_moe, 1)), ")"))
              )
  ) +
    geom_col(position = position_dodge(preserve = "single")) +
    geom_errorbar(aes(ymin = pmax(result - result_moe, 0), ymax = pmin(result + result_moe, 1)),
                  alpha = .5,
                  width = 0.2,
                  position = position_dodge(width =  .9)) +
    theme_minimal() +
    labs(fill = NULL,
         x = xlabel,
         y = NULL) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_y_continuous(labels = yscale) +
    theme(axis.title.x = element_text(margin = margin(t=30)),
          axis.title.y = element_text(margin = margin(r=20)),
          legend.position = 'none')
  
  p <- ggplotly(g, tooltip = "text") %>% layout(font = font.family)
}

stab.plot.bar2 <- function(table, format = c("percent", "nominal"), xlabel) {
  yscale <- plot.format.nums(format)
  
  g <- ggplot(table, 
              aes(x = value, 
                  y = result, 
                  fill = get(colnames(table)[1]),
                  text = paste(paste0(xlabel,':'), value,
                               paste0('<br>', type, ':'), yscale(result))
              )
  ) +
    geom_col(position = position_dodge(preserve = "single")) +
    theme_minimal() +
    labs(fill = NULL,
         x = xlabel,
         y = NULL) +
    scale_y_continuous(labels = yscale) +
    theme(axis.title.x = element_text(margin = margin(t=30)),
          axis.text.x = element_blank(),
          axis.title.y = element_text(margin = margin(r=20))#,
    )
  
  p <- ggplotly(g, tooltip = "text") %>% layout(font = font.family)
}

stab.plot.bar2.moe <- function(table, format = c("percent", "nominal"), xlabel) {
  yscale <- plot.format.nums(format)
  
  g <- ggplot(table, 
              aes(x = value, 
                  y = result, 
                  fill = get(colnames(table)[1]),
                  text = paste(paste0(xlabel,':'), value,
                               paste0('<br>', 'Share (min, max):'), 
                               yscale(result), paste0("(", yscale(pmax(result-result_moe, 0)), ", ", yscale(pmin(result+result_moe, 1)), ")"))
              )
  ) +
    geom_col(position = position_dodge(preserve = "single")) +
    geom_errorbar(aes(ymin = pmax(result - result_moe, 0), ymax = pmin(result + result_moe, 1)),
                  alpha = .5,
                  width = 0.2,
                  position = position_dodge(width =  .9)) +
    theme_minimal() +
    labs(fill = NULL,
         x = xlabel,
         y = NULL) +
    scale_y_continuous(labels = yscale) +
    theme(axis.title.x = element_text(margin = margin(t=30)),
          axis.text.x = element_blank(),
          axis.title.y = element_text(margin = margin(r=20))#,
    )
  
  p <- ggplotly(g, tooltip = "text") %>% layout(font = font.family)
}