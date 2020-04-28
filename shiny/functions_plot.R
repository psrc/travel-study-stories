source.string <- 'Combined 2017/19 Household Travel Survey'
font.family <- list(family = "Lato")

plot.format.nums <- function(format = c("percent", "nominal")) {
  if (format == "percent") {
    yscale <- scales::percent
  } else if (format == "nominal") {
    yscale <- scales::comma
  }
  return(yscale)
}

plot.layout <- function(df, dttype.label, xlabel, ylabel = NULL) {
  if (!is.null(ylabel)) {
    main.title <- paste0(paste(dttype.label, "of", '<i>',ylabel,'</i>', "by", '<i>',xlabel,'</i>'))
  } else {
    main.title <- paste0(paste(dttype.label, "of", '<i>', xlabel, '</i>'))
  }
 
  df %>% layout(font = font.family,
               title = list(text = main.title,
               x=.025,
               font=list(size=14)
               ), # end title list
               margin = list(l = 0, r = 0, t = 60, b = 100),
               annotations = list(x = 1, y = -0.35, text = paste("Source:", source.string), 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=9))
        ) # end layout
}

# Two-way Table functions -------------------------------------------------


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
         y = NULL
         ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_y_continuous(labels = yscale) +
    theme(axis.title.x = element_text(margin = margin(t=30)),
          axis.title.y = element_text(margin = margin(r=20)),
          legend.title=element_text(size=10),
          plot.margin = margin(.6, 4.5, 0, 0, "cm")
          )
  
  p <- ggplotly(g, tooltip = "text") %>% plot.layout(dttype.label, xlabel, ylabel)
    # layout(font = font.family,
    #        title = list(text = paste0(paste(dttype.label, "of", ylabel, "by", xlabel),
    #                                   '<br>',
    #                                   '<sup>',
    #                                   'Source: Combined 2017/19 Household Travel Survey',
    #                                   '</sup>'
    #                                   ),
    #                     x=.025,
    #                     font=list(size=14)
    #                     ) # end title list
    #        ) # end layout
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
  
  p <- ggplotly(g, tooltip = "text") %>% plot.layout(dttype.label, xlabel, ylabel)
  # layout(font = font.family)
}

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
    title.text <- "Share"
  } else {
    f <- ggplot(table, 
                aes(x = value,
                    y = result,
                    group = get(colnames(table)[1]),
                    fill = get(colnames(table)[1]),
                    text = paste(paste0(xlabel,':'), group,
                                 paste0('<br>', ylabel, ':'), value,
                                 paste0('<br>', 'Total (min, max):'), 
                                 yscale(result), paste0("(", yscale(result-result_moe), ", ", yscale(result+result_moe), ")"))
                )
    ) +
      geom_col(position = position_dodge(preserve = "single")) + 
      geom_errorbar(aes(ymin = result - result_moe, ymax = result + result_moe),
                    alpha = .5,
                    width = 0.2,
                    position = position_dodge(width =  .9))
    title.text <- "Total"
  }
  dttype.label <- paste(title.text, "with Margin or Error")
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
  
  p <- ggplotly(g, tooltip = "text") %>% plot.layout(dttype.label, xlabel, ylabel)#layout(font = font.family)
}

xtab.plot.bar.moe.pivot <- function(table, format = c("percent", "nominal"), xlabel, ylabel) {
  yscale <- plot.format.nums(format)
  
  if (format == "percent") {
    f <- ggplot(table, 
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
                     position = position_dodge(width = .9))
    title.text <- "Share"
  } else {
    f <- ggplot(table, 
                aes(x = value,
                    y = result,
                    group = get(colnames(table)[1]),
                    fill = get(colnames(table)[1]),
                    text = paste(paste0(xlabel,':'), group,
                                 paste0('<br>', ylabel, ':'), value,
                                 yscale(result), paste0("(", yscale(result-result_moe), ", ", yscale(result+result_moe), ")"))
                    # paste0('<br>', dttype.label, ':'), yscale(result))
                )
    ) +
      geom_col(position = position_dodge(preserve = "single")) + 
      geom_linerange(aes(ymin = result - result_moe, ymax = result + result_moe),
                     alpha = .4,
                     size = .02,
                     position = position_dodge(width = .9))
    title.text <- "Total"
  }
  dttype.label <- paste(title.text, "with Margin or Error")
  g <- f +
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
  
  p <- ggplotly(g, tooltip = "text") %>% plot.layout(dttype.label, xlabel, ylabel)#layout(font = font.family)
}

# One-way Table functions -------------------------------------------------


stab.plot.bar <- function(table, format = c("percent", "nominal"), xlabel) {
  yscale <- plot.format.nums(format)
  dttype.label <- unique(table$type)
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
  
  p <- ggplotly(g, tooltip = "text") %>% plot.layout(dttype.label, xlabel)#layout(font = font.family)
}

stab.plot.bar.moe <- function(table, format = c("percent", "nominal"), xlabel) {
  yscale <- plot.format.nums(format)
  
  if (format == "percent") {
    f <- ggplot(table,
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
                    position = position_dodge(width =  .9)) 
    title.text <- "Share"
  } else {
    f <- ggplot(table,
                aes(x = value,
                    y = result,
                    fill = get(colnames(table)[1]),
                    text = paste(paste0(xlabel,':'), value,
                                 paste0('<br>', 'Total (min, max):'), 
                                 yscale(result), paste0("(", yscale(result-result_moe), ", ", yscale(result+result_moe), ")"))
                )
    ) +
      geom_col(position = position_dodge(preserve = "single")) +
      geom_errorbar(aes(ymin = result - result_moe, ymax = result + result_moe),
                    alpha = .5,
                    width = 0.2,
                    position = position_dodge(width =  .9)) 
    title.text <- "Total"
  }
  dttype.label <- paste(title.text, "with Margin or Error")
  g <- f +
    theme_minimal() +
    labs(fill = NULL,
         x = xlabel,
         y = NULL) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_y_continuous(labels = yscale) +
    theme(axis.title.x = element_text(margin = margin(t=30)),
          axis.title.y = element_text(margin = margin(r=20)),
          legend.position = 'none')
  
  p <- ggplotly(g, tooltip = "text") %>% plot.layout(dttype.label, xlabel)#layout(font = font.family)
}

stab.plot.bar2 <- function(table, format = c("percent", "nominal"), xlabel) {
  yscale <- plot.format.nums(format)
  dttype.label <- unique(table$type)
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
  
  p <- ggplotly(g, tooltip = "text") %>% plot.layout(dttype.label, xlabel)#layout(font = font.family)
}


stab.plot.bar2.moe <- function(table, format = c("percent", "nominal"), xlabel) {
  yscale <- plot.format.nums(format)
  
  if (format == "percent") {
    f <- ggplot(table, 
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
                    position = position_dodge(width =  .9))
    title.text <- "Share"
  } else {
    f <- ggplot(table, 
                aes(x = value, 
                    y = result, 
                    fill = get(colnames(table)[1]),
                    text = paste(paste0(xlabel,':'), value,
                                 paste0('<br>', 'Total (min, max):'), 
                                 yscale(result), paste0("(", yscale(result-result_moe), ", ", yscale(result+result_moe), ")"))
                )
    ) +
      geom_col(position = position_dodge(preserve = "single")) +
      geom_errorbar(aes(ymin = result - result_moe, ymax = result + result_moe),
                    alpha = .5,
                    width = 0.2,
                    position = position_dodge(width =  .9))
    title.text <- "Total"
  }
  dttype.label <- paste(title.text, "with Margin or Error")
  g <- f +
    theme_minimal() +
    labs(fill = NULL,
         x = xlabel,
         y = NULL) +
    scale_y_continuous(labels = yscale) +
    theme(axis.title.x = element_text(margin = margin(t=30)),
          axis.text.x = element_blank(),
          axis.title.y = element_text(margin = margin(r=20))#,
    )
  
  p <- ggplotly(g, tooltip = "text") %>% plot.layout(dttype.label, xlabel)#layout(font = font.family)
}

# Two-way Facts Table functions -------------------------------------------


xtab.plot.bar.fact <- function(table, format = c("percent", "nominal"), xlabel, ylabel, dttype.label) {
  yscale <- plot.format.nums(format)
  g <- ggplot(table,
              aes(x = group,
                  y = result,
                  fill = get(colnames(table)[1]),
                  text = paste(paste0(xlabel,':'), group,
                               paste0('<br>', dttype.label," of ", ylabel,  ':'), round(result, 2))
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
  p <- ggplotly(g, tooltip = "text") %>% plot.layout(dttype.label, xlabel, ylabel)#layout(font = font.family)
  
}

xtab.plot.bar.fact.moe <- function(table, format= c("percent", "nominal"), xlabel, ylabel, dttype.label) {
  yscale <- plot.format.nums(format)
  
  f <- ggplot(table, 
              aes(x = group,
                  y = result,
                  fill = group,
                  text = paste(paste0(xlabel,':'), group,
                               paste0('<br>', value," of ", ylabel,  ':'), round(result, 2),
                               paste0('<br>', 'Total (min, max):'), 
                               yscale(result), paste0("(", round(result-result_moe, 2), ", ", round(result+result_moe, 2), ")"))
              )
  ) +
    geom_col(position = position_dodge(preserve = "single")) + 
    geom_errorbar(aes(ymin = result - result_moe, ymax = result + result_moe),
                  alpha = .5,
                  width = 0.2,
                  position = position_dodge(width =  .9))

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
  
  p <- ggplotly(g, tooltip = "text") %>% plot.layout(dttype.label, xlabel, ylabel)#layout(font = font.family)
  
}