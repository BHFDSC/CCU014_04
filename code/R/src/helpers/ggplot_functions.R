
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}


addUnits <- function(n) {
  
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0((n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0((n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0((n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0((n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

ggplot_stacked <- function(dt,plot_title='', max_y=0, 
                           show_legend='',x_limits=c(0,0),
                           y_col_which = 'count'){
  
  #cat('ggplot_stacked max_y ', max_y,'\n')
  legend_title <- ""
  ycol = sym(y_col_which)
  
  scale_manual_colors <- ggplot_manual_chapters_colors()
  
  g <- ggplot(data = dt, aes(x = age, y = !!ycol, fill = chapter_text, width=1)) + 
    geom_bar(position='stack', stat='identity')
  
  if(max_y > 0 ){
    g <- g +  scale_y_continuous(limits = c(0,max_y), n.breaks = 4,  labels=addUnits )
  }else{
    g <- g +  scale_y_continuous( labels=addUnits )
  }
  
  g <- g +   scale_x_continuous(limits = x_limits, n.breaks = 10 ) +
    #ylim(0, max_y) +
    #scale_y_continuous(labels = comma) +
    scale_fill_manual( legend_title, values = scale_manual_colors) 
  g <- g + labs(title = plot_title, y = "Count Dispensed", x = "Age")
  
  
  if(show_legend != ''){
    g <- g +  theme(legend.position = "bottom", legend.text = element_text(margin = margin(r = 27)))
    #g <- g + guides(fill = guide_legend(ncol = 3))
  }else{
    g <- g +  theme(legend.position = "none")
  }
  
  
  g
}


