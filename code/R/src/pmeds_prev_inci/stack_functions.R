

get_max_y <- function(dt,strat_values,col_name='count'){
  
  max_y <- 0
  
  for( i in strat_values){
    
    dt_temp <- dt[strat_key == i]
    
    dt_sum <- dt_temp[,.(count = sum(get(col_name))),.(year,age)]
    
    max_m <- max(dt_sum$count)
    
    if(max_m > max_y){
      max_y <- max_m
    }
    
  }
  
  max_y
  
}



plot_and_save_stacked <- function(dt_temp,strat_values,
                                  plot_save_name,x_limits,max_y_value,
                                  plot_main_title='',
                                  g_ncols=5,
                                  g_height = 9,
                                  g_width = 18){
  
  cat("--plot_and_save_stacked---\n")
  print(strat_values)
  
  all_plots <- list()
  
  dt_temp2 <- NULL
  
  #max_y_value <- 0
  
  for(idx in strat_values){
    
    cat('loop strat_values ',idx,'\n')
    
    dt_temp2 <- dt_temp[strat_value == idx]
    #max_y_value <- 0
    cat('--plot_and_save_stacked--- max_y ',max_y_value,'\n')
    cat('--plot_and_save_stacked--- s rows  ',nrow(dt_temp2),'\n')
    
    title = paste0(idx)
    x<- ggplot_stacked(dt_temp2,plot_title=title, max_y=max_y_value,
                       show_legend='',
                       x_limits=x_limits)
    
    ### add to all_plots list
    all_plots[[(length(all_plots) +1)]] <- x
    
  }
  
  ######## produce one graph with legend to extract legend
  
  p_legend <- ggplot_stacked(dt_temp2,plot_title='', max_y=max_y_value, show_legend='b',
                             x_limits=x_limits)
  shared_legend <- extract_legend(p_legend)
  
  ######## arrange plots and save
  
  g <- grid.arrange(arrangeGrob(grobs = all_plots, shared_legend,  ncol=g_ncols ),
                    shared_legend, nrow = 2, heights = c(10, 1),
                    top = textGrob(plot_main_title,gp=gpar(fontsize=30,fontface = 'bold')))
  
  ggsave(file=plot_save_name, g, height=g_height, width=g_width) #saves g
  
}

get_plot_by_chapter <- function(dt_g,plot_title,y_col,show_legend,colors,
                                legend_title=''){
  
  y_col <- sym(y_col)
  
  g <- ggplot(dt_g, aes_string(x="age", y='rate' ,
                               group="strat_value",
                               color="strat_value")) + 
    
    geom_line() +
    scale_color_manual(name=legend_title, values = colors) +
    
    labs(title = paste0(plot_title), y ='Rate', x = "Age") +
    
    #labs(color='IMD Region')  +
    theme(axis.text.x=element_text(angle=90, hjust=1),
          legend.key = element_rect(fill="white"))
  #scale_y_continuous( labels = comma )
  
  # scale_y_continuous(limits=c(7000,-137500))
  
  if(show_legend != ''){
    g <- g +  theme(legend.position = "bottom", legend.text = element_text(margin = margin(r = 27)))
    #g <- g + guides(fill = guide_legend(ncol = 3))
  }else{
    g <- g +  theme(legend.position = "none")
  }
  
  g
  
}


plot_rates_by_chapter <- function(dt,y,keep_ages,plot_save_name,colors,
                                  plot_main_title='',legend_title=''){
  
  chaps <- seq(1,15)
  strat_values <- unique(dt$strat_value)
  
  all_plots <- list()
  for(c in chaps){
    
    dt_x <- dt[dt$prescribed_chapter_num == c & age %between% keep_ages]
    
    dt_x$strat_value <- factor(dt_x$strat_value, levels=strat_values)
    
    plot_title <- unique(dt_x$chapter_text)
    
    x <- get_plot_by_chapter(dt_x, plot_title,y_col='rate','',colors,legend_title)
    
    all_plots[[(length(all_plots) +1)]] <- x
    
  }
  
  p_legend <- get_plot_by_chapter(dt_x,'','','b',colors)
  
  shared_legend <- extract_legend(p_legend)
  plot_main_title2 <- ''
  
  g <- grid.arrange(arrangeGrob(grobs = all_plots, shared_legend,  ncol=3 ),
                    shared_legend, nrow = 2, heights = c(10, 1),
                    top = textGrob(plot_main_title2,gp=gpar(fontsize=30,fontface = 'bold')))
  
  
  
  ggsave(file=plot_save_name, g, height=15, width=15) #saves g
  
  
}

