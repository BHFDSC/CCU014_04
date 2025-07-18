
cs <- seq(0,25)


colors = c("#1984c5", "#22a7f0", "#63bff0", "#a7d5ed",
           "#e2e2e2", "#e1a692", "#de6e56", "#4F8F23",
           "#e14b31", '#432371', "#a86464", '#005bae',
           "#c23728", "#6d4b4b",  "#AA00FF","#23628F")

plot_labels <- c('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15+')


quint_imds <- seq(1,5)


years <- c(2023)

plot_cs <- function(dt,plot_title='',show_legend=''){
  
  g <- ggplot(dt, aes(fill=count_cs, y=count, x=age)) + 
    geom_bar(position = position_fill(reverse = TRUE), stat="identity", width=1)+
    scale_fill_manual(values=colors, labels=plot_labels) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_discrete(breaks = c(1,10,20,30,40,50,60,70,80,90,100)) +
    labs(fill="N of Chemical Substances", x="Age",y="Proportion", title = plot_title)
  
  if(show_legend != ''){
    g <- g +  theme(legend.position = "bottom",legend.text = element_text(size = 6,hjust = 0.5, lineheight = 121) )
    #legend.text = element_text(margin = margin(r = 27)))
    g <- g + guides(fill = guide_legend(nrow = 2))
  }else{
    g <- g +  theme(legend.position = "none")
  }
  
  
  g
}

plot_and_save_cs_counts <- function(dt_temp,strat_values,plot_save_name,
                                    g_ncols=5,plot_main_title=''){
  
  cat("--plot_and_save_stacked---\n")
  print(strat_values)
  
  all_plots <- list()
  
  dt_temp2 <- NULL
  
  #max_y_value <- 0
  
  for(idx in strat_values){
    
    #print(idx)
    
    dt_temp2 <- dt_temp[strat_key == idx]
    #max_y_value <- 0
    #cat('--plot_and_save_stacked--- max_y ',max_y_value,'\n')
    
    title = paste0(idx)
    x <- plot_cs(dt_temp2,plot_title=title,show_legend='')
    
    ### add to all_plots list
    all_plots[[(length(all_plots) +1)]] <- x
    
  }
  
  ######## produce one graph with legend to extract legend
  
  p_legend <- x <- plot_cs(dt_temp2,show_legend='b')
  shared_legend <- extract_legend(p_legend)
  
  ######## arrange plots and save
  
  g <- grid.arrange(arrangeGrob(grobs = all_plots, shared_legend,  ncol=g_ncols ),
                    shared_legend, nrow = 2, heights = c(10, 1),
                    top = textGrob(plot_main_title,gp=gpar(fontsize=30,fontface = 'bold')))
  print(plot_save_name)
  ggsave(file=plot_save_name, g, height=10, width=18) #saves g
  
}

output_imd_quintile <- function(){
  
  fp_data <- paste0(data_root_dir,'cs_counts_imd_quintile_dec_2023.csv')
  print(fp_data)
  
  dt <- fread(fp_data)
  
  dt <- dt[!is.na(dt$age)]
  
  dt <- dt[ dt$age %between% c(1,95)]
  
  dt[is.na(dt)] <- 0
  
  dt <- dt[count_cs > 14 , count_cs := 15] 
  
  dt <- dt[,.(count = sum(count)), by=c('age','count_cs','imd_quintile')]
  
  aseq <- seq(1,95)
  
  dt$age <- factor(dt$age, levels= aseq)
  
  cs <- seq(0,21)
  dt$count_cs <- factor(dt$count_cs, levels= cs)
  
  setnames(dt,'imd_quintile','strat_key')
  
  
  plot_save_name <- paste0(analysis_plots_dir,
                           '/stack_cs_by_imd_quintile_2023.pdf')
  
  plot_and_save_cs_counts(dt,quint_imds,plot_save_name,
                          g_ncols=3,plot_main_title='')
  
}


output_ethnic <- function(){
  fp_data <- paste0(data_root_dir,'cs_counts_ethnicity_description_dec_2023.csv')
  print(fp_data)
  
  dt <- fread(fp_data)
  
  dt <- dt[!is.na(dt$age)]
  
  dt <- dt[ dt$age %between% c(1,95)]
  
  dt <- ethnic_desc_collapse(dt,'')
  
  dt[is.na(dt)] <- 0
  
  dt <- dt[count_cs > 14 , count_cs := 15] 
  
  dt <- dt[,.(count = sum(count)), by=c('age','count_cs','ethnicity_description')]
  
  aseq <- seq(1,95)
  
  dt$age <- factor(dt$age, levels= aseq)
  
  cs <- seq(0,21)
  dt$count_cs <- factor(dt$count_cs, levels= cs)
  
  setnames(dt,'ethnicity_description','strat_key')
  strat_values <- sort( unique(dt$strat_key) )
  
  plot_save_name <- paste0(analysis_plots_dir,
                           '/stack_cs_by_ethnic_dec_2023.pdf')
  
  plot_and_save_cs_counts(dt,strat_values,plot_save_name,
                          g_ncols=4,plot_main_title='')
  
}

output_imd_quintile()
output_ethnic()
