

source('./src/top_n/top_n_bar_functions.R')

years = c(2019,2020,2021,2022)
years = c(2023)

dt_bnf <- get_bnf_dt()
dt_colors <- get_chapter_colors()

bnf_cols <- c('BNF_SECTION','BNF_SUBPARAGRAPH','BNF_CHEMICAL_SUBSTANCE')
#bnf_cols <- c('BNF_SECTION')

top_n <- 10

x<- function(dt,top_n,col,sex){
  
  cols <- c('year','combined_age_band','combined_sex','chapter_color')
  cols <- append(cols,col)
  
  dt <- dt[combined_sex == sex]
  
  dt <- dt[,.(count = sum(count)),by=cols]
  
  dt <- setorder(dt,year,combined_age_band,combined_sex,-count)
  
  dt_x <- dt[, head(.SD, top_n), by=combined_age_band]
  
  setnames(dt_x,col,'label_text')
  
  dt_f <- NULL
  for(i in age_bands){
    dab <- dt_x[combined_age_band == i]
    dt_f <- rbind(dt_f,dab)
  }
  
  dt_x <- dt_f
  
  dt_x$id <- seq(1, nrow(dt_x))
  
  out_file_csv =  paste0(analysis_data_dir,'top_meds_age_',sex,'_',col,'_linked.csv')
  save_dt_to_csv(dt_x,out_file_csv)
  
  dt_x
  
}



out_file <- paste0(data_root_dir,'top_meds_age_sex_linked.csv')
dt <- get_data_top_n(out_file,dt_bnf,dt_colors)

for( bnf_col in bnf_cols){
  
  for(y in years){
    
    all_plots <- list()
    
    dt_temp <- dt[year == y]
    
    dt_x <- x(dt_temp,top_n,bnf_col,"Male")
    all_plots[[(length(all_plots) +1)]] <- get_plot(dt_x,"Male")
    
    dt_x <- x(dt_temp,top_n,bnf_col,"Female")
    all_plots[[(length(all_plots) +1)]] <- get_plot(dt_x, 'Female')
    
    plot_main_title2 <- paste0('link',' ',bnf_col, ' ', y)
    plot_main_title2 <- ''
    
    g <- grid.arrange(arrangeGrob(grobs = all_plots,  ncol=2 ),
                      nrow = 1,
                      top = textGrob(plot_main_title2,gp=gpar(fontsize=30,fontface = 'bold')))
    
    
    plot_save_name <- paste0(analysis_plots_dir,paste0('bar_linked_',bnf_col,'_',y,'.pdf'))
    print(plot_save_name)
    ggsave(file=plot_save_name, g,limitsize = FALSE, height = 30, width = 20) #saves g
    
  }
  
}







