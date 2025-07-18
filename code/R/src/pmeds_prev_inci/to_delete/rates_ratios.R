

fp <- paste0(analysis_data_dir,'prev_age_chapter_stacked_imd_quintile_2023.csv')

dt_in <- fread(fp)

dt_x <- dt_in[dt_in$strat_value == 1 & 
                dt_in$age %in% c(5,10,20,30,40,50,60,70,80,90)]

dt_y <- dt_in[dt_in$strat_value == 5 & 
                dt_in$age %in% c(5,10,20,30,40,50,60,70,80,90)]

dt_out <- merge(x= dt_x,y = dt_y, by = c("prescribed_chapter_num","age" ), all.x=T)

dt_out$rr <- dt_out$rate.x/dt_out$rate.y
keep_cols <- c("prescribed_chapter_num","chapter_text.x","age","rate.x","rate.y","rr")
dt_out <- dt_out[,..keep_cols]

data_save_name <- paste0(analysis_data_dir,'rates_ratio_2023.csv')
#print(data_save_name)
save_dt_to_csv(dt_out,data_save_name)

setkey(dt_out, 'prescribed_chapter_num','age')

dt_chapters <- get_chapter_colors()
dt_chapters <- dt_chapters[BNF_CHAPTER_CODE < 16]
chapters_f <- rev(dt_chapters$chapter_text)


age_f <- c(5,10, 20, 30, 40, 50, 60, 70, 80, 90)
dt_out$age <- factor(dt_out$age,levels = age_f)

dt_out$chapter_text.x <- factor(dt_out$chapter_text.x,levels = chapters_f)

setnames(dt_out,c('chapter_text.x','age','rr'), c('Chapter','Age','Ratio'))


bks <- c(0.25,0.5,0.75,1.0,1.5,2.0,2.5,3.0,3.5,4.0)
#bks <- c( 0.38, 0.63, 0.88 ,1.13, 1.38, 1.63, 1.88, 2.13, 2.38 ,2.63, 2.88, 3.13, 3.38, 3.63, 3.88)

g <- ggplot(dt_out,aes(Age,Chapter,fill=Ratio)) +
  geom_tile() +
  coord_fixed() + 
  #scale_fill_steps(breaks = bks)
  scale_fill_gradientn(
    colours = c("blue", "white", "red"),
    values = c(0, 0.25, 1)
  ) 
# scale_x_discrete(expand = c(0,0))
#scale_fill_distiller(palette = "RdPu")

plot_save_name <- paste0(analysis_plots_dir,'imd_rates_heat.pdf')

print(plot_save_name)
ggsave(file=plot_save_name, g, height = 10, width=7) #saves g

