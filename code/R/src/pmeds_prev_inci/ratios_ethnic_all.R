
fp <- paste0(analysis_data_dir,'prev_age_chapter_stacked_ethnicity_description_2023.csv')

dt_in <- fread(fp)



dt_chapters <- get_chapter_colors()
dt_chapters <- dt_chapters[BNF_CHAPTER_CODE < 16]
chapters_f <- rev(dt_chapters$chapter_text)

keep_ages <- c(5,10,20,30,40,50,60,70,80,90)

b = c("Any other Asian background","Bangladeshi","Black","Caribbean","Chinese",
      "Indian","Mixed","Other","Pakistani")

#b = c("Pakistani")

a <- "White"  


dt_x <- dt_in[strat_value == 'White' & age %in% keep_ages]

dt_all <- NULL

for(e in b){
  
  #e = "Pakistani"
  print(e)
  dt_y <- dt_in[strat_value == e & age %in% keep_ages]
  dt_out <- merge(x= dt_x,y = dt_y, by = c("prescribed_chapter_num","age" ), all.x=T)
  
  dt_out$rr <- dt_out$rate.y/dt_out$rate.x
  keep_cols <- c("prescribed_chapter_num","chapter_text.x","age","rate.x","rate.y","rr")
  dt_out <- dt_out[,..keep_cols]
  
  dt_out$ethnic <- e
  
  
  dt_all <- rbind(dt_all,dt_out)
  
}

dt_all$age <- factor(dt_all$age,levels = keep_ages)

dt_all$chapter_text.x <- factor(dt_all$chapter_text.x,levels = chapters_f)

setnames(dt_all,c('chapter_text.x','age','rr'), c('Chapter','Age','Ratio'))


dt_all[is.na(dt_all)] <- 1
dt_all <- dt_all[Ratio > 5, Ratio := 5]

mid_value <- (1/max(dt_all$Ratio))


g <- ggplot(dt_all,aes(Age,Chapter,fill=Ratio)) +
  geom_tile() +
  coord_fixed()+ 
  scale_fill_gradientn(
    colours = c("blue", "white", "red"),
    values = c(0,mid_value,1)
  ) +
  facet_wrap(~ethnic, ncol=3) +
  theme(strip.background =element_rect(fill="white"))
#theme(axis.text.y = element_blank())
#scale_fill_steps(breaks = bks)
# scale_x_discrete(expand = c(0,0))
#scale_fill_distiller(palette = "RdPu")

plot_save_name <- paste0(analysis_plots_dir,'ethnic_rates_heatmap.pdf')

#print(plot_save_name)
ggsave(file=plot_save_name, g, width=15,height = 10) #saves g
print('---')
