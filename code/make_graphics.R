require(ggplot2)
require(ggrepel)
require(tikzDevice)

df <- read.csv('complexity_measures_w_component.tsv',sep=',')

df$language <- gsub('_',' ',df$language)

artificial.df <- read.csv('../numerals-public/optimal_values.csv')

artificial.df <- unique(artificial.df[,-1])

rownames(artificial.df) <- artificial.df$lexicon

df$diff_broad <- df$ms_complexity_broad-artificial.df[as.character(df$vocab_size_broad),]$avg_ms_complexity

df$diff_narrow <- df$ms_complexity_narrow-artificial.df[as.character(df$vocab_size_narrow),]$avg_ms_complexity

df[is.na(df$diff_narrow),]$diff_narrow <- .5

#df[is.na(df$diff_broad),]$diff_broad <- .5

df1 <- df[,c('language','vocab_size_broad','ms_complexity_broad','diff_broad','component_broad')]

df2 <- df[,c('language','vocab_size_narrow','ms_complexity_narrow','diff_narrow','component_narrow')]

colnames(df1) = colnames(df2) = c('language','vocab_size','ms_complexity','diff','component')

merged.df <- rbind(df1,df2)

merged.df$Representation <- c(rep('Broad',nrow(df)),rep('Narrow',nrow(df)))

tikz('complexity_plot.tex',width=10,height=4)

#p <- ggplot() + geom_line(data=artificial.df[artificial.df$lexicon > 2 & artificial.df$lexicon <= 65,],aes(x=lexicon,y=avg_ms_complexity)) + 
#  geom_point(data=merged.df,aes(x=vocab_size,y=ms_complexity,color=component)) + 
#  geom_text_repel(data=merged.df[abs(merged.df$diff)>.05,],aes(x=vocab_size,y=ms_complexity,label=language),nudge_x=.2,hjust="top",vjust=-6,max.overlaps = 10) + xlab('Vocabulary Size') + ylab('Average Morphosyntactic Complexity')
#
#p + facet_grid(cols=vars(Representation))

p <- ggplot() + geom_line(data=artificial.df[artificial.df$lexicon > 2 & artificial.df$lexicon <= 65,],aes(x=lexicon,y=avg_ms_complexity)) + 
  geom_point(data=merged.df,aes(x=vocab_size,y=ms_complexity,color=component)) + 
  geom_text_repel(data=merged.df[abs(merged.df$diff)>.05,],aes(x=vocab_size,y=ms_complexity,label=language),force = 3,
                  force_pull = 0.1,
                  box.padding = 1,
                  point.padding = 1,
                  segment.size = 0.3,
                  segment.alpha = 0.5,
                  min.segment.length = 0,
                  max.overlaps = Inf,
                  xlim = c(NA, NA),
                  ylim = c(NA, NA)) + xlab('Vocabulary Size') + ylab('Average Morphosyntactic Complexity')

p + facet_grid(cols=vars(Representation)) + guides(color=guide_legend(title="P(k=2)"))# + labs(color="P(k=2)")

dev.off()



