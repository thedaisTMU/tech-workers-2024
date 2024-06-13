library(DaisTheme)
library(data.table)
library(scales)
library(ggplot2)
library(SFSTheme)

load("figure_1.rdata")
load("figure_2.rdata")
load("figure_3.rdata")
load('figure_4.rdata')

skills <- figure_4[, c("subset", "mean","nd")]  
skills <- as.data.table(skills)
skills <- melt(skills, na.rm = FALSE, id = 'subset')
skills$variable <- as.character(skills$variable)
skills$variable[skills$variable == 'mean'] <- 'Digital'
skills$variable[skills$variable == 'nd'] <- 'Non-digital'

figure_1$digital_assign <- as.character(figure_1$digital_assign)
colnames(figure_1)[c(6,8)] <- c('di','pct')
figure_1$pct <- figure_1$pct/100
figure_1$digital_assign[figure_1$digital_assign =='0'] <- "Non-digital"
figure_1$digital_assign[figure_1$digital_assign =='1'] <- "Digital"

figure.1 <- ggplot(figure_1,aes(x=pct,y=-di))+
  sfs.base.theme() +
  geom_point(aes(size = 80,colour=digital_assign))+
  scale_x_continuous(expand = c(0, 0),limits = c(0,max(figure_1$pct)+0.005), labels = percent_format(accuracy = 1))+
  scale_y_continuous(limits = c(-500, 0),breaks= c(-500, 0),labels = c("",""))+
  scale_colour_manual(values = c("#66367b",'#bdbbc0'))+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks.y = element_blank(), axis.text.x = element_text(size=16), axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        legend.text = element_text(size = 16),legend.title = element_text(size = 16))+
  labs(y="Digital intensity of skill",
       x='Percentage of job postings containing skill',
       color="Type of skill")+
  guides(size = 'none', color = guide_legend(nrow = 1))

figure_2$digital_assign <- as.character(figure_2$digital_assign)
colnames(figure_2)[c(5,7)] <- c('di','pct')
figure_2$pct <- figure_2$pct/100
figure_2$digital_assign[figure_2$digital_assign =='0'] <- "Non-digital"
figure_2$digital_assign[figure_2$digital_assign =='1'] <- "Digital"

figure.2 <- ggplot(figure_2,aes(x=pct,y=-di))+
  sfs.base.theme() +
  geom_point(aes(size = 80,colour=digital_assign))+
  scale_x_continuous(expand = c(0, 0),limits = c(0,max(figure_2$pct)+0.005), labels = percent_format(accuracy = 1))+
  scale_y_continuous(limits = c(-500, 0),breaks= c(-500, 0),labels = c("",""))+
  scale_colour_manual(values = c("#66367b",'#bdbbc0'))+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks.y = element_blank(), axis.text.x = element_text(size=16), axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        legend.text = element_text(size = 16),legend.title = element_text(size = 16))+
  labs(y="Digital intensity of skill",
       x='Percentage of job postings containing skill',
       color="Type of skill")+
  guides(size = 'none', color = guide_legend(nrow = 1))

figure_3$digital_assign <- as.character(figure_3$digital_assign)
colnames(figure_3)[c(5,7)] <- c('di','pct')
figure_3$pct <- figure_3$pct/100
figure_3$digital_assign[figure_3$digital_assign =='0'] <- "Non-digital"
figure_3$digital_assign[figure_3$digital_assign =='1'] <- "Digital"

figure.3 <- ggplot(figure_3,aes(x=pct,y=-di))+
  sfs.base.theme() +
  geom_point(aes(size = 80,colour=digital_assign))+
  scale_x_continuous(expand = c(0, 0),limits = c(0,max(figure_3$pct)+0.005), labels = percent_format(accuracy = 1))+
  scale_y_continuous(limits = c(-500, 0),breaks= c(-500, 0),labels = c("",""))+
  scale_colour_manual(values = c("#66367b",'#bdbbc0'))+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks.y = element_blank(), axis.text.x = element_text(size=16), axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        legend.text = element_text(size = 16),legend.title = element_text(size = 16))+
  labs(y="Digital intensity of skill",
       x='Percentage of job postings containing skill',
       color="Type of skill")+
  guides(size = 'none', color = guide_legend(nrow = 1))

figure.4 <- plot.column.sfs(skills, value, subset, group.by = variable,
                            order.bar="ascending", 
                            label=FALSE,
                            # plot.fig.num = "Figure 4",
                            # plot.title= "Average number of digital and non-digital skills by job type, 2023",
                            y.axis= "Average number of skills per job posting",
                            stacked = TRUE,
                            colours = c('#66367b','#bdbbc0'))+
  geom_text(aes(label=format(round(value, digits = 1))), color = c('white','white','white','black','black','black'),
            position = position_stack(.5), size=4)+
  theme(axis.text.x = element_text(angle=0, hjust=0.5), axis.ticks = element_blank())

export.sfs.plot("fig_1_sfs.pdf",figure.1, p.height = 7, p.width = 11)
export.sfs.plot("fig_2_sfs.pdf",figure.2, p.height = 7, p.width = 11)
export.sfs.plot("fig_3_sfs.pdf",figure.3, p.height = 7, p.width = 11)
export.sfs.plot("fig_4_sfs.pdf",figure.4, p.height = 7, p.width = 11)
