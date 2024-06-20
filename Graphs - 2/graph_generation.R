library(DaisTheme)
library(ggplot2)
library(data.table)
library(magrittr)

#########################################################
#Load in graph spread sheets
graph.data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 2/Graphs_spreadsheet.csv")



#################################
#Figure 1 onwards

  #"figure" is always a placeholder allowing for easier renaming of charts

figure <- "Figure 1"
figure_1_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 2/Figure_1.csv")
figure_1_data$Year <- factor(figure_1_data$Year, levels=c(2021, 2016))

figure.1 <- plot.column.dais(figure_1_data, stat, gender, group.by=Year, label=TRUE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 10,
                             export = FALSE)
figure <- "Figure 2"
figure_2_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 2/Figure_2.csv")
figure.2 <- plot.line.dais(figure_2_data, Year, stat,
                           show.points=TRUE,
                           plot.limit=c(0,50),
                           plot.fig.num = figure,
                           plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                           y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                           caption = graph.data[graph.data$Figure_number==figure,Caption],
                           unit.y="%",
                           export = FALSE)

figure <- "Figure 3"
figure_3_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 2/Figure_3.csv")
figure.3 <- plot.column.dais(figure_3_data, stat, gender, group.by=description, label=TRUE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 10,
                             export = FALSE)

figure <- "Figure 4"
figure_4_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 2/Figure_4.csv")

setkey(figure_4_data,pie,Gender,census.year)
figure_4_data[,lw:=mean_empin.roundedVals]
figure_4_data[,dw:=lw-shift(lw),by=.(pie,Gender)]
figure_4_data[,Gender:=as.character(Gender)]

figure.4 <- plot.scatter.dais(figure_4_data[pie > 5 & pie < 95 & census.year==2021], 
                              pie, 
                              dw, 
                              group.by=Gender,
                              plot.fig.num = figure,
                              plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                              y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                              x.axis= graph.data[graph.data$Figure_number==figure,X_Axis],
                              caption = graph.data[graph.data$Figure_number==figure,Caption],
                              unit.y = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                              unit.x = graph.data[graph.data$Figure_number==figure,X_Axis_Ticks],
                              export = FALSE) + 
  scale_y_continuous(labels = scales::dollar, limits = c(-1, 12)) +
  ggplot2::geom_abline(intercept = 0, slope = 0, colour = "#000000")+
  coord_cartesian(xlim = c(1, 102))+
  theme(axis.line.y = element_line())


figure <- "Figure 5"
figure_5_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 2/Figure_5.csv")
figure.5 <- plot.column.dais(figure_5_data, stat, gender, group.by=education, label=TRUE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 10,
                             export = FALSE)

figure <- "Figure 6"
figure_6_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 2/Figure_6.csv")
figure.6 <- plot.column.dais(figure_6_data, stat, age, group.by=description, label=TRUE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 8,
                             export = FALSE)

figure <- "Figure 7"
figure_7_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 2/Figure_7.csv")
figure.7 <- plot.column.dais(figure_7_data, stat, age, group.by=description, label=FALSE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 8,
                             export = FALSE) + 
  ggplot2::geom_text(data = figure_7_data, 
                     ggplot2::aes(y = stat+0.02*stat+8000, 
                                  label = stringr::str_c("$", scales::comma(round(stat, 1)))),
                     position = ggplot2::position_dodge(width = 0.6),
                     size = 8 * 0.352777778, 
                     family = "Replica-Regular",
                     angle = 90) +
  scale_y_continuous(limits = c(0,150000),
                     labels=scales::dollar, expand = c(0, 0))

figure <- "Figure 8"
figure_8_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 2/Figure_8.csv")
figure_8_data$education <- factor(figure_8_data$education, levels=unique(figure_8_data$education))
figure.8 <- plot.column.dais(figure_8_data, stat, education, group.by=description, label=FALSE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 8,
                             export = FALSE) + 
  ggplot2::geom_text(data = figure_8_data, 
                     ggplot2::aes(y = stat+0.02*stat+10000, 
                                  label = stringr::str_c("$", scales::comma(round(stat, 1)))),
                     position = ggplot2::position_dodge(width = 0.6),
                     size = 8 * 0.352777778, 
                     family = "Replica-Regular",
                     angle = 90) +
  scale_y_continuous(limits = c(0,120000),
                     labels=scales::dollar, 
                     expand = c(0, 0))

export.dais.plot("F8", figure.8, p.height = 6, p.width = 7.25, type = "pdf")


figure <- "Figure 9"
figure_9_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 2/Figure_9.csv") %>% dplyr::filter(!is.na(stat)) %>% as.data.table
figure.9 <- plot.column.dais(figure_9_data, stat, vismin, group.by=description, label=TRUE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 10,
                             export = FALSE)

figure <- "Figure 10"
figure_10_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 2/Figure_10.csv") %>% dplyr::filter(!is.na(stat)) %>% as.data.table
figure.10 <- plot.column.dais(figure_10_data, stat, vismin, order.bar	="descending", label=TRUE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 8,
                             export = FALSE)

figure <- "Figure 11"
figure_11_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 2/Figure_11.csv") %>% dplyr::filter(!is.na(stat)) %>% as.data.table
figure.11 <- plot.column.dais(figure_11_data, stat, vismin, group.by=description, label=TRUE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 10,
                             export = FALSE)

figure <- "Figure 12"
figure_12_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 2/Figure_12.csv") %>% dplyr::filter(!is.na(stat)) %>% as.data.table
figure.12 <- plot.column.dais(figure_12_data, stat, vismin, group.by=description, label=TRUE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 10,
                             export = FALSE)
#################################
#Exports

export.dais.plot("F1", figure.1, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F2", figure.2, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F3", figure.3, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F4", figure.4, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F5", figure.5, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F6", figure.6, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F7", figure.7, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F8", figure.8, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F9", figure.9, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F10", figure.10, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F11", figure.11, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F12", figure.12, p.height = 6, p.width = 7.25, type = "pdf")


