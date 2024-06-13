library(DaisTheme)
library(ggplot2)
library(data.table)


#########################################################
#Load in graph spread sheets
graph.data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 1/Graphs_spreadsheet.csv")



#################################
#Figure 2 onwards (Figure 1 is manual)
figure <- "Figure 2"
figure_2_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 1/Figure_2.csv")
figure.2 <- plot.column.dais(figure_2_data, stat, year, order.bar="ascending", label=TRUE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 10,
                             export = FALSE,
                             export.name = "F2")

figure <- "Figure 3"
figure_3_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 1/Figure_3.csv")
figure.3 <- plot.column.dais(figure_3_data, stat, description, order.bar="descending", label=TRUE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 8,
                             export = FALSE,
                             export.name = "F3")

figure <- "Figure 4"
figure_4_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 1/Figure_4.csv")
figure.4 <- plot.column.dais(figure_4_data, stat, province, order.bar="descending", label=TRUE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 8,
                             export = FALSE,
                             export.name = "F4")

figure <- "Figure 5"
figure_5_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 1/Figure_5.csv")
figure.5 <- plot.column.dais(figure_5_data, stat, cma, order.bar="descending", label=TRUE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 8,
                             export = FALSE,
                             export.name = "F5")


ticks.seq <- set.ticks.seq(130000, 0, unit = "$", 
                           lang = "EN")

figure <- "Figure 6"
figure_6_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 1/Figure_6.csv")
figure.6 <- plot.column.dais(figure_6_data, salary, cma, order.bar="descending", group.by=occupation, label=FALSE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             label.unit = graph.data[graph.data$Figure_number==figure,Y_Axis_Ticks],
                             label.adjust = 0.02,
                             label.text.size = 8,
                             export = FALSE,
                             export.name = "F6") + 
  ggplot2::geom_text(data = figure_6_data, 
                     ggplot2::aes(y = salary+0.02*salary+13000, 
                                  label = stringr::str_c("$", scales::comma(round(salary, 1)))),
                     position = ggplot2::position_dodge(width = 0.6),
                     size = 8 * 0.352777778, 
                     family = "Replica-Regular",
                     angle = 90) +
  scale_y_continuous(limits = c(0,130000), breaks = ticks.seq$breaks, 
                     labels = ticks.seq$labels)

rm(ticks.seq)

figure <- "Figure 7"
figure_7_data <- fread("C:/Users/alockhart/Desktop/Canada's Tech Workers/Graphs - 1/Figure_7.csv")
figure.7 <- plot.column.dais(figure_7_data, med_inc, area, order.bar="ascending", group.by=country, colours=set.colours(2,categorical.choice=c("hot.pink","teal")),
                             column.width=1, label=FALSE,
                             plot.fig.num = figure,
                             plot.title= graph.data[graph.data$Figure_number==figure,Figure_title],
                             y.axis= graph.data[graph.data$Figure_number==figure,Y_Axis],
                             caption = graph.data[graph.data$Figure_number==figure,Caption],
                             export = FALSE,
                             export.name = "F7") +
  theme(axis.text.x=element_blank())

#################################
#Exports

export.dais.plot("F2", figure.2, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F3", figure.3, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F4", figure.4, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F5", figure.5, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F6", figure.6, p.height = 6, p.width = 7.25, type = "pdf")
export.dais.plot("F7", figure.7, p.height = 6, p.width = 7.25, type = "pdf")


