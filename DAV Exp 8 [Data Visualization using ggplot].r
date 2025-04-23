# install.packages("ggplot2")
# install.packages("vcd")
# install.packages("hflights")
# install.packages("dplyr")

library(ggplot2)
library(vcd)
library(hflights)
library(dplyr)

df = data.frame(Months=c("Jan", "Feb", "Mar", "Apr"), Sales=c(4.2, 10, 29.5, 15))
df$Months <- factor(df$Months, levels = c("Jan", "Feb", "Mar", "Apr"))
ggplot(data=df, aes(x=Months, y=Sales)) + geom_point()

ggplot(data=df, aes(x=Months, y=Sales)) + geom_bar(stat="identity", aes(fill=Sales)) + geom_text(aes(label=Sales, vjust=Sales/2), size=3.5)
p = ggplot(data=df, aes(x=Months,y=Sales)) + geom_bar(stat="identity", aes(fill=Months)) + coord_flip()

### Bar plots with labels

# Outside bars
ggplot(data=df, aes(x=Months, y=Sales)) + geom_bar(stat="identity", fill="steelblue") + geom_text(aes(label=Sales, vjust=Sales/2), size=3.5)

# Change barplot line colors by groups
p = ggplot(data=df, aes(x=Months, y=Sales, fill=Months)) + geom_bar(stat="identity")
p

# Change legend position
p + theme(legend.position="top")
p + theme(legend.position="bottom")

# Remove legend
p + theme(legend.position="none")

# Add trend line
p = ggplot(df, aes(x=Months, y=Sales, fill=Months)) + geom_bar(stat="identity")
p + geom_line(aes(y=Sales), group=1, size=2, color="black") + geom_text(aes(label=Sales), vjust=-.5, size=-3.5)

# Dataset: hflights
f_sub = filter(hflights, ArrDelay>400)
p = ggplot(f_sub, aes(x=ArrTime, y=ArrDelay, color=factor(DayOfWeek)))
p + geom_point()
f_sub = filter(hflights, DayOfWeek>5)
p = ggplot(f_sub, aes(x=DepTime, y=DepDelay, color=factor(Cancelled)))
p + geom_point() + geom_smooth() + geom_line()

### Single numeric variable

# Histogram
f_sub = filter(hflights, DepDelay<200)

# Density
ggplot(f_sub, aes(x=DepDelay)) + geom_density()

# Box plots
ggplot(f_sub, aes(x=DepDelay, y=DepDelay)) + geom_boxplot(color="purple")

# Violin plot
ggplot(f_sub, aes(x=DepDelay, y=DepDelay)) + geom_violin(color="purple")

# Comparing density with normal
ggplot(f_sub, aes(x=DepDelay)) + geom_density(color="blue") + stat_function(fun=dnorm, args=list(mean=mean(f_sub$DepDelay), sd=sd(f_sub$DepDelay)), color="green")

# Single categorical variable
f_sub = hflights%>%filter(UniqueCarrier %in% c("UA", "AA", "XE"))
p = ggplot(f_sub, aes(x=UniqueCarrier, y=DepDelay, color=Cancelled))
p + geom_point()
ggplot(f_sub, aes(x=UniqueCarrier)) + geom_bar(color="red", fill="yellow", width=0.5) + coord_polar(theta='y') + xlab("Carrier Name") + ylab("Frequency") + ggtitle("Carrier Frequencies")

# Numerical data
f_sub = filter(hflights, DepDelay<50)
ggplot(f_sub, aes(x=DepTime)) + geom_bar(color='orange', width=0.5) + coord_flip()

# Categorical data
ggplot(hflights, aes(x=Origin, fill=UniqueCarrier)) + geom_bar()