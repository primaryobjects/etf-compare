#
# Kory Becker http://primaryobjects.com/kory-becker
# 12/14/2017
#

library(ggplot2)
library(reshape2)
library(RColorBrewer)

names <- c('JNK', 'HYG')

# Analysis of JNK holdings.
data <- read.csv('data/jnk-holdings.csv', skip=3)
data <- data[!is.na(data$Coupon),]
data <- data[data$Name != 'Cash_USD',]

# Clean names.
data$name <- data$Name
data$name <- sub(' \\d.+', '', data$name)
data$name <- sub('144A', '', data$name)
data$name <- trimws(data$name)

data$percent <- data$Weight
group1 <- data.frame(name = data$Identifier, percent = data$Weight, id = data$name)

data <- read.csv('data/hyg-holdings.csv', skip=10)
data <- data[1:(nrow(data)-2),]
data$name <- data$Name
data$name <- sub('144A', '', data$name)
data$name <- sub(' \\/ .+', '', data$name)
data$name <- trimws(data$name)

data$percent <- data$Weight
group2 <- data.frame(name = data$ISIN, percent = data$Weight, id = data$name)

# Convert weight string to numeric.
group1$percent <- as.numeric(sub('%', '', group1$percent))
group2$percent <- as.numeric(sub('%', '', group2$percent))

common <- group1[group1$name %in% intersect(group1$name, group2$name),]
group1CommonPercent <- nrow(common) / nrow(group1)
group2CommonPercent <- nrow(common) / nrow(group2)

# Funds in VYM, not existing in SDY.
group1missing <- group2[group2$name %in% setdiff(group2$name, group1$name), ]

# Funds in SDY, not existing in VYM.
group2missing <- group1[group1$name %in% setdiff(group1$name, group2$name), ]

# Build tidy dataset with holding counts and common holding counts.
holdingCounts <- data.frame(name = names[1], count = nrow(group1) - nrow(common), common = nrow(common))
holdingCounts <- rbind(holdingCounts, data.frame(name = names[2], count = nrow(group2) - nrow(common), common = nrow(common)))

# Transpose the data.frame into a format for plotting with variable colors.                       
holdingCounts <- melt(holdingCounts)

# Draw bar chart of common holdings.
g <- ggplot(holdingCounts, aes(x = name, y = value, fill = variable))
g <- g + geom_bar(alpha=I(.9), stat='identity')
g <- g + geom_col(position = position_stack(reverse = TRUE))
g <- g + ggtitle(paste('Common Holdings:', names[1], 'vs', names[2]))
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + xlab('ETF')
g <- g + ylab('Total Count of Holdings')
g <- g + scale_fill_manual(values=c('#303030', '#00bb00'), labels=c('Unique', 'In common'))
g <- g + theme(legend.title=element_blank())
g <- g + annotate("text", x = c(1,2), y=c(450, 420), label = c(paste0(round(group1CommonPercent * 100, 2), '%'), paste0(round(group2CommonPercent * 100, 2), '%')), colour = 'white')
print(g)

# Build tidy dataset with holding counts and common holding counts.
holdingPercents <- data.frame(name = names[1], holding = group1[group1$name %in% intersect(group1$name, group2$name),])
holdingPercents <- rbind(holdingPercents, data.frame(name = names[2], holding = group2[group2$name %in% intersect(group2$name, group1$name),]))

# Order by holding name.
holdingPercents <- holdingPercents[order(holdingPercents$holding.name),]

plotCommonHoldings <- function(holdingPercents, name1, name2, count = 10) {
  # Plots a chart of common holdings between two bond funds, in decreasing order according to the first fund.
  
  # Order by value decreasing in ggplot. We can do this by setting the factor levels.
  # Confirm by checking levels(x$holding.name). Note the order is different than a regular order() command.
  # See http://rstudio-pubs-static.s3.amazonaws.com/7433_4537ea5073dc4162950abb715f513469.html

  # Since the bond names can be different across funds, copy the names across by matching ISIN (holding.name). Since these are common holdings, there will always be a matching ISIN between funds.
  holdingPercents[holdingPercents$name == name2,]$holding.id <- holdingPercents[holdingPercents$name == name1,]$holding.id
  # Group the same company's bonds together under 1 company name, separated by fund.
  holdingPercents <- aggregate(holding.percent ~ holding.id + name, data = holdingPercents, FUN=sum)

  # Next, isolate the group to base the sort on.
  x <- holdingPercents[holdingPercents$name == name1,]
  # Set the sort column as a factor, specifying the levels based upon the value column.
  x$holding.id <- factor(x$holding.id, levels = x$holding.id[order(x$holding.percent, decreasing = TRUE)])

  # Append the other group (order no longer matters, since the factor is already set).
  y <- rbind(x, holdingPercents[holdingPercents$name == name2,])
  # Apply the ordered factor to the other group.
  y$holding.id <- x$holding.id
  # Limit to the first 20 results of the distinct groups. Note, plot order is already defined by the factor levels.
  y <- head(y[order(y$holding.id),], count * 2)
  
  # Draw bar chart of common holdings by percent, decreasing by first group.
  g <- ggplot(y, aes(x = holding.id, y = holding.percent, fill = name, group = name))
  g <- g + geom_bar(alpha=I(.9), stat='identity', position='dodge')
  g <- g + ggtitle(paste('Common Holdings:', name1, 'vs', name2))
  g <- g + theme_bw()
  g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1))
  g <- g + xlab('Aggregated Holdings')
  g <- g + ylab('Percent %')
  g <- g + scale_fill_manual(values=c('#303030', '#808080'))
  g <- g + theme(legend.title=element_blank())
  print(g)
}

plotCommonHoldings(holdingPercents, names[1], names[2])
plotCommonHoldings(holdingPercents, names[2], names[1])

percentEnergy <- function(data) {
  # Find energy holdings.
  e <- grepl('ENERGY|WIND|OCEAN| OIL |DRILL |HALCON|OCEANICS| OFFSHORE |DRILLING|PETROLEUM| PETRO | COAL ', data$id, ignore.case = TRUE)
  energy <- data[which(e),]
  
  # Percent energy.
  sum(energy$percent)
}

energy1 <- percentEnergy(group1)
energy2 <- percentEnergy(group2)
energy <- data.frame(name = names, value = c(energy1, energy2), variable = factor(0))
energy <- rbind(energy, data.frame(name = names, value = c(100 - energy1, 100 - energy2), variable = factor(1)))

# Draw bar chart of percent energy.
g <- ggplot(energy, aes(x = name, y = value, fill = variable))
g <- g + geom_bar(alpha=I(.9), stat='identity')
g <- g + geom_col(position = position_stack(reverse = TRUE))
g <- g + ggtitle(paste('Percent Energy:', names[1], 'vs', names[2]))
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + xlab('ETF')
g <- g + ylab('Percent of Energy Holdings')
g <- g + scale_fill_manual(values=c('#00bb00', '#303030'), labels=c('Energy', 'Non-energy'))
g <- g + theme(legend.title=element_blank())
g <- g + annotate("text", x = c(1,2), y=c(4, 4), label = c(paste0(round(energy2, 2), '%'), paste0(round(energy1, 2), '%')), colour = 'white')
print(g)

# Consolidate duplicate rows (same company name, different bond rates), adding weight percents.
consolidated <- group1
consolidated$id <- group1[group1$name == consolidated$name,]$id
consolidated <- aggregate(percent ~ id, data = consolidated, FUN=sum)
# Set sort order for plot by percent.
consolidated$id <- factor(consolidated$id, levels = consolidated$id[order(consolidated$percent, decreasing = TRUE)])
consolidated <- consolidated[order(consolidated$id),]

colourCount = 50
getPalette = colorRampPalette(brewer.pal(99, "Paired"))

# Draw bar chart of all holdings, sorted by weight.
g <- ggplot(head(consolidated, 50), aes(x = id, y = percent, fill=id))
g <- g + geom_bar(alpha=I(.9), stat='identity')
g <- g + ggtitle(paste('Top 50 Consolidated Holdings for', names[1]))
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + xlab('Company')
g <- g + ylab('Percent %')
g <- g + theme(legend.title=element_blank(), legend.position='none')
g <- g + scale_fill_manual(values=getPalette(colourCount))
print(g)

# Note, consolidated total is 99.41155. The remaining percent is cash 0.588456. Total = 100%.
write.csv(consolidated, file=paste0('data/', tolower(names[1]), '-consolidated.csv'))
