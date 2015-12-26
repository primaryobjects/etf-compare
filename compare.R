#
# Kory Becker http://primaryobjects.com/kory-becker
# 12/24/2015
#

library(ggplot2)
library(reshape2)

names <- c('SDY', 'VYM')
group1 <- read.csv('data/sdy-holdings.tsv', sep = '\t', col.names = c('name', 'percent'), header = FALSE)
group2 <- read.csv('data/vym-holdings.tsv', sep = '\t', col.names = c('name', 'percent'), header = FALSE)

# Convert percent string to numeric.
group1$percent <- as.numeric(sub('%', '', group1$percent)) / 100
group2$percent <- as.numeric(sub('%', '', group2$percent)) / 100

common <- group1[group1$name %in% intersect(group1$name, group2$name),]
group1CommonPercent <- nrow(common) / nrow(group1)
group2CommonPercent <- nrow(common) / nrow(group2)

# Funds in VYM, not existing in SDY.
group1missing <- group2[group2$name %in% setdiff(group2$name, group1$name), ]

# Funds in SDY, not existing in VYM.
group2missing <- group1[group1$name %in% setdiff(group1$name, group2$name), ]

# Build tidy dataset with holding counts and common holding counts.
holdingCounts <- data.frame(name = 'SDY', count = nrow(group1) - nrow(common), common = nrow(common))
holdingCounts <- rbind(holdingCounts, data.frame(name = names[2], count = nrow(group2) - nrow(common), common = nrow(common)))

# Transpose the data.frame into a format for plotting with variable colors.                       
holdingCounts <- melt(holdingCounts)

# Draw bar chart of common holdings.
g <- ggplot(holdingCounts, aes(x = name, y = value, fill = variable))
g <- g + geom_bar(alpha=I(.9), stat='identity')
g <- g + ggtitle(paste('Common Holdings:', names[1], 'vs', names[2]))
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + xlab('ETF')
g <- g + ylab('Total Count of Holdings')
g <- g + scale_fill_manual(values=c('#303030', '#00bb00'), labels=c('Unique', 'In common'))
g <- g + theme(legend.title=element_blank())
g <- g + annotate("text", x = c(1,2), y=c(30, 30), label = c(paste0(round(group1CommonPercent * 100, 2), '%'), paste0(round(group2CommonPercent * 100, 2), '%')), colour = 'white')
print(g)

# Build tidy dataset with holding counts and common holding counts.
holdingPercents <- data.frame(name = names[1], holding = group1[group1$name %in% intersect(group1$name, group2$name),])
holdingPercents <- rbind(holdingPercents, data.frame(name = names[2], holding = group2[group2$name %in% intersect(group2$name, group1$name),]))

# Order by holding name.
holdingPercents <- holdingPercents[order(holdingPercents$holding.name),]

plotCommonHoldings <- function(holdingPercents, name1, name2, count = 10) {
  # Plots a chart of common holdings between two funds, in decreasing order according to the first fund.
  
  # Order by value decreasing in ggplot. We can do this by setting the factor levels.
  # Confirm by checking levels(x$holding.name). Note the order is different than a regular order() command.
  # See http://rstudio-pubs-static.s3.amazonaws.com/7433_4537ea5073dc4162950abb715f513469.html
  
  # First, isolate the group to base the sort on.
  x <- holdingPercents[holdingPercents$name == name1,]
  # Set the sort column as a factor, specifying the levels based upon the value column.
  x$holding.name <- factor(x$holding.name, levels = x$holding.name[order(x$holding.percent, decreasing = TRUE)])
  # Append the other group (order no longer matters, since the factor is already set).
  y <- rbind(x, holdingPercents[holdingPercents$name == name2,])
  # Apply the ordered factor to the other group.
  y$holding.name <- x$holding.name
  # Limit to the first 20 results of the distinct groups. Note, plot order is already defined by the factor levels.
  y <- head(y[order(y$holding.name),], count * 2)
  
  # Draw bar chart of common holdings by percent, decreasing by first group.
  g <- ggplot(y, aes(x = holding.name, y = holding.percent, fill = name, group = name))
  g <- g + geom_bar(alpha=I(.9), stat='identity', position='dodge')
  g <- g + ggtitle(paste('Common Holdings:', name1, 'vs', name2))
  g <- g + theme_bw()
  g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1))
  g <- g + xlab('Holdings')
  g <- g + ylab('Percent %')
  g <- g + scale_fill_manual(values=c('#303030', '#808080'))
  g <- g + theme(legend.title=element_blank())
  print(g)
}

plotCommonHoldings(holdingPercents, names[1], names[2])
plotCommonHoldings(holdingPercents, names[2], names[1])

# Plot dividend difference.
div1 <- read.csv('data/sdy-dividends.tsv', sep = '\t', col.names = c('ex', 'type', 'amount', 'declaration', 'record', 'payment'), header = FALSE)
div2 <- read.csv('data/vym-dividends.tsv', sep = '\t', col.names = c('ex', 'type', 'amount', 'declaration', 'record', 'payment'), header = FALSE)

# Remove long-term cap gain payouts.
div1b <- div1[abs(div1$amount - mean(div1$amount)) < 1,]

div <- data.frame((mean(head(div1b$amount, 6)) / 74.07) * 4 * 100, (mean(head(div2$amount, 6)) / 67.35) * 4 * 100)
names(div) <- names
divm <- melt(div)

g <- ggplot(divm, aes(x = variable, y = value))
g <- g + geom_bar(alpha=I(.9), stat='identity')
g <- g + ggtitle('Dividend Yield')
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + xlab('ETF')
g <- g + ylab('Yield')
g <- g + annotate("text", x = c(1,2), y=c(0.5, 0.5), label = c(paste0(round(div[1,1], 2), '%'), paste0(round(div[1,2], 2), '%')), colour = 'white')
print(g)
