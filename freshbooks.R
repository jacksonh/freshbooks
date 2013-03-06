

plotRevenueByWeek <- function (estimatedBillingFile) {

	weekColumn <- function () {
		apply (items, 1, function (r) strftime (as.Date (r[2], "%m/%d/%y"), format = "%W"))
	}

	items <- read.csv (estimatedBillingFile)

	items$Week <- weekColumn ()

	a <- aggregate (items$Estimated.Billing, by=list (Week=items$Week), FUN=sum)
	
	ymin = 0

	# barplot likes to go over teh ymax, so inflate it a little
	ymax = max (a$x) + 500

	barplot (a$x, names.arg=a$Week, 
			 ylim = c (ymin, ymax),
			 main = "Revenue by Week", 
			 xlab = "Week of Year",
			 ylab = "Revenue in Dollars")
}