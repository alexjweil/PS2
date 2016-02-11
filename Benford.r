#' Performs the common portion of Benford's test.
#' @param frequencies A vector, list, or matrix of digit frequencies
#' @param critical.values The critical values for the test in question
#' @param name The name of the test
#' @param test.function The function used to conduct the test
#' @return A list containing the Name, Test Statistic, Stars, and relative frequencies of each digit in the input.
test = function(frequencies, critical.values, name, test.function) {
	relative.frequencies = setNames(frequencies - log10(1 + 1/(1:9)), 1:9)
	test.statistic = test.function(relative.frequencies)
	stars  = sum(critical.values < test.statistic) + 1
	return(append(list(Name=name, Test.Statistic=test.statistic, Stars=stars), relative.frequencies))
}

#' Performs either Cho-Gains's d test or Leemis's m test for Benford's law.
#' @param frequencies A vector, list, or matrix of digit frequencies
#' @param type \code{'d'} if Cho-Gains's test is to be run, \code{'m'} if Leemis's is.
#' @return @seealso test
benfords.test = function(frequencies, type) {
	cho.gains = list(frequencies=frequencies, critical.values=c(1.212, 1.330, 1.569), name="Cho-Gains's d", test=max)
	leemis    = list(frequencies=frequencies, critical.values=c(0.851, 0.967, 1.212), name="Leemis's m", test=function(x) {sqrt(sum(x^2))})
	target    = if (type == 'd') cho.gains else leemis
	return(do.call(test, target))
}

#' Performs both Cho-Gains's d test and Leemis's m test for Benford's law,
#' formats the output nicely, and prints or returns that data, along with a key.
#' @param frequencies A vector, list, or matrix of digit frequencies
#' @param echo Whether or not the function should print the data as well.
#' @return A dataframe containing the results of the two tests
print.benfords = function(frequencies, echo=TRUE) {
	# Make a datafram for easy printing.
	df = rbind.data.frame(benfords.test(frequencies, "m"), benfords.test(frequencies, "d") , make.row.names = FALSE)
	legend = cbind(Alphas=c("0.10", "0.05", "0.01"), Stars=1:3)
	if (echo) {
		print(df, right=FALSE)
		print(legend, right=FALSE)
		return(invisible(df))
	}
	return(df)
}
#' Outputs the results of \code{print.benfords} to a .csv file.
#' @param frequencies A vector, list, or matrix of digit frequencies
#' @param file The path of the .csv file to output.
#' @return None
#' @seealso print.benfords
write.benfords = function(frequencies, file) {
	sink("/dev/null") # Sinking to /dev/null supressess output
	results = print.benfords(frequencies, FALSE)
	sink()
	write.csv(results, file)
}

foo = c(0.25, 0.25, 0, 0, 0, 0, 0, 0.25, 0.25)
print.benfords(foo)#, "x.csv")
