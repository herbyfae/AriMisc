\name{Summary}
\alias{Summary}

\title{
Categorical/factor inclusive summary
}
\description{
Just ripped code from Aggregate to use it without a "by".
}
\usage{
Summary(data, measures = "core")
}
\arguments{
  \item{data}{
Self explanatory
}
  \item{measures}{
Same deal as with Aggregate.

What summary statistics should be given.
\bold{Capitalization isn't relevant, just here to make it easier to read}

\cr

\bold{Numeric columns:}

"core" computes the mean, quartiles, and, when present a tally of the column's NAs;

"mean" and "median" can be given individually;

"iqr" gives you the interquantile range;

"na" forces every initial column to return a tally of their NAs(The rationale stays for character/factor columns);

"relev_na" only prints NAs when the column actually has them(NA = 0 in that row, just a light optimization at most);

Quantiles can be specified, just keep it consistent from 0-1 or 0-100(if there's any value over 1, it will divide them all by 100)

\cr

\bold{Binary/string columns:}

"cat.k" specifies a k number of a string variable's most common elements to be outputted(can be 0). Defaults to 3. "cat.all" prints every element of every column.

To this you could also add "_fromTotal" or "_rawQuant". "_rawQuant" is the default, giving you the count. "_fromTotal" computes the percentages of each category in the dataset.
\bold{You don't need to set a k to set the type, eg. cat_fromTotal works}

Mode has been removed, cells will always display the relevant name and percentage/count.

\cr

\bold{Example:}

measures = c("core", 0.1, 0.9, "na", "cat.5_rawQuant") would give you the mean, 10,25,50,75 and 90\% quantiles for every numeric column. Binary columns would present the percentage of the most common option and the mode for each subset of the data. Factors/categorical variables would be limited to their 5 most common values, giving the counts of each, along with their mode. NAs would be returned for every column

}
}

\examples{

data("iris")
iris$Sepal.Length[c(49, 101, 113, 57)] = NA
iris$Species[c(1,2,54)] = NA

summary(iris)

# ~ Equivalent to summary
Summary(iris, measures = "core")

# Unless(deals with categorical columns just the same)
 ## Still no multistring support though(WIP)

iris$Species = as.character(iris$Species)
summary(iris)
Summary(iris)

# Specifying measures + fromTotal
Summary(iris, measures = c("median", 5, 95, "iqr", "cat_fromTotal", "na"))

# Messing with the categorical/binary commands

set.seed(1)
iris$bin = sample(c(0,1), prob = c(0.7, 0.3), size = 150, replace = T)
# Preferable to set any binaries as character/factor
# (as a workaround, anything with only two values is interpreted as binary, numeric or not)

## Absolute minimum extractable
Summary(iris, measures = "cat.0")

## Setting cat_k
Summary(iris[,-c(1:4)], measures = c("cat.50"))
# cat_k can only be set to one number. This acts as a maximum
# (Any more than the available options for any column will simply be cut)

### Example with multiple categories with different lengths

set.seed(2)
iris$cat1 = sample(c("test_1","test_2","test_3","test_4","test_5"), size = 150, replace = T)

set.seed(3)
iris$cat2 = sample(c("test_1","test_2","test_3","test_4","test_5","test_6", "test_7"),
                   size = 150, replace = T)

Summary(iris[,-c(1:4)], measures = c("cat.5"))
# Unfortunately also demonstrates a problem with row generation
 # If not all of the rows for a type of data aren't generated at once, they'll end up intersecting one another

# Also a solid example of how they get cut
# cat2 loses test 3 and 6

table(iris$cat2)[order(table(iris$cat2))]
# As you can see these are the least common globally
## Also visible by:

Summary(iris[,-c(1:4)], measures = c("cat.all_rawQuant"))

}

