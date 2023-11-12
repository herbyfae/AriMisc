\name{Aggregate}
\alias{Aggregate}
\title{
Aggregate with multiple basic summary statistics at once
}
\description{
Given a factor to separate directly or a numeric vector to break up, returns summary measures for each column of each partition of the data. Added support for "by"s with NAs and options for categorical/factors and binary data.
}
\usage{
Aggregate(data,
          by = NULL,
          quantiles = NULL,
          breaks.num = NULL,
          measures = "core",
          multicat = NULL,
          multicat.perIndiv = T
)
}

\arguments{
  \item{data}{
Pretty self explanatory. Unless you call "by" by name, exclude the vector from here. If it's named, exact matches for the column name will be automatically removed.
}
  \item{by}{
Vector to separate the data with. Factors/character vectors don't require anything else, numeric ones do. Can have NAs, these will simply form a separate subset.
}
  \item{quantiles}{
Quantiles used to separate a numeric by. Defaults to quartiles.
Not to confuse with getting summary quantiles, that's in measures.
}
  \item{breaks.num}{
Same rationale, just giving it specific numbers.
}
  \item{measures}{
What summary statistics should be given.
\bold{Capitalization isn't relevant, just here to make it easier to read}

\cr

\bold{Numeric columns:}

"core" computes the mean, quartiles, and, when present a tally of the column's NAs;

"mean" and "median" can be given individually;

"iqr" gives you the interquantile range;

"ext" gives you the extrema, "min" and "max" can be used seperatly too since it had to be divided anyways for looks.

"na" forces every initial column to return a tally of their NAs(The rationale stays for character/factor columns);

"relev_na" only prints NAs when the column actually has them;

Quantiles can be specified, just keep it consistent from 0-1 or 0-100(if there's any value over 1, it will divide them all by 100)

\cr

\bold{Binary/string columns:}

"cat.k" specifies a k number of a string variable's most common elements to be outputted(can be 0). Defaults to 3. "cat.all" prints every element of every column.

To this you could also add "_perBy", "_fromTotal" or "_rawQuant". "_perBy" is the default, displaying the percentages of each category per each level of by. "_fromTotal" computes the percentages of each category in the dataset per each level of by. So if a column is 33\% "a", and all of these are in a level of by, "_perBy" will give you 100\% on that level, while "_fromTotal" would give you 33\%. "_rawQuant" simply gives you the count.
\bold{You don't need to set a k to set the type, eg. cat_fromTotal works}

Mode is on by default for non binary categorical variables. For now anything with an isolated "mode" deactivates it, so use "mode.off" if its more intuitive(yes "." passes through "\\\b" checks, don't ask me why when "_" doesn't). For binary variables, it's off by default. "bin_mode" activates it if you need it for some reason.

\cr

\bold{Example:}

measures = c("core", 0.1, 0.9, "na", "cat.5_rawQuant", "bin_mode") would give you the mean, 10,25,50,75 and 90\% quantiles for every numeric column. Binary columns would present the percentage of the most common option and the mode for each subset of the data. Factors/categorical variables would be limited to their 5 most common values, giving the counts of each, along with their mode. NAs would be returned for every column

}
\item{multistr}{

\bold{Spoilers}

specific options for columns with multiple strings per cell. Obeys categorical settings(to be changeable in the future). Can be introduced two ways:

\cr

If the split is always the same(every string in every relevant column is always separated by "_" for example), you can just enter c(index or colnames, separation).

If different columns have different splits, please enter a list with the same format, eg. list(c(3,"_"), c(2,4, ","), c(3,6, "\\<BR\\>"))

Separators should be in Regex(\link{https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html}
}
\item{multicat.perIndiv}{
Should the tally be divided by the number of individuals or the number of total strings. Defaults to true. Doesn't affect _rawQuant. Specific to the others eg. divided by the total number of strings in the subset or globally.
}

}

\examples{

data("iris")
iris$Sepal.Length[c(49, 101, 113, 57)] = NA
iris$Species[c(1,2,54)] = NA

summary(iris)

# By a factor
Aggregate(iris, by = iris$Species, measures = "core")

# Naming by and specifying measures
Aggregate(iris, by = "Species", measures = c("median", 5, 95, "iqr"))

# Default numeric by
Aggregate(iris[,-1], by = iris$Sepal.Length, measures = c("median", "iqr", "relev_na"))

# Messing with by's quantiles + fromTotal
Aggregate(iris[,-2], by = iris$Sepal.Width, quantiles = c(.35,.65), measures = c("mean", "iqr", "cat_fromTotal"))

# By a specific break + specific case of core + na forcing every column vs only when relevant
hist(iris$Petal.Length)
Aggregate(iris[,-3], by = iris$Petal.Length, breaks.num = c(2), measures = c("core", "na"))

# Messing with the categorical/binary commands

set.seed(1)
iris$bin = sample(c(0,1), prob = c(0.7, 0.3), size = 150, replace = T)
# Preferable to set any binaries as character/factor
# (as a workaround, anything with only two values is interpreted as binary, numeric or not)

hist(iris$Petal.Width)

## Minimum activated by default
Aggregate(iris[,-4], by = iris$Petal.Width, breaks.num = c(0.75), measures = "")

## Absolute minimum extractable
Aggregate(iris[,-4], by = iris$Petal.Width, breaks.num = c(0.75), measures = c("mode.off", "cat.0"))

## Forcing mode for binary variables + setting cat_k
Aggregate(iris[,-4], by = iris$Petal.Width, breaks.num = c(0.75), measures = c("bin_mode", "cat.50"))
# cat_k can only be set to one number. This acts as a maximum
# (Any more than the available options for any column will simply be cut)

### Example with multiple categories with different lengths

set.seed(2)
iris$cat1 = sample(c("test_1","test_2","test_3","test_4","test_5"), size = 150, replace = T)

set.seed(3)
iris$cat2 = sample(c("test_1","test_2","test_3","test_4","test_5","test_6", "test_7"),
                   size = 150, replace = T)

Aggregate(iris[,-4], by = iris$Petal.Width, breaks.num = c(0.75),
          measures = c("bin_mode", "cat.5"))
# Also a solid example of how they get cut
# cat2 loses test 3 and 6

table(iris$cat2)[order(table(iris$cat2))]
# As you can see these are the least common globally
## Also visible by:

Aggregate(iris[,-c(4:5)], by = iris$Petal.Width, breaks.num = c(0.75),
          measures = c("cat.all_rawQuant"))
}
