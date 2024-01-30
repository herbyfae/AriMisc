\name{Aggregate}
\alias{Aggregate}
\title{
Aggregate with multiple basic summary statistics at once
}
\description{
Given a factor to separate directly or a numeric vector to break up, returns summary measures for each column of each partition of the data. Added support for "by"s with NAs and options for categorical/factors and binary data. Finally got multistring options functional, if a bit finicky if using regex.
}
\usage{
Aggregate(data,
          by = NULL,
          quantiles = NULL,
          breaks.num = NULL,
          measures = "core",
          multistr = NULL
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

"core" computes the mean, sd, quartiles, and, when present a tally of the column's NAs;

"mean" and "median" can be given individually;

"sd", "var" and "mad" give you the standard deviation, variance and median absolute deviation respectively;

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

Mode can be turned on via "mode" in either measures or multistr.

Added the option to lock binary variable display to the highest or lowest level: "bin_refhigh" and "bin_reflow".

\cr

\bold{Basic example:}

measures = c("core", 0.1, 0.9, "na", "cat.5_rawQuant", "bin_mode") would give you the mean, 10,25,50,75 and 90\% quantiles for every numeric column. Binary columns would present the percentage of the most common option and the mode for each subset of the data. Factors/categorical variables would be limited to their 5 most common values, giving the counts of each, along with their mode. NAs would be returned for every column

}
  \item{multistr}{

Separate set of settings for any string column that you want to give a division to. In no particular order you need to give the function 3 things:

The column in question, index only(of the given data -> data[,3:5] -> selecting column 4 would be done with 2). You can give multiple per set, but they can't overlap. You can duplicate the column in question to give it different settings if necessary.

The separation in question. Can be as simple as a comma or as messy as regex. You can provide multiple for the same column, they'll be interpreted as "example|example|example".

Finally, whether you want to get the count or the percentage of the strings in the subset of by. This comes attached with the number of strings you want to display, with descending prevalence globally. This becomes k_count/perc.

This can be presented as one vector or as a list of settings if you want to use different ones for multiple columns. So, for example, multistr = c(2, ",", "all_perc") would give you the percentage of all substrings divided by commas in the second column. This will be the percentage of items present in the subset of by.

list(c(1:4, ",", "_", "5_count"), c(6, "[\\\(\\\)]", "all_perc")) would give you the count of the 5 most prevalent substrings in the first four columns separated by commas or "_"s. Additionally, column 6 would effectively remove "()"s, eg a(b) would be returned as "a" and "b". All substrings would be shown, with their percentages.

}

}

\examples{

# Does it still perform on the old examples?
data("iris")
iris$Sepal.Length[c(49, 101, 113, 57)] = NA
iris$Species[c(1,2,54)] = NA

summary(iris)

# By a factor
Aggregate(iris, by = iris$Species, measures = "core")

# Naming by and specifying measures
Aggregate(iris, by = "Species", measures = c("median", 5, 95, "iqr"))

# Default numeric by
 # Mode isn't on by default anymore
Aggregate(iris[,-1], by = iris$Sepal.Length, measures = c("median", "iqr", "relev_na", "mode"))

# Messing with by's quantiles + count(percentage of the subset by default)
Aggregate(iris[,-2], by = iris$Sepal.Width, quantiles = c(.35,.65), measures = c("mean", "iqr", "cat_count"))

# By a specific break + specific case of core + na forcing every column vs only when relevant
hist(iris$Petal.Length)
Aggregate(iris[,-3], by = iris$Petal.Length, breaks.num = c(2), measures = c("core", "na"))

# Messing with the categorical/binary commands

set.seed(1)
iris$bin = sample(c(0,1), prob = c(0.7, 0.3), size = 150, replace = T)
# Preferable to set any binaries as character/factor
# (as a workaround, anything with only two values is interpreted as binary, numeric or not)

hist(iris$Petal.Width)

## Minimum activated by default(3 categories + relevant NAs)
Aggregate(iris[,-4], by = iris$Petal.Width, breaks.num = c(0.75), measures = "")

## Absolute minimum extractable(relevant NAs and binary percentages)
Aggregate(iris[,-4], by = iris$Petal.Width, breaks.num = c(0.75), measures = c("cat.0"))

## Setting cat.k
Aggregate(iris[,-4], by = iris$Petal.Width, breaks.num = c(0.75), measures = c("mode", "cat.2"))
Aggregate(iris[,-4], by = iris$Petal.Width, breaks.num = c(0.75), measures = c("mode", "cat.50_count"))
# cat_k can only be set to one number. This acts as a maximum
# (Any more than the available options for any column will simply be cut)

### Example with multiple categories with different lengths

set.seed(2)
iris$cat1 = sample(c("test_1","test_2","test_3","test_4","test_5"), size = 150, replace = T)

set.seed(3)
iris$cat2 = sample(c("test_1","test_2","test_3","test_4","test_5","test_6", "test_7"),
                   size = 150, replace = T)

Aggregate(iris[,-4], by = iris$Petal.Width, breaks.num = c(0.75),
          measures = c("mode", "cat.5_count"))
# Also a solid example of how they get cut
# cat2 loses test 3 and 6

table(iris$cat2)[order(table(iris$cat2))]
# As you can see these are the least common globally
## Also visible by:

# Yes this can be used as a summary too while I'm updating the other
 # Just create an univarite factor as long as your dataset
Aggregate(iris[,-c(4:5)], by = as.factor(rep(1,150)),
          measures = c("core","cat.all_count"))

# Multistr support?
# TL;DR from testing: basic separations seem ok, regex is finnicky as hell

## Since I already have a "_" separation in the generated cats:

# Taken as categories
Aggregate(iris[,7:8], by = as.factor(iris$bin))
# Multistr settings
Aggregate(iris[,7:8], by = as.factor(iris$bin), multistr = c(1,2, "_", "7_count"))

# Let's stress test this a bit shall we
 # Can't repeat columns but you can give different settings for different columns

iris$stress = sample(c("21_5,7;2", "32:4_43", "71a91"), 150, replace = T)
iris$stress2 = sample(c("a(b)", "r[31]", "p <BR> O"), 150, replace = T)

# Basic separations + ability to set multiple for a column
Aggregate(iris[, 8:9],
          by = as.factor(iris$bin),
          multistr = list(c("_", 1, "4_count"),
                          c(2, "_", ",", ";", "a", ":", "all_count")))

# Requiring straight up regex
 # Duplicated to see better what's happening

# Interacting expressions make this a mess, so try to keep divisions
 # simple and not too varied

# eg:
trimws(strsplit("p <BR> O", "<\\<BR\\>>")[[1]])
trimws(strsplit("a(b)", "[\\(\\)]")[[1]])
trimws(strsplit("r[31]", "[]|[<>]")[[1]]) # Got this one by accident

# If I join them all, it looks fine here
 # Outside of the spaces but that's dealt with with a for loop
strsplit(c("a(b)", "r[31]", "p <BR> O"), "[]|[<>]|[\\(\\)]|<\\<BR\\>>")

# Give it to the function and BR appears
 # Kinda weird considering I just applied the exact same separation
 # Same order too if that's supposed to matter...
Aggregate(
  cbind(iris$stress2, iris$stress2, iris$stress2),
  by = as.factor(iris$bin),
  measures = "cat_count",
  multistr = list(c("[]", "[<>]", "[\\(\\)]", "<\\<BR\\>>" , 2, "all_count"),
                  c("[]|[<>]", 3, "all_count"))
)

# Compilation of the other examples

Aggregate(
  iris,
  by = as.factor(iris$bin),
  measures = c("cat_count", "core"),
  multistr = list(c("_", 7:8, "3_count"),
                  c(9, "_", ",", ";", "a", ":", "all_count"),
                  c("[]", "[<>]", "[\\(\\)]", "<\\<BR\\>>" , 10, "all_count"))
)
}
