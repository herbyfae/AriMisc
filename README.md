## Version 0.8.4 - The semi-professional semi-update

Most of what I'm introducing right now is backend, so not much is changing outside of trying to make a habit of 
making a changelog here.

- Made missinglevel and NumToFact more robust and updated the documentation to be clearer;
- Added documentation for RankDiv as a teaser;
- Cleaned up the README.

Most of the way there but I have to get this "released" today:

- Adding specific support for dates on Summary and Aggregate;
- Ending the tag system in Summary and Aggregate in favour of separating measures by data type;
- Adding an "f" tag to how you break up "by"s in Aggregate, along with the option to partition categorical "by"s. 
This is essentially a "full" tag, so a break at 50% would create a group with every element of by <50%, no matter
what other tags were before it(The point of this is to allow effectively duplicates).
- Adding RankDiv, a function that orders and cuts up a vector by size, explained in the new .Rd.
- Revamped the settings for categorical columns in Summary and Aggregate to make them clearer and align them better with 
my time experimenting with PBI;
- Aggregate and Summary's documentation is being changed to be easier to understand
(Only not here already as the new settings might lead to confusion);
- Revamping the code of every function to be clearer, along with adding some commentary.

Eventually but low priority:

- Adding a formal testing backend.


# Museum/Lab for all the things that seemed kind of like good ideas at the time

**Summary** pretty much only restructures the base summary to give you a little more control. 
Just an uglier version of tbl_summary with the option to cap the number of categories displayed;

**Aggregate** is the star of the show. Just a robust early visualization tool. 
Allows you to mess around with numeric "by"s via breaks or quantiles, and break up multistring columns natively,
just with a little jank if you're using multiple instances of regex per column;

**NumToFact** rips off the loops used to break up numeric "by"s. 
Can break up more than one column at once and return them back into place if the full data is given;

**missinglevel** compares two vectors, giving you a list of matches, misses and their index in either vector;

**Poisson** systematically applies poisson/quasipoisson and negative binomial regressions to each column you give it.

### **Use with caution**

**RegViz** and **MatrixViz** apply the old aggregation loops to model predictions. 
These exist merely as visualization tools to look for factors that you might want to account for too.
These are best treated as exhibits and not serious tools.
