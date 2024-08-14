# Museum/Lab for all the things I was too lazy to look up
### Currently home to 4 decent functions and 2 examples of peak brainrot

**Summary** pretty much only restructures the base summary to give you a little more control. Just an uglier version of tbl_summary with the option to cap the number of categories displayed;

**Aggregate** is the star of the show. Just a robust early visualization tool. Allows you to mess around with numeric "by"s via breaks or quantiles, and break up multistring columns natively, just with a little jank if you're using multiple instances of regex per column;

**NumToFact** rips off the loops used to break up numeric "by"s. Can break up more than one column at once and return them back into place if the full data is given;

**missinglevel** compares two vectors, giving you a list of matches, misses and their index in either vector;

**Poisson** systematically applies poisson/quasipoisson and negative binomial regressions to each column you give it.

### **Superhell(derogatory):**

**RegViz** and **MatrixViz** apply the old aggregation loops to model predictions. These exist merely as visualization tools to look for factors that you might want to account for too. These are not robust in the slightest, so caution is advised if you want to use them for some reason
