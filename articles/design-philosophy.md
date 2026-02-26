# Design Philosophy

**urbnindicators** makes a number of opinionated design choices about
what data to select from the Census Bureau API, how to process it, what
relevant derived variables to calculate, and even which types of
geographies to support.

“Opinionated” doesn’t mean that these decisions are the best ones for
every user or use-case, but these decisions are designed to either speed
or improve the accuracy of a common use-case involving a large set of
variables (optionally over multiple years).

## Design choices

- **Support geographies from the tract level and up**. Block groups are
  not supported because the margins of error for block group-level
  estimates are often so large as to make the estimates meaningless.
  Further, many estimates available from the tract level and up are not
  available for block groups.

- **Support five-year estimates only.** One-year estimates bring margin
  of error challenges, even for relatively larger-population
  geographies, such as tracts, zip codes, and some places and counties.

- **Support only a subset of ACS variables.** Pre-calculated ACS
  estimates cover tens of thousands of different variables. But, in our
  work, only a small fraction of these is used frequently. We’ve tried
  to select those common variables to return by default, cognizant that
  at present, every additional variable returned results in a slower
  query. Open an issue in GitHub if you’d like to see additional
  variables added to the default set.

- **Rename all variables.** The default variable names returned by the
  API are not human-friendly. Not only is it challenging to determine
  what a given variable represents when you’re looking at a name like
  `B01001_001E`, but when you’re looking at a dozen or a hundred such
  variables, it’s very easy to accidentally misinterpret or mis-select
  the variable(s) you want. For these reasons, we apply more meaningful
  names to every returned variable while retaining consistency of
  variable names from within the same table so that it’s easy to select
  and operate on sets of interrelated variables. The downside of this
  approach is that the default API variable names are used in other
  publications, and that you will find no documentation anywhere (apart
  from the codebook returned by this package!) of a variable named, for
  example, `race_personofcolor_percent`. Variables in the codebook have
  their original API names included in their definitions.
