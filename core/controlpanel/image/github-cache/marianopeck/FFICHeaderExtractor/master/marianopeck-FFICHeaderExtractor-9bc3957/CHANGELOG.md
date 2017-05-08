# Change Log


## [Unreleased](https://github.com/marianopeck/FFICHeaderExtractor/tree/HEAD)

[Full Changelog](https://github.com/marianopeck/FFICHeaderExtractor/compare/v0.1.0...HEAD)

**Implemented enhancements:**

- Added Travis CI integration using [Smalltalk-CI project](https://github.com/hpi-swa/smalltalkCI)

**Closed issues:**


**Merged pull requests:**


**Fixed bugs:**

- Fix a bug in `FFICHeaderExtractor >> #extractHeadersInformation` because it should NOT `#error:` if the log is not empty because it could have been warnings...Use a `#inform:` instead.

**Documentation updates**

- Added section for [Future Work](https://github.com/marianopeck/FFICHeaderExtractor#future-work)



## [v0.1.0](https://github.com/marianopeck/FFICHeaderExtractor/tree/v0.1.0) (2016-01-27)
First milestone release.
