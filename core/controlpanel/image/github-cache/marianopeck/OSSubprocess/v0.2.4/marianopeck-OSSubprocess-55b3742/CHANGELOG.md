# Change Log

## [Unreleased](https://github.com/marianopeck/OSSubprocess/tree/HEAD)

[Full Changelog](https://github.com/marianopeck/OSSubprocess/compare/v0.2.4...HEAD)

**Implemented enhancements:**

**Closed issues:**

**Merged pull requests:**

**Fixed bugs:**

**Documentation updates**



## [v0.2.4](https://github.com/marianopeck/OSSubprocess/tree/v0.2.4) (2016-05-30)

[Full Changelog](https://github.com/marianopeck/OSSubprocess/compare/v0.2.3...v0.2.4)

**Implemented enhancements:**

- Migrate from our custom Travis CI scripts to [smalltalk-ci](https://github.com/hpi-swa/smalltalkCI)
**Closed issues:**

- Childs processes never finished (Pharo delaySchedulerClass bug) [NOW FOR REAL] [\#19](https://github.com/marianopeck/OSSubprocess/issues/19)



## [v0.2.3](https://github.com/marianopeck/OSSubprocess/tree/v0.2.3) (2016-05-30)

[Full Changelog](https://github.com/marianopeck/OSSubprocess/compare/v0.2.2...v0.2.3)

**Implemented enhancements:**

- Rename `pwd:` to `workingDirectory:` [\#18](https://github.com/marianopeck/OSSubprocess/issues/18)

**Closed issues:**

- Childs processes never finished (Pharo delaySchedulerClass bug) [\#19](https://github.com/marianopeck/OSSubprocess/issues/19)


## [v0.2.2](https://github.com/marianopeck/OSSubprocess/tree/v0.2.2) (2016-05-07)

[Full Changelog](https://github.com/marianopeck/OSSubprocess/compare/v0.2.1...v0.2.2)

**Closed issues:**

- Problem trying to terminate already terminated childWatcher on system startup [\#17](https://github.com/marianopeck/OSSubprocess/issues/17)

**Fixed bugs:**

- Do not use "v" as part of the Metacello version string so that this project can be used as a dependency via Versionner until the bug is fixed in Metacello [\#392](https://github.com/dalehenrich/metacello-work/issues/392)


## [v0.2.1](https://github.com/marianopeck/OSSubprocess/tree/v0.2.1) (2016-01-31)

[Full Changelog](https://github.com/marianopeck/OSSubprocess/compare/v0.2.0...v0.2.1)

**Implemented enhancements:**

- Added Travis CI integration
- Added dependency to [FFICHeaderExtractor](https://github.com/marianopeck/FFICHeaderExtractor), needed by issue [\#15](https://github.com/marianopeck/OSSubprocess/issues/15)
- Update to new `registerToolClassNamed:` mechanisim rather than `addToStartUpList:` and `addToShutDownList:` as for Pharo 50558.

**Closed issues:**

- Use FFICHeaderExtractor to minimize usage of OSProcess [\#15](https://github.com/marianopeck/OSSubprocess/issues/15)
- `upToEnd` may fail when child process is writing [\#16](https://github.com/marianopeck/OSSubprocess/issues/16)

**Merged pull requests:**

- Typos in comments and a method name [\#14](https://github.com/marianopeck/OSSubprocess/pull/14) ([cdlm](https://github.com/cdlm))

**Fixed bugs:**

- Fix random test failures that used `fork`.

**Documentation updates**

- Re-organization of the main README
- Added section for [Future Work](https://github.com/marianopeck/OSSubprocess#future-work)
- Added section for [Running Tests](https://github.com/marianopeck/OSSubprocess#running-the-tests)


## [v0.2.0](https://github.com/marianopeck/OSSubprocess/tree/v0.2.0) (2016-01-19)


**Implemented enhancements:**

- Improve `#bashCommand` to rely on $SHELL if defined  [\#13](https://github.com/marianopeck/OSSubprocess/issues/13)
- Add OS signal sending to process (`sigterm`, `sigkill`, etc) [\#4](https://github.com/marianopeck/OSSubprocess/issues/4)
- Added API for processing streams while process is running (`#runAndWaitPollingEvery:doing:onExitDo:`)
- Added option `#terminateOnShutdown` to terminate running processes on Pharo shutdown
- Move creation of temp files to class side

**Fixed bugs:**

- VM Crash when forking infinitive process and image restart (added new `#stopWaiting` called from `#shutDown:`)  [\#12](https://github.com/marianopeck/OSSubprocess/issues/12)

**Closed issues:**

- Double check `ExternalAddress allocate`  and `free` [\#9](https://github.com/marianopeck/OSSubprocess/issues/9)

**Merged pull requests:**

- typos, small edits in first 200 lines [\#1](https://github.com/marianopeck/OSSubprocess/pull/1) ([StephanEggermont](https://github.com/StephanEggermont))

**Documentation updates**

- Better explanation of synchronism vs asynchronous
- Add a section specially for asynchronous with a `tail -f` example
- Add new doc for all new features and enchacements
- Added a ChangeLog file to doc.

[Full Changelog](https://github.com/marianopeck/OSSubprocess/compare/v0.1.4...v0.2.0)


## [v0.1.4](https://github.com/marianopeck/OSSubprocess/tree/v0.1.4) (2016-01-14)
First milestone release.
