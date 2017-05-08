tests
testFutureWithFutureInsideSchedulesOk

	| future |

	future := [ [TKTCurrentTaskRunner value] schedule ] future.

	self shouldnt: (future synchronizeTimeout: 1 second) raise: UnhandledError.