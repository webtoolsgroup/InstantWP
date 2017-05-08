tests
testFutureExecutesByDefaultInGlobalWorkerPool

	| future |

	future := [ TKTCurrentTaskRunner value ] future.

	self assert: (future synchronizeTimeout: 1 second) equals: TKTWorkerPool global.