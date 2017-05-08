starting
doStop

	self tearDown.
	(TKTNewProcessTaskRunner new future: [ self kill ])
		flatCollect: [ :r | stopCallbacks do: [ :each | each value: r ] ]