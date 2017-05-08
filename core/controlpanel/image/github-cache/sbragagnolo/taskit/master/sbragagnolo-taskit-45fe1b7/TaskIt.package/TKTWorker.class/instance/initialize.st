initialization
initialize
	super initialize.
	process := TKTWorkerProcess new
		taskQueue: queue;
		yourself.
	self name: ('Worker-{ID}' format: {(#ID -> UUID new)} asDictionary).
	WeakRegistry default add: self