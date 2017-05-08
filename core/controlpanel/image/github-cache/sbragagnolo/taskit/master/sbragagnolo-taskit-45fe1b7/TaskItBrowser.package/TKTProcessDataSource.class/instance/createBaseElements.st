initialization
createBaseElements
	^ (TKTProcess allInstances collect: [ :process | TKTProcessModel on: process ]) asOrderedCollection	",
			((Process allInstances reject: #isTerminated) collect: [ :process | TKTSystemProcessModel on: process ])
				asOrderedCollection"