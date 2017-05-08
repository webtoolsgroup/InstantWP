testing
isRelatedWith: anObject
	^ messageSend ifNotNil: [ messageSend receiver = anObject ] ifNil: [ false ]