as yet unclassified
send: aSelector with: anArrayOfParameters to: anObject
	^ self forMessageSend: (MessageSend receiver: anObject selector: aSelector arguments: anArrayOfParameters)