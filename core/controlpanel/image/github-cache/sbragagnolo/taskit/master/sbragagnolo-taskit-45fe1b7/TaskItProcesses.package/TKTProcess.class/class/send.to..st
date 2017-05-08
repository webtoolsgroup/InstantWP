as yet unclassified
send: aSelector to: anObject
	^ self forMessageSend: (MessageSend receiver: anObject selector: aSelector)