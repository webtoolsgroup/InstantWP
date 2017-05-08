accessing
name
	^ name
		ifNil: [ name := 'Process: '
				,
					(messageSend
						ifNil: [ '' ]
						ifNotNil: [ '#' , messageSend selector , ' -> ' , messageSend receiver printString  ]) ]