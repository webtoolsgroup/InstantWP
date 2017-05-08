setup
setUp
	| messageSend |
	TKTProcess uniqueAnnouncer when: TKTProcessStarted send: #processStarted: to: self.
	TKTProcess uniqueAnnouncer when: TKTProcessFinishing send: #processFinishing: to: self.
	dummyObject := Dictionary new.
	messageSend := MessageSend
		receiver: dummyObject
		selector: #at:put: 
		arguments:
			{'key'.
			'value'}.
	process := TKTProcess forMessageSend: messageSend