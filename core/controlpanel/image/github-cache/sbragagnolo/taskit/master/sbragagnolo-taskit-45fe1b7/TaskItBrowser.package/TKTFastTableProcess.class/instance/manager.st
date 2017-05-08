instance creation
manager
	^ manager
		ifNil: [ 
			manager := TabManager new
				when: #tabManagerDeleted send: #delete to: self;
				yourself ]