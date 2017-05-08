menu - columns
nameMenuColumn: column row: rowIndex
	| process menu |
	process := (self elementAt: rowIndex) model.
	process ifNil: [ ^ nil ].
	menu := UIManager default newMenuIn: self table for: self.
	menu add: 'Inspect process' target: process selector: #inspect.
	process isSuspended
		ifTrue: [ menu add: 'Resume process' target: process selector: #resume ]
		ifFalse: [ menu add: 'Suspend process' target: process selector: #suspend ].
	process ifRunning: [ menu add: 'Cancel process' target: process selector: #cancel ].

	^ menu