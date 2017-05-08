menu - columns
receiverMenuColumn: column row: rowIndex
	| receiver menu |
	receiver := (self elementAt: rowIndex) receiver.
	menu := UIManager default newMenuIn: self table for: self.
	menu add: 'Inspect receiver' target: receiver selector: #inspect.
	^ menu