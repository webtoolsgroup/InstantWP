menu - columns
jobMenuColumn: column row: rowIndex
	| job menu |
	job := (self elementAt: rowIndex) job.
	job = 'NonJobRelated'
		ifFalse: [ 
			menu := UIManager default newMenuIn: self table for: self.
			menu add: 'Inspect job' target: job selector: #inspect.
			^ menu ].
	^ nil