menu - columns
taskMenuColumn: column row: rowIndex
	| taskExecution menu |
	taskExecution := (self elementAt: rowIndex) task.
	taskExecution = 'NonTaskRelated'
		ifFalse: [ 
			menu := UIManager default newMenuIn: self table for: self.
			menu add: 'Inspect task execution' target: taskExecution selector: #inspect.
			^ menu ].
	^ nil