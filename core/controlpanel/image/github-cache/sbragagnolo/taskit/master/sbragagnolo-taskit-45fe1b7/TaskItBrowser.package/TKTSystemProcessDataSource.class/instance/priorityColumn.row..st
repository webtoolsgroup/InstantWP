accessing - columns
priorityColumn: column row: rowIndex
	^ FTCellMorph new
		color: (self colorFor: rowIndex);
		addMorph: (self elementAt: rowIndex) priority asStringMorph;
		yourself