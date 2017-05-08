accessing - columns
argumentsColumn: column row: rowIndex
	^ FTCellMorph new
		color: (self colorFor: rowIndex);
		addMorph: (self elementAt: rowIndex) arguments asStringMorph;
		yourself