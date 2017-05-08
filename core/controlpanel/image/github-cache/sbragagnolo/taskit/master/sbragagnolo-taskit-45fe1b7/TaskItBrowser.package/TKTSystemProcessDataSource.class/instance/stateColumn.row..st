accessing - columns
stateColumn: column row: rowIndex
	^ FTCellMorph new
		color: (self colorFor: rowIndex);
		addMorph: (self elementAt: rowIndex) state asStringMorph;
		yourself.
		
	
		
	
	
