accessing - columns
taskColumn: column row: rowIndex
	^ FTCellMorph new
		color: (self colorFor: rowIndex);
		addMorph: (self elementAt: rowIndex) task asStringMorph;
		yourself.
		
		



		
	
	
