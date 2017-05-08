accessing - columns
nameColumn: column row: rowIndex
	^ FTCellMorph new  
		color: (self colorFor: rowIndex);
		cellInset: 20;
		addMorphBack: (self iconFor: rowIndex) asMorph;
		addMorphBack: (self elementAt: rowIndex) name asStringMorph;
		yourself