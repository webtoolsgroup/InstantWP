as yet unclassified
exampleProcessTable
	"Show a table with all Object methods, with a header"
	<example>
	| table |

	table := FTTableMorph new
		extent: 650@500;
		addColumn: ((FTColumn id: '#') width: 40);
		addColumn: ((FTColumn id: 'Name') width: 350);
		addColumn: (FTColumn id: 'Sending');
		addColumn: (FTColumn id: 'To');
		addColumn: (FTColumn id: 'With');
		addColumn: (FTColumn id: 'State');
		addColumn: (FTColumn id: 'Related Task execution');
		addColumn: (FTColumn id: 'Related Job');
		intercellSpacing: 1;
		dataSource: (TKTProcessDataSource new);
		selectRowIndex: 1;
		showFirstRowSelection;
		onAnnouncement: FTSelectionChanged 
			do: [ :ann | ('rows selected: ', (ann newSelectedRowIndexes asString)) crLog ];
		onAnnouncement: FTStrongSelectionChanged 
			do: [ :ann | ('double-click on row: ', (ann selectedRowIndex asString)) crLog ];
		beMultipleSelection;
		yourself.
		
	^ table openInWindow