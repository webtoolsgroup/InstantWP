accessing
taskItProcessesTable
	^ table
		ifNil: [ processDataSource := TKTProcessDataSource new.
			table := Tab
				label: 'TaskIT Processes'
				icon: Smalltalk ui icons smallHelpIcon
				retrievingBlock: [ FTTableMorph new
						extent: 650 @ 500;
						addColumn: ((FTColumn id: '#') width: 40);
						addColumn: ((FTColumn id: 'Name') width: 250);
						addColumn: ((FTColumn id: 'Sending') width: 150);
						addColumn: ((FTColumn id: 'To') width: 200);
						addColumn: ((FTColumn id: 'With') width: 50);
						addColumn: ((FTColumn id: 'State') width: 70);
						"						addColumn: ((FTColumn id: 'Related Task execution') width: 250);"
							"						addColumn: ((FTColumn id: 'Related Job') width: 100);"
							intercellSpacing: 1;
						dataSource: processDataSource;
						selectRowIndex: 1;
						showFirstRowSelection;
						beMultipleSelection;
						beResizable;
						enableFilter;
						yourself ] ]