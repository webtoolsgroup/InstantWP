accessing
systemProcessesTable
	^ systable
		ifNil: [ systemDataSource := TKTSystemProcessDataSource new.
			systable := Tab
				label: 'System Processes'
				icon: Smalltalk ui icons smallHelp
				retrievingBlock: [ FTTableMorph new
						extent: 650 @ 500;
						addColumn: ((FTColumn id: '#') width: 20);
						addColumn: ((FTColumn id: 'Priority') width: 50);
						addColumn: ((FTColumn id: 'Name') width: 600);
						addColumn: ((FTColumn id: 'State') width: 40);
						intercellSpacing: 1;
						dataSource: systemDataSource;
						selectRowIndex: 1;
						showFirstRowSelection;
						beMultipleSelection;
						enableFilter;
						yourself ] ]