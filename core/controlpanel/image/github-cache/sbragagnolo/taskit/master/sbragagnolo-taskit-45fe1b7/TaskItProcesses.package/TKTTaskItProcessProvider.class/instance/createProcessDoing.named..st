instance-creation
createProcessDoing: aValuable named: aName
	^ (TKTProcess send: #value to: aValuable)
		annotate: #task with: aValuable;
		name: aName;
		run