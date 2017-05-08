global
global

	^ GlobalInstance ifNil: [
		GlobalInstance := self new
			name: 'Global TaskIT Worker Pool';
			poolMaxSize: 4;
			start ]