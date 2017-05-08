instance-creation
valuable: aValuable timeout: aTimeout

	^ TKTTimeoutableTask new
		valuable: aValuable;
		timeout: aTimeout;
		yourself