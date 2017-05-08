instance-creation
fromCollectionOfFutures: aCollectionOfFutures

	^ self
		reduceCollectionOfFutures: aCollectionOfFutures
		with: [ :acum :each | acum add: each. acum ] 