instance-creation
reduceCollectionOfFutures: aCollectionOfFutures with: aBlock

	^ aCollectionOfFutures
		inject: (TKTFuture of: OrderedCollection new)
		into: [ :facum :feach | facum flatCollect: [ :acum | 
				feach collect: [ :each | aBlock value: acum value: each ] ] ]