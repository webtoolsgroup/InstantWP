annotations
annotation: aTag
	^ self processAnnotation at: aTag ifAbsent: [ nil ]