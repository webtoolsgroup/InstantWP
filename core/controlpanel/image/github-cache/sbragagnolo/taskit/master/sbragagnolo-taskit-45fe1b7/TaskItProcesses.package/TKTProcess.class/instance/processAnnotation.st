accessing
processAnnotation
	^ processAnnotation ifNil: [ processAnnotation := WeakValueDictionary new ]