as yet unclassified
testProcessFinishingAndStartedAnnouncementsContainsTheSameProcess
	process run.
	
	process cancel.
	self assert: processFinishing equals: processStarted.