initialization
initialize
	super initialize.
	self elements: self createBaseElements.
	TKTProcess uniqueAnnouncer weak when: TKTProcessStarted send: #noteProcessStarted: to: self.
	TKTProcess uniqueAnnouncer weak when: TKTProcessFinishing send: #noteProcessFinished: to: self.
	TKTProcess uniqueAnnouncer weak when: TKTProcessCancelledByThirdThread send: #noteProcessCancelled: to: self.
	TKTProcess uniqueAnnouncer weak when: TKTProcessResumed  send: #noteProcessResumed: to: self.
	TKTProcess uniqueAnnouncer weak when: TKTProcessSuspended send: #noteProcessSuspended: to: self