setup
setUp
	| messageSend |
	TKTProcess uniqueAnnouncer when: TKTProcessStarted send: #processStarted: to: self.
	TKTProcess uniqueAnnouncer when: TKTProcessFinishing send: #processFinishing: to: self.
	TKTProcess uniqueAnnouncer when: TKTProcessCancelledByThirdThread send: #processCancelled: to: self.
	TKTProcess uniqueAnnouncer when: TKTProcessSuspended  send: #processSuspended: to: self.
	TKTProcess uniqueAnnouncer when: TKTProcessResumed  send: #processResumed: to: self.
	TKTProcess uniqueAnnouncer when: TKTProcessCancelledByThirdThread send: #processCancelled: to: self.
	messageSend := MessageSend receiver: self selector: #iterateForEver.
	process := TKTProcess forMessageSend: messageSend.