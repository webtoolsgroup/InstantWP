private
privateCreatePharoProcess
	process := Process forContext: self createContext priority: self spawnPriority.
	process name: self name.
	^ process.