accessing
indexString
	^ '{name}:{selector}:{receiver}:{state}'
		format:
			{(#name -> self name ).
			(#selector -> self selector asString).
			(#receiver -> self receiver asString).
			(#state -> self state asString)} asDictionary