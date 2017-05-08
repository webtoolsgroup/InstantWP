versions
v0_1_0: spec
	<version: 'v0.1.0'>
	
	spec
		for: #'common'
		do: [ 
			spec
				baseline: 'FFICHeaderExtractor'
					with: [ spec repository: 'github://marianopeck/FFICHeaderExtractor:v0.1.0/repository' ];
				import: 'FFICHeaderExtractor' ].