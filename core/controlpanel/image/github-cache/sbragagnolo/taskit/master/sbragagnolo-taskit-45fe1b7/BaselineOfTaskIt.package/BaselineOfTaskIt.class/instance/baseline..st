baseline
baseline: spec
	<baseline>
	spec
		for: #common
		do: [ spec
				package: #TaskIt;
				package: #'TaskIt-Tests' with: [ spec requires: #('TaskIt') ];
				package: #TaskItServices with: [ spec requires: #('TaskIt') ];
				package: #'TaskItServices-Tests' with: [ spec requires: #('TaskItServices') ];
				package: #TaskItProcesses with: [ spec requires: #('TaskIt') ];
				package: #'TaskItProcesses-Tests' with: [ spec requires: #('TaskItProcesses') ];
				package: #TaskItBrowser with: [ spec requires: #('TaskItProcesses') ].
			spec group: 'minimal' with: #(#TaskIt) ]