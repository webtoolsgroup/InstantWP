world menu
monitorCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: 'Process dashboard')
		parent: self title;
		icon: ThemeIcons current processBrowserIcon;
		action: [ TKTFastTableProcess new open ];
		order: 3