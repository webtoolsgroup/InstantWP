world menu
menuCommandOn: aBuilder 
	<worldMenu> 

	(aBuilder item: self title)
		parent: #Tools;
		icon: ThemeIcons current processBrowserIcon;
		order: 0.5.