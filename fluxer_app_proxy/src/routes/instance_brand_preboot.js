(() => {
	try {
		const style = document.getElementById('fluxer-instance-theme-color');
		const saved = JSON.parse(localStorage.getItem('fluxer:instance-brand:preboot') || 'null');
		if (!style || !saved || saved.c !== style.getAttribute('data-color') || !saved.v || typeof saved.v !== 'object') {
			return;
		}
		let rules = '';
		for (const [theme, variables] of Object.entries(saved.v)) {
			if (!/^[a-z_]+$/.test(theme) || !variables || typeof variables !== 'object') continue;
			let declarations = '';
			for (const [name, value] of Object.entries(variables)) {
				if (/^--instance-[a-z-]+$/.test(name) && typeof value === 'string' && !/[;{}<\\]/.test(value)) {
					declarations += `${name}:${value};`;
				}
			}
			rules += `${theme === 'dark' ? ':root' : `:root.theme-${theme}`}{${declarations}}`;
		}
		style.textContent += rules;
	} catch {}
})();
