(() => {
	try {
		const factor = Number.parseFloat(localStorage.getItem('fluxer:accessibility:saturation-preboot') ?? '');
		if (factor >= 0 && factor < 1) {
			document.documentElement.style.setProperty('--saturation-factor', String(factor));
		}
	} catch {}
})();
