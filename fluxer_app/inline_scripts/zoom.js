(() => {
	try {
		const stored = localStorage.getItem('fluxer:accessibility:zoom-preboot');
		if (!stored) {
			return;
		}
		const percent = Number.parseInt(stored, 10);
		if (percent >= 50 && percent <= 200) {
			document.documentElement.style.setProperty('--custom-zoom', String(percent));
		}
	} catch {}
})();
