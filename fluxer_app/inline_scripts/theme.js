(() => {
	try {
		const theme = localStorage.getItem('theme');
		if (theme) {
			document.documentElement.classList.add(`theme-${theme}`);
		}
	} catch {}
})();
