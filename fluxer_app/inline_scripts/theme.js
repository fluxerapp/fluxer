(() => {
	try {
		const root = document.documentElement;
		if (document.hasFocus()) {
			root.classList.add('window-focused');
		}
		const theme = localStorage.getItem('theme');
		if (theme) {
			root.classList.add(`theme-${theme}`);
		}
	} catch {}
})();
