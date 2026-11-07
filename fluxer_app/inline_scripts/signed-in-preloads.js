(() => {
	try {
		if (!localStorage.getItem('{{SIGNED_IN_HINT_STORAGE_KEY}}')) {
			return;
		}
		const preloads = JSON.parse(document.getElementById('{{SIGNED_IN_PRELOADS_ELEMENT_ID}}').textContent);
		for (const preload of preloads) {
			const link = document.createElement('link');
			link.rel = 'preload';
			link.as = preload.as;
			link.href = preload.href;
			link.setAttribute('fetchpriority', 'low');
			if (preload.integrity) {
				link.crossOrigin = 'anonymous';
				link.integrity = preload.integrity;
			}
			document.head.appendChild(link);
		}
	} catch {}
})();
