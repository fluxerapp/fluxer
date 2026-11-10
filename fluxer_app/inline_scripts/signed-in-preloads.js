(() => {
	try {
		if (!localStorage.getItem('{{SIGNED_IN_HINT_STORAGE_KEY}}')) {
			return;
		}
		const frame = window['{{BOOT_FRAME_GLOBAL}}'];
		const preloads = JSON.parse(document.getElementById('{{SIGNED_IN_PRELOADS_ELEMENT_ID}}').textContent);
		const insertPreloads = (as) => {
			for (const preload of preloads.filter((entry) => entry.as === as)) {
				const link = document.createElement('link');
				link.rel = 'preload';
				link.as = preload.as;
				link.href = preload.href;
				link.setAttribute('fetchpriority', frame && as === 'style' ? 'high' : 'low');
				if (preload.integrity) {
					link.crossOrigin = 'anonymous';
					link.integrity = preload.integrity;
				}
				document.head.appendChild(link);
			}
		};
		insertPreloads('style');
		if (frame) {
			frame.settled.then(() => insertPreloads('script'));
		} else {
			insertPreloads('script');
		}
	} catch {}
})();
