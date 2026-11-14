(() => {
	try {
		const MAX_RETRIES_PER_FILE = 2;
		const URL_ATTRIBUTE_BY_TAG = {SCRIPT: 'src', LINK: 'href'};
		const retriesByFile = {};

		const withRetryParam = (url, attempt) => {
			const separator = url.indexOf('?') < 0 ? '?' : '&';
			const nonce = Math.random().toString(36).slice(2, 10);
			return `${url}${separator}r=${attempt}-${nonce}`;
		};

		window.addEventListener(
			'error',
			(event) => {
				const failed = event.target;
				if (!failed?.hasAttribute?.('data-fluxer-entry') || !failed.integrity) {
					return;
				}
				const tag = failed.tagName;
				const urlAttribute = URL_ATTRIBUTE_BY_TAG[tag];
				if (!urlAttribute) {
					return;
				}
				const originalUrl = failed.getAttribute('data-fluxer-src') || failed.getAttribute(urlAttribute);
				if (!originalUrl) {
					return;
				}
				const attempt = (retriesByFile[originalUrl] || 0) + 1;
				if (attempt > MAX_RETRIES_PER_FILE) {
					return;
				}
				retriesByFile[originalUrl] = attempt;
				try {
					console.warn(
						'[ChunkIntegrity] A file failed to load or failed its integrity check. Fetching a fresh copy.',
						originalUrl,
					);
				} catch {}
				const replacement = document.createElement(tag);
				for (const attribute of failed.attributes) {
					replacement.setAttribute(attribute.name, attribute.value);
				}
				replacement.setAttribute('data-fluxer-src', originalUrl);
				replacement.setAttribute(urlAttribute, withRetryParam(originalUrl, attempt));
				const swapIn = () => {
					const parent = failed.parentNode || document.head;
					parent.insertBefore(replacement, failed.nextSibling);
					failed.parentNode?.removeChild(failed);
				};
				if (typeof fetch === 'function') {
					fetch(originalUrl, {cache: 'reload', mode: 'cors', credentials: 'omit', integrity: failed.integrity}).then(
						swapIn,
						swapIn,
					);
				} else {
					swapIn();
				}
			},
			true,
		);
	} catch {}
})();
