(() => {
	try {
		const location = window.location;
		const isHandoff = /^#(?:.*&)?handoff=/.test(location.hash);
		if (location.pathname !== '/' || isHandoff) {
			return;
		}
		location.replace(`/channels/@me${location.search}${location.hash}`);
	} catch {}
})();
