(() => {
	try {
		if (typeof WebSocket !== 'function' || typeof fetch !== 'function') {
			return;
		}
		if (location.protocol === 'fluxer-app:') {
			return;
		}
		const path = location.pathname;
		if (path === '/' || path.startsWith('/migrate/') || path.startsWith('/oauth2/')) {
			return;
		}
		if (localStorage.getItem('fluxer:gateway:preboot:session') !== '1') {
			return;
		}
		const hint = JSON.parse(localStorage.getItem('fluxer:preboot:network') || 'null');
		const activeAccountKey = localStorage.getItem('fluxer:auth:active-account-key');
		if (hint?.v !== 1 || typeof hint.a !== 'string' || hint.a !== activeAccountKey) {
			return;
		}

		const preboot = {startedAt: Date.now(), discovery: null, gateway: null};
		window.__FLUXER_PREBOOT_NETWORK__ = preboot;

		const discoveryUrl = hint.d;
		if (typeof discoveryUrl === 'string' && /^https?:\/\//.test(discoveryUrl)) {
			const response = fetch(discoveryUrl, {
				method: 'GET',
				headers: {Accept: 'application/json'},
				redirect: 'error',
				credentials: 'omit',
				cache: 'no-store',
				referrerPolicy: 'no-referrer',
			});
			response.catch(() => {});
			preboot.discovery = {url: discoveryUrl, response};
		}

		const gatewayUrl = hint.g;
		if (typeof gatewayUrl !== 'string' || !/^wss?:\/\//.test(gatewayUrl)) {
			return;
		}

		const WASM_URL = '{{LIBFLUXCORE_WASM_URL}}';
		const preloadWasm = () => {
			const link = document.createElement('link');
			link.rel = 'preload';
			link.as = 'fetch';
			link.crossOrigin = 'anonymous';
			link.fetchPriority = 'low';
			link.href = WASM_URL;
			document.head.appendChild(link);
		};
		if (!WASM_URL.includes('{{')) {
			if (document.readyState === 'loading') {
				document.addEventListener('DOMContentLoaded', preloadWasm);
			} else {
				preloadWasm();
			}
		}

		const url = new URL(gatewayUrl);
		url.searchParams.set('v', '1');
		url.searchParams.set('encoding', 'json');
		url.searchParams.set('compress', 'zstd-stream');
		url.searchParams.set('stream', '1');
		const socket = new WebSocket(url.toString());
		socket.binaryType = 'arraybuffer';

		const HEARTBEAT_INTERVAL_MS = 20_000;
		const UNCLAIMED_AFTER_LOAD_MS = 15_000;
		const HEARTBEAT_FRAME_ZSTD = new Uint8Array([
			40, 181, 47, 253, 32, 17, 137, 0, 0, 123, 34, 111, 112, 34, 58, 49, 44, 34, 100, 34, 58, 110, 117, 108, 108, 125,
		]);
		let heartbeatTimer = 0;
		let unclaimedTimer = 0;

		const stopTimers = () => {
			clearInterval(heartbeatTimer);
			clearTimeout(unclaimedTimer);
			removeEventListener('pagehide', closeUnclaimed);
			removeEventListener('load', startUnclaimedTimer);
		};
		const gateway = {url: url.toString(), socket, open: false, messages: [], release: stopTimers};
		function closeUnclaimed() {
			if (preboot.gateway !== gateway) {
				return;
			}
			preboot.gateway = null;
			stopTimers();
			try {
				socket.close(1000, 'Preboot gateway unclaimed');
			} catch {}
		}
		function startUnclaimedTimer() {
			clearTimeout(unclaimedTimer);
			unclaimedTimer = setTimeout(closeUnclaimed, UNCLAIMED_AFTER_LOAD_MS);
		}

		socket.onopen = () => {
			gateway.open = true;
			heartbeatTimer = setInterval(() => {
				if (preboot.gateway === gateway && socket.readyState === WebSocket.OPEN) {
					socket.send(HEARTBEAT_FRAME_ZSTD);
				}
			}, HEARTBEAT_INTERVAL_MS);
		};
		socket.onmessage = (event) => {
			gateway.messages.push(event);
		};
		socket.onclose = socket.onerror = () => {
			if (preboot.gateway === gateway) {
				preboot.gateway = null;
				stopTimers();
			}
		};

		preboot.gateway = gateway;
		addEventListener('pagehide', closeUnclaimed);
		if (document.readyState === 'complete') {
			startUnclaimedTimer();
		} else {
			addEventListener('load', startUnclaimedTimer);
		}
	} catch {}
})();
