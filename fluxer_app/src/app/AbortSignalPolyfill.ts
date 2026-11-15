// SPDX-License-Identifier: AGPL-3.0-or-later

function abortSignalTimeout(milliseconds: number): AbortSignal {
	const controller = new AbortController();
	setTimeout(() => controller.abort(new DOMException('The operation timed out.', 'TimeoutError')), milliseconds);
	return controller.signal;
}

function abortSignalAny(signals: Iterable<AbortSignal>): AbortSignal {
	const controller = new AbortController();
	const sources = [...signals];
	const aborted = sources.find((source) => source.aborted);
	if (aborted) {
		controller.abort(aborted.reason);
		return controller.signal;
	}
	for (const source of sources) {
		source.addEventListener('abort', () => controller.abort(source.reason), {once: true, signal: controller.signal});
	}
	return controller.signal;
}

function installAbortSignalPolyfill(target: typeof AbortSignal = AbortSignal): void {
	if (typeof target.timeout !== 'function') {
		target.timeout = abortSignalTimeout;
	}
	if (typeof target.any !== 'function') {
		target.any = abortSignalAny;
	}
}

installAbortSignalPolyfill();
