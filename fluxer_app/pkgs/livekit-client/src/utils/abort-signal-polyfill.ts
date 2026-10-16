// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
export function abortSignalAny(signals: Array<AbortSignal>): AbortSignal {
	if (signals.length === 0) {
		const controller = new AbortController();
		return controller.signal;
	}

	if (signals.length === 1) {
		return signals[0];
	}

	for (const signal of signals) {
		if (signal.aborted) {
			return signal;
		}
	}

	const controller = new AbortController();
	const unlisteners: Array<() => void> = Array(signals.length);

	const cleanup = () => {
		for (const unsubscribe of unlisteners) {
			unsubscribe();
		}
	};

	signals.forEach((signal, index) => {
		const handler = () => {
			controller.abort(signal.reason);
			cleanup();
		};

		signal.addEventListener('abort', handler);
		unlisteners[index] = () => signal.removeEventListener('abort', handler);
	});

	return controller.signal;
}

export function abortSignalTimeout(ms: number): AbortSignal {
	const controller = new AbortController();

	setTimeout(() => {
		controller.abort(new DOMException(`signal timed out after ${ms} ms`, 'TimeoutError'));
	}, ms);

	return controller.signal;
}
