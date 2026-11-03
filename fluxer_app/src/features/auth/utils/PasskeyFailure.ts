// SPDX-License-Identifier: AGPL-3.0-or-later

type PasskeyFailureFallback = 'show_error' | 'browser_handoff';
type PasskeyFailure = 'aborted' | PasskeyFailureFallback;

export function classifyPasskeyFailure(error: unknown, fallback: PasskeyFailureFallback): PasskeyFailure {
	if (!(error instanceof Error)) {
		return fallback;
	}
	if (error.name === 'AbortError') {
		return 'aborted';
	}
	if (error.name === 'NotAllowedError') {
		return 'show_error';
	}
	return fallback;
}
