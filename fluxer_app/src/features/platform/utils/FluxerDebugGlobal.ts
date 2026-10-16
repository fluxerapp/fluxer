// SPDX-License-Identifier: AGPL-3.0-or-later

type FluxerDebugObject = NonNullable<Window['__FLUXER_DEBUG__']>;

export function getFluxerDebugObject(): FluxerDebugObject | null {
	if (typeof window === 'undefined') {
		return null;
	}
	const existing = window.__FLUXER_DEBUG__;
	if (existing === undefined || existing === null) {
		const created: FluxerDebugObject = {};
		window.__FLUXER_DEBUG__ = created;
		return created;
	}
	if (typeof existing !== 'object' || Array.isArray(existing)) {
		return null;
	}
	return existing;
}
