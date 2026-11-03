// SPDX-License-Identifier: AGPL-3.0-or-later

import {AsyncLocalStorage} from 'node:async_hooks';

export type AdminAuditAttribution = Readonly<Record<string, string>>;

const attributionContext = new AsyncLocalStorage<AdminAuditAttribution>();

export function runWithAdminAuditAttribution<T>(attribution: AdminAuditAttribution | null, fn: () => T): T {
	if (attribution === null) {
		return fn();
	}
	return attributionContext.run(attribution, fn);
}

export function withAdminAuditAttribution(metadata: Map<string, string>): Map<string, string> {
	const attribution = attributionContext.getStore();
	if (!attribution) {
		return metadata;
	}
	const merged = new Map(metadata);
	for (const [key, value] of Object.entries(attribution)) {
		merged.set(key, value);
	}
	return merged;
}
