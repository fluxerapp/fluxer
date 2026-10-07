// SPDX-License-Identifier: AGPL-3.0-or-later

export const DESKTOP_KNOWN_INSTANCE_CHANNELS = Object.freeze({
	getAll: 'desktop-known-instances:get-all',
	upsert: 'desktop-known-instances:upsert',
	delete: 'desktop-known-instances:delete',
} as const);

export interface DesktopKnownInstanceRecord {
	readonly instanceKey: string;
	readonly domain: string;
	readonly displayName: string;
	readonly lastUsed: number;
}

export interface DesktopKnownInstanceStorageAPI {
	getAll: () => Promise<ReadonlyArray<DesktopKnownInstanceRecord>>;
	upsert: (instance: DesktopKnownInstanceRecord) => Promise<void>;
	delete: (instanceKey: string) => Promise<void>;
}
