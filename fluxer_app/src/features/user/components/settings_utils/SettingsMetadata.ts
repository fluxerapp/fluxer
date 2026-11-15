// SPDX-License-Identifier: AGPL-3.0-or-later

export type SettingsAudience = 'primary' | 'advanced' | 'developer';

export type SettingsCategoryTag =
	| 'account'
	| 'privacy'
	| 'appearance'
	| 'accessibility'
	| 'chat'
	| 'media'
	| 'voice'
	| 'notifications'
	| 'desktop'
	| 'developer';

export type SettingsStatusBadgeKind = 'experimental';

export interface SettingsMetadata {
	audience?: SettingsAudience;
	tags?: ReadonlyArray<SettingsCategoryTag>;
	addedAt?: string;
	badges?: ReadonlyArray<SettingsStatusBadgeKind>;
}

const SETTINGS_NEW_WINDOW_MS = 30 * 24 * 60 * 60 * 1000;

function parseTimestampMs(value: string | undefined): number | null {
	if (!value) return null;
	const ms = Date.parse(value);
	return Number.isFinite(ms) ? ms : null;
}

export function isSettingsItemNew(
	item: Pick<SettingsMetadata, 'addedAt'>,
	now: Date | number = Date.now(),
	userCreatedAt?: Date | null,
): boolean {
	const addedAtMs = parseTimestampMs(item.addedAt);
	if (addedAtMs == null) return false;
	const nowMs = typeof now === 'number' ? now : now.getTime();
	if (addedAtMs > nowMs) return false;
	if (userCreatedAt && userCreatedAt.getTime() >= addedAtMs) return false;
	return nowMs - addedAtMs <= SETTINGS_NEW_WINDOW_MS;
}

export function isSettingsItemExperimental(item: Pick<SettingsMetadata, 'badges'>): boolean {
	return item.badges?.includes('experimental') ?? false;
}

export function getSettingsAudience(
	item: Pick<SettingsMetadata, 'audience'> & {id?: string; isAdvanced?: boolean},
): SettingsAudience {
	if (item.audience) return item.audience;
	if (item.isAdvanced || item.id?.startsWith('advanced-')) return 'advanced';
	return 'primary';
}
