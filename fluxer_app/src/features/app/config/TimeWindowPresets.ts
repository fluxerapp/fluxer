// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const SECONDS_PRESET_LABEL_DESCRIPTOR = msg({
	message: '{tenSeconds} seconds',
	comment: 'Time window preset label shown as a standalone duration in option lists. The number is interpolated.',
});
const FIFTEEN_MINUTES_PRESET_LABEL_DESCRIPTOR = msg({
	message: '{fifteenMinutes} minutes',
	comment: 'Time window preset label shown as a standalone duration in option lists. The number is interpolated.',
});
const THIRTY_MINUTES_PRESET_LABEL_DESCRIPTOR = msg({
	message: '{thirtyMinutes} minutes',
	comment: 'Time window preset label shown as a standalone duration in option lists. The number is interpolated.',
});
const ONE_HOUR_PRESET_LABEL_DESCRIPTOR = msg({
	message: '{oneHour} hour',
	comment: 'Time window preset label for a single-hour interval. The number is interpolated.',
});
const THREE_HOURS_PRESET_LABEL_DESCRIPTOR = msg({
	message: '{threeHours} hours',
	comment: 'Time window preset label shown as a standalone duration in option lists. The number is interpolated.',
});
const FOUR_HOURS_PRESET_LABEL_DESCRIPTOR = msg({
	message: '{fourHours} hours',
	comment: 'Time window preset label shown as a standalone duration in option lists. The number is interpolated.',
});
const EIGHT_HOURS_PRESET_LABEL_DESCRIPTOR = msg({
	message: '{eightHours} hours',
	comment: 'Time window preset label shown as a standalone duration in option lists. The number is interpolated.',
});
const TWENTY_FOUR_HOURS_PRESET_LABEL_DESCRIPTOR = msg({
	message: '{twentyFourHours} hours',
	comment: 'Time window preset label shown as a standalone duration in option lists. The number is interpolated.',
});
const THREE_DAYS_PRESET_LABEL_DESCRIPTOR = msg({
	message: '{threeDays} days',
	comment: 'Time window preset label shown as a standalone duration in option lists. The number is interpolated.',
});
const DONT_CLEAR_PRESET_LABEL_DESCRIPTOR = msg({
	message: "Don't clear",
	comment: 'Time window preset option meaning the value should never auto-clear or expire.',
});
const FOR_SECONDS_PRESET_LABEL_DESCRIPTOR = msg({
	message: 'For {tenSeconds} seconds',
	comment: 'Time window preset label phrased as a duration ("For X seconds"). Used in option lists for timed actions.',
});
const FOR_FIFTEEN_MINUTES_PRESET_LABEL_DESCRIPTOR = msg({
	message: 'For {fifteenMinutes} minutes',
	comment: 'Time window preset label phrased as a duration ("For X minutes"). Used in option lists for timed actions.',
});
const FOR_THIRTY_MINUTES_PRESET_LABEL_DESCRIPTOR = msg({
	message: 'For {thirtyMinutes} minutes',
	comment: 'Time window preset label phrased as a duration ("For X minutes"). Used in option lists for timed actions.',
});
const FOR_ONE_HOUR_PRESET_LABEL_DESCRIPTOR = msg({
	message: 'For {oneHour} hour',
	comment: 'Time window preset label phrased as a duration ("For 1 hour"). Used in option lists for timed actions.',
});
const FOR_THREE_HOURS_PRESET_LABEL_DESCRIPTOR = msg({
	message: 'For {threeHours} hours',
	comment: 'Time window preset label phrased as a duration ("For X hours"). Used in option lists for timed actions.',
});
const FOR_FOUR_HOURS_PRESET_LABEL_DESCRIPTOR = msg({
	message: 'For {fourHours} hours',
	comment: 'Time window preset label phrased as a duration ("For X hours"). Used in option lists for timed actions.',
});
const FOR_EIGHT_HOURS_PRESET_LABEL_DESCRIPTOR = msg({
	message: 'For {eightHours} hours',
	comment: 'Time window preset label phrased as a duration ("For X hours"). Used in option lists for timed actions.',
});
const FOR_TWENTY_FOUR_HOURS_PRESET_LABEL_DESCRIPTOR = msg({
	message: 'For {twentyFourHours} hours',
	comment: 'Time window preset label phrased as a duration ("For X hours"). Used in option lists for timed actions.',
});
const FOR_THREE_DAYS_PRESET_LABEL_DESCRIPTOR = msg({
	message: 'For {threeDays} days',
	comment: 'Time window preset label phrased as a duration ("For X days"). Used in option lists for timed actions.',
});
const BASE_PRESETS = [
	{key: '15m', minutes: 15},
	{key: '30m', minutes: 30},
	{key: '1h', minutes: 60},
	{key: '3h', minutes: 3 * 60},
	{key: '4h', minutes: 4 * 60},
	{key: '8h', minutes: 8 * 60},
	{key: '24h', minutes: 24 * 60},
	{key: '3d', minutes: 3 * 24 * 60},
	{key: 'never', minutes: null},
] as const;
const DEVELOPER_PRESETS = [{key: '10s', minutes: 10 / 60}] as const;
const ALL_PRESETS = [...BASE_PRESETS, ...DEVELOPER_PRESETS] as const;
const BASE_TIME_WINDOW_KEYS: ReadonlyArray<(typeof BASE_PRESETS)[number]['key']> = BASE_PRESETS.map(
	(preset) => preset.key,
);
const DEVELOPER_TIME_WINDOW_KEYS: ReadonlyArray<(typeof DEVELOPER_PRESETS)[number]['key']> = DEVELOPER_PRESETS.map(
	(preset) => preset.key,
);

export type TimeWindowKey = (typeof ALL_PRESETS)[number]['key'];
export type TimeWindowPreset = (typeof ALL_PRESETS)[number];

const PRESET_MAP = new Map<TimeWindowKey, TimeWindowPreset>(ALL_PRESETS.map((preset) => [preset.key, preset]));
export const DEFAULT_TIME_WINDOW_KEY: TimeWindowKey = '24h';
const EXPIRY_MATCH_TOLERANCE_MS = 60 * 1000;
const TEN_SECONDS = 10;
const FIFTEEN_MINUTES = 15;
const THIRTY_MINUTES = 30;
const ONE_HOUR = 1;
const THREE_HOURS = 3;
const FOUR_HOURS = 4;
const EIGHT_HOURS = 8;
const TWENTY_FOUR_HOURS = 24;
const THREE_DAYS = 3;
export const TIME_WINDOW_LABEL_MESSAGES: Record<TimeWindowKey, MessageDescriptor> = {
	'10s': {...SECONDS_PRESET_LABEL_DESCRIPTOR, values: {tenSeconds: TEN_SECONDS}},
	'15m': {...FIFTEEN_MINUTES_PRESET_LABEL_DESCRIPTOR, values: {fifteenMinutes: FIFTEEN_MINUTES}},
	'30m': {...THIRTY_MINUTES_PRESET_LABEL_DESCRIPTOR, values: {thirtyMinutes: THIRTY_MINUTES}},
	'1h': {...ONE_HOUR_PRESET_LABEL_DESCRIPTOR, values: {oneHour: ONE_HOUR}},
	'3h': {...THREE_HOURS_PRESET_LABEL_DESCRIPTOR, values: {threeHours: THREE_HOURS}},
	'4h': {...FOUR_HOURS_PRESET_LABEL_DESCRIPTOR, values: {fourHours: FOUR_HOURS}},
	'8h': {...EIGHT_HOURS_PRESET_LABEL_DESCRIPTOR, values: {eightHours: EIGHT_HOURS}},
	'24h': {...TWENTY_FOUR_HOURS_PRESET_LABEL_DESCRIPTOR, values: {twentyFourHours: TWENTY_FOUR_HOURS}},
	'3d': {...THREE_DAYS_PRESET_LABEL_DESCRIPTOR, values: {threeDays: THREE_DAYS}},
	never: DONT_CLEAR_PRESET_LABEL_DESCRIPTOR,
};
export const TIME_WINDOW_FOR_LABEL_MESSAGES: Record<Exclude<TimeWindowKey, 'never'>, MessageDescriptor> = {
	'10s': {...FOR_SECONDS_PRESET_LABEL_DESCRIPTOR, values: {tenSeconds: TEN_SECONDS}},
	'15m': {...FOR_FIFTEEN_MINUTES_PRESET_LABEL_DESCRIPTOR, values: {fifteenMinutes: FIFTEEN_MINUTES}},
	'30m': {...FOR_THIRTY_MINUTES_PRESET_LABEL_DESCRIPTOR, values: {thirtyMinutes: THIRTY_MINUTES}},
	'1h': {...FOR_ONE_HOUR_PRESET_LABEL_DESCRIPTOR, values: {oneHour: ONE_HOUR}},
	'3h': {...FOR_THREE_HOURS_PRESET_LABEL_DESCRIPTOR, values: {threeHours: THREE_HOURS}},
	'4h': {...FOR_FOUR_HOURS_PRESET_LABEL_DESCRIPTOR, values: {fourHours: FOUR_HOURS}},
	'8h': {...FOR_EIGHT_HOURS_PRESET_LABEL_DESCRIPTOR, values: {eightHours: EIGHT_HOURS}},
	'24h': {...FOR_TWENTY_FOUR_HOURS_PRESET_LABEL_DESCRIPTOR, values: {twentyFourHours: TWENTY_FOUR_HOURS}},
	'3d': {...FOR_THREE_DAYS_PRESET_LABEL_DESCRIPTOR, values: {threeDays: THREE_DAYS}},
};
export const minutesToMs = (minutes: number | null): number | null => (minutes == null ? null : minutes * 60 * 1000);
const getTimeWindowKeys = (includeDeveloperOptions: boolean): ReadonlyArray<TimeWindowKey> => {
	if (!includeDeveloperOptions) return BASE_TIME_WINDOW_KEYS;
	return [...DEVELOPER_TIME_WINDOW_KEYS, ...BASE_TIME_WINDOW_KEYS];
};
const createTimeWindowOptionList = (keys: ReadonlyArray<TimeWindowKey>): ReadonlyArray<TimeWindowPreset> => {
	const list = keys.map((key) => {
		const preset = PRESET_MAP.get(key);
		if (!preset) {
			throw new Error(`Unknown time window key: ${key}`);
		}
		return preset;
	});
	const neverIndex = list.findIndex((preset) => preset.key === 'never');
	if (neverIndex <= 0) return list;
	const neverPreset = list[neverIndex];
	const withoutNever = list.filter((_, index) => index !== neverIndex);
	return [neverPreset, ...withoutNever];
};
export const getTimeWindowPresets = (options?: {
	includeDeveloperOptions?: boolean;
	includeNever?: boolean;
}): ReadonlyArray<TimeWindowPreset> => {
	const includeDeveloperOptions = options?.includeDeveloperOptions ?? false;
	const includeNever = options?.includeNever ?? true;
	const keys = getTimeWindowKeys(includeDeveloperOptions);
	const filteredKeys = includeNever ? keys : keys.filter((key) => key !== 'never');
	return createTimeWindowOptionList(filteredKeys);
};
export const getFiniteTimeWindowPresets = (options?: {
	includeDeveloperOptions?: boolean;
}): ReadonlyArray<TimeWindowPreset> =>
	getTimeWindowPresets({includeDeveloperOptions: options?.includeDeveloperOptions, includeNever: false});
export const getTimeWindowKeyForExpiresAt = (
	expiresAt: string | null | undefined,
	options: {
		includeDeveloperOptions?: boolean;
		referenceTime?: number | Date;
		fallbackKey?: TimeWindowKey;
	} = {},
): TimeWindowKey => {
	if (expiresAt == null) return 'never';
	const expiresAtMs = Date.parse(expiresAt);
	const referenceMs =
		options.referenceTime instanceof Date ? options.referenceTime.getTime() : (options.referenceTime ?? Date.now());
	const fallbackKey = options.fallbackKey ?? DEFAULT_TIME_WINDOW_KEY;
	if (!Number.isFinite(expiresAtMs) || !Number.isFinite(referenceMs)) return fallbackKey;
	const remainingMs = expiresAtMs - referenceMs;
	if (remainingMs <= 0) return fallbackKey;
	const finitePresets = getFiniteTimeWindowPresets({includeDeveloperOptions: options.includeDeveloperOptions});
	const matchingPreset = finitePresets.find((preset) => {
		const durationMs = minutesToMs(preset.minutes);
		return durationMs != null && remainingMs <= durationMs + EXPIRY_MATCH_TOLERANCE_MS;
	});
	const lastPreset = finitePresets[finitePresets.length - 1];
	return matchingPreset?.key ?? lastPreset?.key ?? fallbackKey;
};
