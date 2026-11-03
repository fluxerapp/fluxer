// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage, {subscribeAppStorageScope} from '@app/features/platform/state/PersistentStorage';
import {isRecord} from '@fluxer/record_utils/src/RecordGuards';
import {makeAutoObservable} from 'mobx';

interface CompactVoiceCallHeightState {
	defaultHeight: number | null;
	heightsByKey: Record<string, number>;
	expandedByKey: Record<string, boolean>;
}

const COMPACT_VOICE_CALL_HEIGHT_STORAGE_KEY = 'compact_voice_call_heights';
const COMPACT_VOICE_CALL_HEIGHT_MIN = 320;
const COMPACT_VOICE_CALL_HEIGHT_MAX = 1049;

function clampCompactVoiceCallHeight(value: number): number {
	return Math.max(COMPACT_VOICE_CALL_HEIGHT_MIN, Math.min(Math.round(value), COMPACT_VOICE_CALL_HEIGHT_MAX));
}

function parseCompactVoiceCallHeight(value: unknown): number | null {
	if (typeof value !== 'number' || !Number.isFinite(value)) {
		return null;
	}
	return clampCompactVoiceCallHeight(value);
}

function parseHeightMap(value: unknown): Record<string, number> {
	if (!isRecord(value)) {
		return {};
	}
	const result: Record<string, number> = {};
	for (const [key, rawHeight] of Object.entries(value)) {
		const parsed = parseCompactVoiceCallHeight(rawHeight);
		if (parsed == null) {
			continue;
		}
		result[key] = parsed;
	}
	return result;
}

function parseBooleanMap(value: unknown): Record<string, boolean> {
	if (!isRecord(value)) {
		return {};
	}
	const result: Record<string, boolean> = {};
	for (const [key, rawValue] of Object.entries(value)) {
		if (typeof rawValue !== 'boolean') {
			continue;
		}
		result[key] = rawValue;
	}
	return result;
}

function getInitialState(): CompactVoiceCallHeightState {
	const raw = AppStorage.getJSON<unknown>(COMPACT_VOICE_CALL_HEIGHT_STORAGE_KEY);
	if (!isRecord(raw)) {
		return {
			defaultHeight: null,
			heightsByKey: {},
			expandedByKey: {},
		};
	}
	return {
		defaultHeight: parseCompactVoiceCallHeight(raw.defaultHeight),
		heightsByKey: parseHeightMap(raw.heightsByKey),
		expandedByKey: parseBooleanMap(raw.expandedByKey),
	};
}

class CompactVoiceCallHeight {
	defaultHeight: number | null = null;
	heightsByKey: Record<string, number> = {};
	expandedByKey: Record<string, boolean> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.reload();
	}

	reload(): void {
		const initialState = getInitialState();
		this.defaultHeight = initialState.defaultHeight;
		this.heightsByKey = initialState.heightsByKey;
		this.expandedByKey = initialState.expandedByKey;
	}

	getStartingHeight(heightKey: string): number | null {
		return this.heightsByKey[heightKey] ?? this.defaultHeight;
	}

	getExpandedForKey(heightKey: string, defaultValue: boolean): boolean {
		return this.expandedByKey[heightKey] ?? defaultValue;
	}

	setHeightForKey(heightKey: string, height: number): number {
		const normalizedHeight = clampCompactVoiceCallHeight(height);
		this.defaultHeight = normalizedHeight;
		this.heightsByKey = {
			...this.heightsByKey,
			[heightKey]: normalizedHeight,
		};
		this.persist();
		return normalizedHeight;
	}

	setExpandedForKey(heightKey: string, expanded: boolean, {persist = true}: {persist?: boolean} = {}): boolean {
		this.expandedByKey = {
			...this.expandedByKey,
			[heightKey]: expanded,
		};
		if (persist) {
			this.persist();
		}
		return expanded;
	}

	private persist(): void {
		const state: CompactVoiceCallHeightState = {
			defaultHeight: this.defaultHeight,
			heightsByKey: this.heightsByKey,
			expandedByKey: this.expandedByKey,
		};
		AppStorage.setJSON(COMPACT_VOICE_CALL_HEIGHT_STORAGE_KEY, state);
	}
}

export function getGuildVoiceCallExpansionKey(channelId: string): string {
	return `guild-voice:${channelId}`;
}

export function getCompactVoiceCallExpansionKey(channelId: string, callMessageId: string | null): string {
	return callMessageId ? `${channelId}:${callMessageId}` : channelId;
}

export {COMPACT_VOICE_CALL_HEIGHT_MAX, COMPACT_VOICE_CALL_HEIGHT_MIN};

const compactVoiceCallHeight = new CompactVoiceCallHeight();

subscribeAppStorageScope(compactVoiceCallHeight.reload);

export default compactVoiceCallHeight;
