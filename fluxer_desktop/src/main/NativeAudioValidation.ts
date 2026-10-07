// SPDX-License-Identifier: AGPL-3.0-or-later

import type {NativeAudioStartOptions, VirtmicNode} from '@electron/common/Types';

interface InternalAudioFrame {
	samples: Float32Array;
	sampleRate: number;
	channels: number;
	timestampUs: number | bigint;
}

const MIN_NATIVE_AUDIO_SAMPLE_RATE = 8000;
const MAX_NATIVE_AUDIO_SAMPLE_RATE = 192000;
const MAX_NATIVE_AUDIO_CHANNELS = 8;
const MAX_NATIVE_AUDIO_FRAME_SECONDS = 1;
const MAX_LINUX_RULE_PATTERNS = 64;
const MAX_LINUX_RULE_KEYS_PER_PATTERN = 32;
const MAX_LINUX_RULE_KEY_LENGTH = 128;
const MAX_LINUX_RULE_VALUE_LENGTH = 512;
const MAX_UINT32 = 0xffffffff;

export function isValidTargetPid(pid: unknown): pid is number {
	return typeof pid === 'number' && Number.isSafeInteger(pid) && pid > 0 && pid <= MAX_UINT32;
}

function isPlainObject(value: unknown): value is Record<string, unknown> {
	if (!value || typeof value !== 'object' || Array.isArray(value)) return false;
	const proto = Object.getPrototypeOf(value);
	return proto === Object.prototype || proto === null;
}

function isBooleanOrUndefined(value: unknown): boolean {
	return value === undefined || typeof value === 'boolean';
}

function isValidVirtmicNodePattern(value: unknown): value is VirtmicNode {
	if (!isPlainObject(value)) return false;
	const entries = Object.entries(value);
	if (entries.length > MAX_LINUX_RULE_KEYS_PER_PATTERN) return false;
	return entries.every(
		([key, propertyValue]) =>
			key.length > 0 &&
			key.length <= MAX_LINUX_RULE_KEY_LENGTH &&
			typeof propertyValue === 'string' &&
			propertyValue.length <= MAX_LINUX_RULE_VALUE_LENGTH,
	);
}

function isValidVirtmicNodeList(value: unknown): value is Array<VirtmicNode> {
	return Array.isArray(value) && value.length <= MAX_LINUX_RULE_PATTERNS && value.every(isValidVirtmicNodePattern);
}

function isValidOptionalVirtmicNodeList(value: unknown): value is Array<VirtmicNode> | undefined {
	return value === undefined || isValidVirtmicNodeList(value);
}

export function isValidLinuxRule(
	rule: NativeAudioStartOptions['linuxRule'],
): rule is NonNullable<NativeAudioStartOptions['linuxRule']> {
	if (!isPlainObject(rule)) return false;
	const allowedKeys = new Set([
		'include',
		'exclude',
		'ignoreDevices',
		'ignoreInputMedia',
		'ignoreVirtual',
		'workaround',
		'onlySpeakers',
		'onlyDefaultSpeakers',
	]);
	if (!Object.keys(rule).every((key) => allowedKeys.has(key))) return false;
	return (
		isValidOptionalVirtmicNodeList(rule.include) &&
		isValidOptionalVirtmicNodeList(rule.exclude) &&
		isBooleanOrUndefined(rule.ignoreDevices) &&
		isBooleanOrUndefined(rule.ignoreInputMedia) &&
		isBooleanOrUndefined(rule.ignoreVirtual) &&
		isBooleanOrUndefined(rule.workaround) &&
		isBooleanOrUndefined(rule.onlySpeakers) &&
		isBooleanOrUndefined(rule.onlyDefaultSpeakers)
	);
}

export function normalizeTimestampUs(timestampUs: number | bigint): number {
	const numericTimestamp = typeof timestampUs === 'bigint' ? Number(timestampUs) : timestampUs;
	if (!Number.isFinite(numericTimestamp)) return 0;
	return Math.max(0, Math.min(Number.MAX_SAFE_INTEGER, Math.round(numericTimestamp)));
}

export function isValidAudioFrame(frame: unknown): frame is InternalAudioFrame {
	if (!frame || typeof frame !== 'object') return false;
	const candidate = frame as Partial<InternalAudioFrame>;
	const samples = candidate.samples;
	const sampleRate = candidate.sampleRate;
	const channels = candidate.channels;
	const timestampUs = candidate.timestampUs;
	if (!(samples instanceof Float32Array)) return false;
	if (typeof sampleRate !== 'number' || !Number.isFinite(sampleRate)) return false;
	if (typeof channels !== 'number' || !Number.isSafeInteger(channels)) return false;
	if (sampleRate < MIN_NATIVE_AUDIO_SAMPLE_RATE || sampleRate > MAX_NATIVE_AUDIO_SAMPLE_RATE) {
		return false;
	}
	if (channels < 1 || channels > MAX_NATIVE_AUDIO_CHANNELS) return false;
	if (samples.length % channels !== 0) return false;
	const frameCount = samples.length / channels;
	const timestampValid =
		typeof timestampUs === 'bigint'
			? timestampUs >= 0n
			: typeof timestampUs === 'number' && Number.isFinite(timestampUs) && timestampUs >= 0;
	return frameCount > 0 && frameCount <= Math.ceil(sampleRate * MAX_NATIVE_AUDIO_FRAME_SECONDS) && timestampValid;
}

export function audioFrameDebugDetails(frame: unknown): Record<string, unknown> {
	if (!frame || typeof frame !== 'object') {
		return {type: frame === null ? 'null' : typeof frame};
	}
	const candidate = frame as Partial<InternalAudioFrame>;
	return {
		sampleRate: typeof candidate.sampleRate === 'number' ? candidate.sampleRate : null,
		channels: typeof candidate.channels === 'number' ? candidate.channels : null,
		samplesLength: candidate.samples instanceof Float32Array ? candidate.samples.length : null,
		timestampType: typeof candidate.timestampUs,
	};
}
