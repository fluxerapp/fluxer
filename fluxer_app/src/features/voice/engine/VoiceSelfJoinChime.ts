// SPDX-License-Identifier: AGPL-3.0-or-later

import {SoundType} from '@app/features/notification/utils/SoundUtils';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import VoiceRegionTeleport from '@app/features/voice/state/VoiceRegionTeleport';

export const SELF_JOIN_CHIME_DEDUPE_WINDOW_MS = 2000;
export const SELF_JOIN_CHIME_START_DEADLINE_MS = 1500;
export const VOICE_JOIN_CHIME_SEQUENCE_RETENTION_MS = 30_000;
const RECENT_SELF_JOIN_CHIME_MAX_ENTRIES = 16;
export const VOICE_JOIN_CHIME_SEQUENCE_MAX_ENTRIES = 64;

export type SelfJoinChimeSource = 'gateway' | 'livekit-room' | 'native-ready';

export interface VoiceJoinChimeSequenceIdentity {
	userId: string;
	channelId: string;
}

export type VoiceJoinChimeSequenceResult = 'started' | 'unavailable' | 'expired-before-start';

interface VoiceJoinChimeSequenceEntry {
	key: string;
	joinToken: string | null;
	controller: AbortController;
	result: VoiceJoinChimeSequenceResult | null;
	expired: boolean;
	resultPromise: Promise<VoiceJoinChimeSequenceResult>;
	resolveResult: (result: VoiceJoinChimeSequenceResult) => void;
	startPromise: Promise<VoiceJoinChimeSequenceResult> | null;
	deadline: ReturnType<typeof setTimeout> | null;
	cleanup: ReturnType<typeof setTimeout> | null;
}

interface RecentSelfJoinChime {
	playedAt: number;
	source: SelfJoinChimeSource;
	startPromise: Promise<boolean>;
}

const recentSelfJoinChimesByConnectionId = new Map<string, RecentSelfJoinChime>();
const voiceJoinChimeSequenceEntries = new Map<string, VoiceJoinChimeSequenceEntry>();

export function resetSelfJoinChimesForTests(): void {
	recentSelfJoinChimesByConnectionId.clear();
	for (const entry of voiceJoinChimeSequenceEntries.values()) {
		entry.controller.abort();
		if (entry.deadline) clearTimeout(entry.deadline);
		if (entry.cleanup) clearTimeout(entry.cleanup);
		if (entry.result === null) {
			entry.result = 'unavailable';
			entry.resolveResult('unavailable');
		}
	}
	voiceJoinChimeSequenceEntries.clear();
}

function getVoiceJoinChimeSequenceKey(identity: VoiceJoinChimeSequenceIdentity): string {
	return JSON.stringify([identity.channelId, identity.userId]);
}

function removeVoiceJoinChimeSequenceEntry(
	entry: VoiceJoinChimeSequenceEntry,
	pendingResult: VoiceJoinChimeSequenceResult = entry.startPromise ? 'unavailable' : 'expired-before-start',
): void {
	if (voiceJoinChimeSequenceEntries.get(entry.key) !== entry) return;
	if (entry.result === null) {
		entry.controller.abort();
		entry.result = pendingResult;
		entry.resolveResult(pendingResult);
	}
	if (entry.deadline) clearTimeout(entry.deadline);
	if (entry.cleanup) clearTimeout(entry.cleanup);
	voiceJoinChimeSequenceEntries.delete(entry.key);
}

function settleVoiceJoinChimeSequenceEntry(
	entry: VoiceJoinChimeSequenceEntry,
	result: VoiceJoinChimeSequenceResult,
): void {
	if (entry.result !== null) return;
	entry.result = result;
	if (entry.deadline) clearTimeout(entry.deadline);
	entry.deadline = null;
	entry.resolveResult(result);
	entry.cleanup = setTimeout(() => {
		removeVoiceJoinChimeSequenceEntry(entry);
	}, VOICE_JOIN_CHIME_SEQUENCE_RETENTION_MS);
}

function createVoiceJoinChimeSequenceEntry(key: string, joinToken: string | null = null): VoiceJoinChimeSequenceEntry {
	while (voiceJoinChimeSequenceEntries.size >= VOICE_JOIN_CHIME_SEQUENCE_MAX_ENTRIES) {
		const oldest = voiceJoinChimeSequenceEntries.values().next().value;
		if (!oldest) break;
		removeVoiceJoinChimeSequenceEntry(oldest);
	}
	let resolveResult!: (result: VoiceJoinChimeSequenceResult) => void;
	const resultPromise = new Promise<VoiceJoinChimeSequenceResult>((resolve) => {
		resolveResult = resolve;
	});
	const entry: VoiceJoinChimeSequenceEntry = {
		key,
		joinToken,
		controller: new AbortController(),
		result: null,
		expired: false,
		resultPromise,
		resolveResult,
		startPromise: null,
		deadline: null,
		cleanup: null,
	};
	entry.deadline = setTimeout(() => {
		entry.expired = true;
		entry.controller.abort();
		settleVoiceJoinChimeSequenceEntry(entry, entry.startPromise ? 'unavailable' : 'expired-before-start');
	}, SELF_JOIN_CHIME_START_DEADLINE_MS);
	voiceJoinChimeSequenceEntries.set(key, entry);
	return entry;
}

function getOrCreateVoiceJoinChimeSequenceEntry(identity: VoiceJoinChimeSequenceIdentity): VoiceJoinChimeSequenceEntry {
	const key = getVoiceJoinChimeSequenceKey(identity);
	return voiceJoinChimeSequenceEntries.get(key) ?? createVoiceJoinChimeSequenceEntry(key);
}

export function startVoiceJoinChimeSequence(
	identity: VoiceJoinChimeSequenceIdentity,
	joinToken: string | null,
	start: (signal: AbortSignal) => Promise<boolean>,
): Promise<VoiceJoinChimeSequenceResult> {
	let entry = getOrCreateVoiceJoinChimeSequenceEntry(identity);
	const exactJoinToken = joinToken && joinToken.length > 0 ? joinToken : null;
	if (
		(exactJoinToken && entry.joinToken && exactJoinToken !== entry.joinToken) ||
		(entry.expired && !entry.startPromise)
	) {
		removeVoiceJoinChimeSequenceEntry(entry);
		entry = createVoiceJoinChimeSequenceEntry(getVoiceJoinChimeSequenceKey(identity), exactJoinToken);
	}
	if (entry.joinToken === null) entry.joinToken = exactJoinToken;
	if (entry.startPromise) return entry.startPromise;
	if (entry.result !== null) return Promise.resolve(entry.result);
	const startedEntry = entry;
	const startPromise = Promise.resolve()
		.then(() => start(startedEntry.controller.signal))
		.then(
			(result) => {
				const sequenceResult = result ? 'started' : 'unavailable';
				settleVoiceJoinChimeSequenceEntry(startedEntry, sequenceResult);
				return startedEntry.result ?? sequenceResult;
			},
			() => {
				settleVoiceJoinChimeSequenceEntry(startedEntry, 'unavailable');
				return 'unavailable' as const;
			},
		);
	entry.startPromise = Promise.race([startPromise, entry.resultPromise]);
	return entry.startPromise;
}

export function waitForVoiceJoinChimeSequence(
	identity: VoiceJoinChimeSequenceIdentity,
): Promise<VoiceJoinChimeSequenceResult> {
	return getOrCreateVoiceJoinChimeSequenceEntry(identity).resultPromise;
}

export function discardVoiceJoinChimeSequence(identity: VoiceJoinChimeSequenceIdentity): void {
	const entry = voiceJoinChimeSequenceEntries.get(getVoiceJoinChimeSequenceKey(identity));
	if (entry) removeVoiceJoinChimeSequenceEntry(entry, 'expired-before-start');
}

function pruneRecentSelfJoinChimes(now: number): void {
	for (const [connectionId, entry] of recentSelfJoinChimesByConnectionId) {
		if (now - entry.playedAt >= SELF_JOIN_CHIME_DEDUPE_WINDOW_MS) {
			recentSelfJoinChimesByConnectionId.delete(connectionId);
		}
	}
	while (recentSelfJoinChimesByConnectionId.size >= RECENT_SELF_JOIN_CHIME_MAX_ENTRIES) {
		const oldestKey = recentSelfJoinChimesByConnectionId.keys().next().value;
		if (oldestKey === undefined) break;
		recentSelfJoinChimesByConnectionId.delete(oldestKey);
	}
}

async function startSelfJoinChime(externalSignal?: AbortSignal): Promise<boolean> {
	const controller = new AbortController();
	const abort = (): void => controller.abort();
	if (externalSignal?.aborted) {
		controller.abort();
	} else {
		externalSignal?.addEventListener('abort', abort, {once: true});
	}
	let deadline: ReturnType<typeof setTimeout> | null = null;
	try {
		return await Promise.race([
			SoundCommands.playOneShotSoundImmediatelyBypassingSelfDeafened(SoundType.UserJoin, controller.signal, {
				ignoreGroupCooldown: true,
			}),
			new Promise<boolean>((resolve) => {
				deadline = setTimeout(() => {
					controller.abort();
					resolve(false);
				}, SELF_JOIN_CHIME_START_DEADLINE_MS);
			}),
		]);
	} finally {
		if (deadline) clearTimeout(deadline);
		externalSignal?.removeEventListener('abort', abort);
	}
}

export function playSelfJoinChimeOnce(
	connectionId: string | null | undefined,
	source: SelfJoinChimeSource,
	signal?: AbortSignal,
): Promise<boolean> {
	if (VoiceRegionTeleport.shouldSuppressRejoinSounds()) {
		return Promise.resolve(false);
	}
	if (!connectionId) {
		return startSelfJoinChime(signal);
	}

	const now = Date.now();
	pruneRecentSelfJoinChimes(now);
	const recent = recentSelfJoinChimesByConnectionId.get(connectionId);
	if (recent && now - recent.playedAt < SELF_JOIN_CHIME_DEDUPE_WINDOW_MS) return recent.startPromise;

	const startPromise = startSelfJoinChime(signal);
	recentSelfJoinChimesByConnectionId.set(connectionId, {playedAt: now, source, startPromise});
	return startPromise;
}
