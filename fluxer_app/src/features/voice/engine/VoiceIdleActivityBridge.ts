// SPDX-License-Identifier: AGPL-3.0-or-later

import Idle from '@app/features/ui/state/Idle';
import type {VoiceEngineV2AppParticipantSnapshot} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectors';

const VOICE_ACTIVITY_RECORD_INTERVAL_MS = 5000;

type VoiceActivitySource = Readonly<{
	isLocal?: boolean;
	isSpeaking?: boolean;
	isAudioLevelSpeaking?: boolean;
	isMicrophoneEnabled?: boolean;
}>;
type NoteVoiceActivityOptions = Readonly<{
	force?: boolean;
	now?: number;
	speaking?: boolean;
}>;

let lastRecordedVoiceActivityAt: number | null = null;

function isSpeaking(source: VoiceActivitySource, override?: boolean): boolean {
	return override ?? Boolean(source.isSpeaking || source.isAudioLevelSpeaking);
}

function shouldRecordVoiceActivity(source: VoiceActivitySource, options?: NoteVoiceActivityOptions): boolean {
	if (!source.isLocal) return false;
	if (source.isMicrophoneEnabled === false) return false;
	return isSpeaking(source, options?.speaking);
}

function maybeRecordActivity(now: number, force: boolean): void {
	if (
		force ||
		lastRecordedVoiceActivityAt === null ||
		now - lastRecordedVoiceActivityAt >= VOICE_ACTIVITY_RECORD_INTERVAL_MS
	) {
		lastRecordedVoiceActivityAt = now;
		Idle.recordActivity();
	}
}

export function noteLocalVoiceActivity(
	source: VoiceActivitySource | undefined,
	options?: NoteVoiceActivityOptions,
): boolean {
	if (!source || !shouldRecordVoiceActivity(source, options)) return false;
	maybeRecordActivity(options?.now ?? Date.now(), options?.force === true);
	return true;
}

export function noteLocalVoiceActivityFromSnapshot(
	snapshot: VoiceEngineV2AppParticipantSnapshot | undefined,
	options?: NoteVoiceActivityOptions,
): boolean {
	return noteLocalVoiceActivity(snapshot, options);
}
