// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {
	VoiceMediaGraphSubscriptionCommand,
	VoiceMediaGraphSubscriptionEntry,
} from '@app/features/voice/engine/VoiceMediaGraphSubscriptionTypes';

const VOICE_MEDIA_GRAPH_RECONCILE_SCAN_LIMIT = 2048;

export function voiceMediaGraphCommandsEquivalent(
	left: VoiceMediaGraphSubscriptionCommand,
	right: VoiceMediaGraphSubscriptionCommand,
): boolean {
	if (left.type !== right.type) return false;
	if (left.participantIdentity !== right.participantIdentity) return false;
	return left.source === right.source;
}

export function voiceMediaGraphCommandAlreadyQueued(
	queued: ReadonlyArray<VoiceMediaGraphSubscriptionCommand>,
	command: VoiceMediaGraphSubscriptionCommand,
): boolean {
	assert.ok(queued.length <= VOICE_MEDIA_GRAPH_RECONCILE_SCAN_LIMIT, 'queued command scan exceeded limit');
	for (const existing of queued) {
		if (voiceMediaGraphCommandsEquivalent(existing, command)) return true;
	}
	return false;
}

export function reconcileVoiceMediaGraphSubscriptionEntry(
	entry: VoiceMediaGraphSubscriptionEntry,
): Array<VoiceMediaGraphSubscriptionCommand> {
	if (!entry.subscribed) return [];
	if (!entry.publication.available) return [];
	const target = {participantIdentity: entry.participantIdentity, source: entry.source};
	if (entry.actual.subscribed !== true) {
		return [{type: 'subscribePublication', ...target, enabled: entry.desired.enabled, quality: entry.desired.quality}];
	}
	const commands: Array<VoiceMediaGraphSubscriptionCommand> = [];
	if (entry.actual.enabled !== entry.desired.enabled) {
		commands.push({type: 'setPublicationEnabled', ...target, enabled: entry.desired.enabled});
	}
	if (entry.actual.quality !== entry.desired.quality) {
		commands.push({type: 'setPublicationQuality', ...target, quality: entry.desired.quality});
	}
	return commands;
}
