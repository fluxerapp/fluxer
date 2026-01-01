/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {ConnectionQuality, Participant, Room} from 'livekit-client';
import {makeAutoObservable, runInAction} from 'mobx';
import {Logger} from '~/lib/Logger';

const logger = new Logger('VoiceParticipantManager');

export type LivekitParticipantSnapshot = Readonly<{
	identity: string;
	userId: string | null;
	connectionId: string | null;
	sid: string;
	isLocal: boolean;
	isSpeaking: boolean;
	connectionQuality: ConnectionQuality;
	metadata?: string;
	attributes: Readonly<Record<string, string>>;
	audioTrackSids: ReadonlyArray<string>;
	videoTrackSids: ReadonlyArray<string>;
	isMicrophoneEnabled: boolean;
	isCameraEnabled: boolean;
	isScreenShareEnabled: boolean;
	joinedAt: number | null;
	lastSpokeAt: number | null;
}>;

const extractUserId = (identity: string): string | null => {
	const match = identity.match(/^user_(\d+)(?:_(.+))?$/);
	return match ? match[1] : null;
};

const extractConnectionId = (identity: string): string | null => {
	const match = identity.match(/^user_(\d+)_(.+)$/);
	return match ? match[2] : null;
};

const keysSorted = (m: Map<string, unknown>): ReadonlyArray<string> => Object.freeze([...m.keys()].sort());

const attrsClone = (a: Readonly<Record<string, string>>): Readonly<Record<string, string>> => Object.freeze({...a});

const arraysEqual = (a: ReadonlyArray<string>, b: ReadonlyArray<string>): boolean => {
	if (a === b) return true;
	if (a.length !== b.length) return false;
	for (let i = 0; i < a.length; i++) {
		if (a[i] !== b[i]) return false;
	}
	return true;
};

const createSnapshot = (p: Participant): LivekitParticipantSnapshot => ({
	identity: p.identity,
	userId: extractUserId(p.identity),
	connectionId: extractConnectionId(p.identity),
	sid: p.sid,
	isLocal: p.isLocal,
	isSpeaking: p.isSpeaking,
	connectionQuality: p.connectionQuality,
	metadata: p.metadata ?? undefined,
	attributes: attrsClone(p.attributes),
	audioTrackSids: keysSorted(p.audioTrackPublications),
	videoTrackSids: keysSorted(p.videoTrackPublications),
	isMicrophoneEnabled: p.isMicrophoneEnabled,
	isCameraEnabled: p.isCameraEnabled,
	isScreenShareEnabled: p.isScreenShareEnabled,
	joinedAt: p.joinedAt ? p.joinedAt.getTime() : null,
	lastSpokeAt: p.lastSpokeAt ? p.lastSpokeAt.getTime() : null,
});

const snapshotsEqual = (a: LivekitParticipantSnapshot | undefined, b: LivekitParticipantSnapshot): boolean => {
	if (!a) return false;
	return (
		a.identity === b.identity &&
		a.sid === b.sid &&
		a.isLocal === b.isLocal &&
		a.isSpeaking === b.isSpeaking &&
		a.connectionQuality === b.connectionQuality &&
		a.metadata === b.metadata &&
		a.isMicrophoneEnabled === b.isMicrophoneEnabled &&
		a.isCameraEnabled === b.isCameraEnabled &&
		a.isScreenShareEnabled === b.isScreenShareEnabled &&
		a.joinedAt === b.joinedAt &&
		a.lastSpokeAt === b.lastSpokeAt &&
		arraysEqual(a.audioTrackSids, b.audioTrackSids) &&
		arraysEqual(a.videoTrackSids, b.videoTrackSids) &&
		JSON.stringify(a.attributes) === JSON.stringify(b.attributes)
	);
};

class VoiceParticipantManager {
	private _participants: Readonly<Record<string, LivekitParticipantSnapshot>> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get participants(): Readonly<Record<string, LivekitParticipantSnapshot>> {
		return this._participants;
	}

	upsertParticipant(participant: Participant): void {
		const newSnap = createSnapshot(participant);
		const existing = this._participants[participant.identity];

		if (snapshotsEqual(existing, newSnap)) {
			return;
		}

		runInAction(() => {
			this._participants = {
				...this._participants,
				[participant.identity]: newSnap,
			};
		});

		logger.debug('[upsertParticipant] Updated', {identity: participant.identity, isLocal: participant.isLocal});
	}

	removeParticipant(identity: string): void {
		if (!(identity in this._participants)) {
			return;
		}

		runInAction(() => {
			const next = {...this._participants};
			delete next[identity];
			this._participants = next;
		});

		logger.debug('[removeParticipant] Removed', {identity});
	}

	hydrateFromRoom(room: Room): void {
		const next: Record<string, LivekitParticipantSnapshot> = {};

		if (room.localParticipant) {
			next[room.localParticipant.identity] = createSnapshot(room.localParticipant);
		}

		room.remoteParticipants.forEach((participant) => {
			next[participant.identity] = createSnapshot(participant);
		});

		runInAction(() => {
			this._participants = next;
		});

		logger.info('[hydrateFromRoom] Hydrated participants', {count: Object.keys(next).length});
	}

	updateActiveSpeakers(speakers: Array<Participant>): void {
		const speakerIds = new Set(speakers.map((s) => s.identity));
		let changed = false;
		const next = {...this._participants};

		for (const [identity, snap] of Object.entries(this._participants)) {
			const shouldBeSpeaking = speakerIds.has(identity);
			if (snap.isSpeaking !== shouldBeSpeaking) {
				next[identity] = {...snap, isSpeaking: shouldBeSpeaking};
				changed = true;
			}
		}

		if (changed) {
			runInAction(() => {
				this._participants = next;
			});
		}
	}

	getParticipantByUserIdAndConnectionId(
		userId: string,
		connectionId: string | null,
	): LivekitParticipantSnapshot | undefined {
		for (const participant of Object.values(this._participants)) {
			if (participant.userId === userId && participant.connectionId === connectionId) {
				return participant;
			}
		}
		return undefined;
	}

	getParticipant(identity: string): LivekitParticipantSnapshot | undefined {
		return this._participants[identity];
	}

	getLocalParticipant(): LivekitParticipantSnapshot | undefined {
		for (const participant of Object.values(this._participants)) {
			if (participant.isLocal) {
				return participant;
			}
		}
		return undefined;
	}

	clear(): void {
		runInAction(() => {
			this._participants = {};
		});
		logger.debug('[clear] All participants cleared');
	}

	extractUserId(identity: string): string | null {
		return extractUserId(identity);
	}

	extractConnectionId(identity: string): string | null {
		return extractConnectionId(identity);
	}
}

const instance = new VoiceParticipantManager();
(window as typeof window & {_voiceParticipantManager?: VoiceParticipantManager})._voiceParticipantManager = instance;
export default instance;
