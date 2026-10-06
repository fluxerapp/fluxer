// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	asVoiceTrackSource,
	type VoiceTrackSource,
	VoiceTrackSource as VoiceTrackSourceValue,
} from '@app/features/voice/engine/VoiceTrackSource';
import type {LocalParticipant, LocalTrack, LocalTrackPublication} from 'livekit-client';

interface LocalParticipantWithAudioPublications extends Pick<LocalParticipant, 'audioTrackPublications'> {}
interface LocalParticipantWithTrackPublications extends Pick<LocalParticipant, 'trackPublications'> {}
interface LocalParticipantWithUnpublish extends LocalParticipantWithTrackPublications {
	unpublishTrack: LocalParticipant['unpublishTrack'];
}

export type LocalMediaPublicationSlot = 'camera' | 'screen_share';

export interface LocalMediaPublicationCapResult {
	keptPublications: Array<LocalTrackPublication>;
	unpublishedPublications: Array<LocalTrackPublication>;
	failedPublications: Array<{
		publication: LocalTrackPublication;
		error: unknown;
	}>;
}

export function isMicrophoneSourcePublication(
	publication: Pick<LocalTrackPublication, 'source'> | null | undefined,
): boolean {
	return asVoiceTrackSource(publication?.source) === VoiceTrackSourceValue.Microphone;
}

export function isControllableMicrophonePublication(
	publication: Pick<LocalTrackPublication, 'source'> | null | undefined,
): boolean {
	if (!publication) {
		return false;
	}
	return isMicrophoneSourcePublication(publication);
}

export function getLocalMicrophonePublications(
	participant: LocalParticipantWithAudioPublications,
): Array<LocalTrackPublication> {
	return Array.from(participant.audioTrackPublications.values()).filter(
		(publication): publication is LocalTrackPublication => isControllableMicrophonePublication(publication),
	);
}

export function getPrimaryLocalMicrophonePublication(
	participant: LocalParticipantWithAudioPublications,
): LocalTrackPublication | null {
	return getLocalMicrophonePublications(participant)[0] ?? null;
}

export function getLocalTrackPublicationsBySource(
	participant: LocalParticipantWithTrackPublications,
	source: VoiceTrackSource,
): Array<LocalTrackPublication> {
	return Array.from(participant.trackPublications.values()).filter(
		(publication): publication is LocalTrackPublication => asVoiceTrackSource(publication?.source) === source,
	);
}

export function getLocalCameraPublications(
	participant: LocalParticipantWithTrackPublications,
): Array<LocalTrackPublication> {
	return getLocalTrackPublicationsBySource(participant, VoiceTrackSourceValue.Camera);
}

export function getLocalScreenShareVideoPublications(
	participant: LocalParticipantWithTrackPublications,
): Array<LocalTrackPublication> {
	return getLocalTrackPublicationsBySource(participant, VoiceTrackSourceValue.ScreenShare);
}

export function getLocalScreenShareAudioPublications(
	participant: LocalParticipantWithTrackPublications,
): Array<LocalTrackPublication> {
	return getLocalTrackPublicationsBySource(participant, VoiceTrackSourceValue.ScreenShareAudio);
}

export function getLocalScreenSharePublications(
	participant: LocalParticipantWithTrackPublications,
): Array<LocalTrackPublication> {
	return [...getLocalScreenShareVideoPublications(participant), ...getLocalScreenShareAudioPublications(participant)];
}

export function getLocalPublicationTrack(publication: LocalTrackPublication): LocalTrack | null {
	return (publication.track ?? publication.videoTrack ?? publication.audioTrack ?? null) as LocalTrack | null;
}

export function getLocalPublicationMediaStreamTrack(publication: LocalTrackPublication): MediaStreamTrack | null {
	return getLocalPublicationTrack(publication)?.mediaStreamTrack ?? null;
}

export function isLiveLocalTrackPublication(publication: LocalTrackPublication): boolean {
	const mediaStreamTrack = getLocalPublicationMediaStreamTrack(publication);
	return mediaStreamTrack != null && mediaStreamTrack.readyState !== 'ended';
}

function choosePrimaryLocalPublication(
	publications: ReadonlyArray<LocalTrackPublication>,
	preferredPublication?: LocalTrackPublication | null,
): LocalTrackPublication | null {
	if (
		preferredPublication &&
		publications.includes(preferredPublication) &&
		isLiveLocalTrackPublication(preferredPublication)
	) {
		return preferredPublication;
	}
	return publications.find(isLiveLocalTrackPublication) ?? null;
}

async function unpublishLocalPublication(
	participant: LocalParticipantWithUnpublish,
	publication: LocalTrackPublication,
	stopOnUnpublish?: boolean,
): Promise<boolean> {
	const track = getLocalPublicationTrack(publication);
	if (!track) {
		return false;
	}
	await participant.unpublishTrack(track, stopOnUnpublish);
	return true;
}

async function enforceSinglePublicationForSource(
	participant: LocalParticipantWithUnpublish,
	source: VoiceTrackSource,
	options: {
		preferredPublication?: LocalTrackPublication | null;
		stopOnUnpublish?: boolean;
	} = {},
): Promise<LocalMediaPublicationCapResult> {
	const publications = getLocalTrackPublicationsBySource(participant, source);
	const primary = choosePrimaryLocalPublication(publications, options.preferredPublication);
	const duplicates = primary ? publications.filter((publication) => publication !== primary) : publications;
	const result: LocalMediaPublicationCapResult = {
		keptPublications: primary ? [primary] : [],
		unpublishedPublications: [],
		failedPublications: [],
	};
	const settled = await Promise.allSettled(
		duplicates.map(async (publication) => ({
			publication,
			unpublished: await unpublishLocalPublication(participant, publication, options.stopOnUnpublish),
		})),
	);
	for (let i = 0; i < settled.length; i++) {
		const item = settled[i]!;
		if (item.status === 'fulfilled') {
			if (item.value.unpublished) {
				result.unpublishedPublications.push(item.value.publication);
			}
			continue;
		}
		const publication = duplicates[i];
		if (publication) {
			result.failedPublications.push({publication, error: item.reason});
		}
	}
	return result;
}

function mergeCapResults(...results: Array<LocalMediaPublicationCapResult>): LocalMediaPublicationCapResult {
	return {
		keptPublications: results.flatMap((result) => result.keptPublications),
		unpublishedPublications: results.flatMap((result) => result.unpublishedPublications),
		failedPublications: results.flatMap((result) => result.failedPublications),
	};
}

export async function enforceLocalMediaPublicationCap(
	participant: LocalParticipantWithUnpublish,
	slot: LocalMediaPublicationSlot,
	options: {
		preferredPublication?: LocalTrackPublication | null;
		stopOnUnpublish?: boolean;
	} = {},
): Promise<LocalMediaPublicationCapResult> {
	if (slot === 'camera') {
		return enforceSinglePublicationForSource(participant, VoiceTrackSourceValue.Camera, options);
	}
	return mergeCapResults(
		await enforceSinglePublicationForSource(participant, VoiceTrackSourceValue.ScreenShare, options),
		await enforceSinglePublicationForSource(participant, VoiceTrackSourceValue.ScreenShareAudio, options),
	);
}

export async function unpublishLocalMediaPublications(
	participant: LocalParticipantWithUnpublish,
	publications: ReadonlyArray<LocalTrackPublication>,
	options: {
		stopOnUnpublish?: boolean;
	} = {},
): Promise<LocalMediaPublicationCapResult> {
	const result: LocalMediaPublicationCapResult = {
		keptPublications: [],
		unpublishedPublications: [],
		failedPublications: [],
	};
	const settled = await Promise.allSettled(
		publications.map(async (publication) => ({
			publication,
			unpublished: await unpublishLocalPublication(participant, publication, options.stopOnUnpublish),
		})),
	);
	for (let i = 0; i < settled.length; i++) {
		const item = settled[i]!;
		const publication = publications[i]!;
		if (item.status === 'fulfilled') {
			if (item.value.unpublished) {
				result.unpublishedPublications.push(publication);
			}
			continue;
		}
		result.failedPublications.push({publication, error: item.reason});
	}
	return result;
}

export function selectLocalMediaPublicationsForConnectionRepublish(
	publications: ReadonlyArray<LocalTrackPublication>,
): Array<LocalTrackPublication> {
	const selected: Array<LocalTrackPublication> = [];
	const selectedSources = new Set<VoiceTrackSource>();
	const cappedSources = new Set<VoiceTrackSource>([
		VoiceTrackSourceValue.Camera,
		VoiceTrackSourceValue.ScreenShare,
		VoiceTrackSourceValue.ScreenShareAudio,
	]);
	for (const publication of publications) {
		const source = asVoiceTrackSource(publication.source);
		if (!cappedSources.has(source)) {
			selected.push(publication);
			continue;
		}
		if (selectedSources.has(source)) {
			continue;
		}
		if (!isLiveLocalTrackPublication(publication)) {
			continue;
		}
		selectedSources.add(source);
		selected.push(publication);
	}
	return selected;
}
