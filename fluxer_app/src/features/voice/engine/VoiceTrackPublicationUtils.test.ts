// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';
import {releaseCaptureWithRepublishedTrack} from '@app/features/voice/engine/VoiceTrackPublicationUtils';
import {type LocalParticipant, type LocalTrack, type LocalTrackPublication, ParticipantEvent} from 'livekit-client';
import {describe, expect, it, vi} from 'vitest';

function fakeMediaStreamTrack(): MediaStreamTrack {
	return {readyState: 'live'} as MediaStreamTrack;
}

function fakeLocalTrack(mediaStreamTrack: MediaStreamTrack): LocalTrack {
	return {mediaStreamTrack, stop: vi.fn()} as unknown as LocalTrack;
}

function fakeRoom(): {participant: LocalParticipant; unpublish(publication: LocalTrackPublication): void} {
	const emitter = new EventEmitter();
	return {
		participant: emitter as unknown as LocalParticipant,
		unpublish: (publication) => emitter.emit(ParticipantEvent.LocalTrackUnpublished, publication),
	};
}

function republish(room: ReturnType<typeof fakeRoom>, sourceTrack: LocalTrack): LocalTrackPublication {
	const publication = {track: fakeLocalTrack(sourceTrack.mediaStreamTrack)} as LocalTrackPublication;
	releaseCaptureWithRepublishedTrack(room.participant, publication, sourceTrack);
	return publication;
}

function stopTrack(track: MediaStreamTrack): void {
	Object.assign(track, {readyState: 'ended'});
}

describe('releaseCaptureWithRepublishedTrack', () => {
	it('stops the processed microphone capture when its hot-swapped publication is stopped on leave', () => {
		const processedTrack = fakeMediaStreamTrack();
		const microphone = fakeLocalTrack(processedTrack);
		const newRoom = fakeRoom();
		const publication = republish(newRoom, microphone);

		stopTrack(processedTrack);
		newRoom.unpublish(publication);

		expect(microphone.stop).toHaveBeenCalledTimes(1);
	});

	it('keeps the capture running when the publication is handed to another room without stopping', () => {
		const processedTrack = fakeMediaStreamTrack();
		const microphone = fakeLocalTrack(processedTrack);
		const newRoom = fakeRoom();
		const publication = republish(newRoom, microphone);

		newRoom.unpublish(publication);

		expect(microphone.stop).not.toHaveBeenCalled();
	});

	it('stops the original capture after a second hot-swap republishes the wrapper track', () => {
		const processedTrack = fakeMediaStreamTrack();
		const microphone = fakeLocalTrack(processedTrack);
		const secondRoom = fakeRoom();
		const secondPublication = republish(secondRoom, microphone);
		const thirdRoom = fakeRoom();
		const thirdPublication = republish(thirdRoom, secondPublication.track!);

		secondRoom.unpublish(secondPublication);
		stopTrack(processedTrack);
		thirdRoom.unpublish(thirdPublication);

		expect(microphone.stop).toHaveBeenCalledTimes(1);
	});
});
