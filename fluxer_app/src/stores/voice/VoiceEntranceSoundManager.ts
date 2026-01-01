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

import type {Room} from 'livekit-client';
import {Track} from 'livekit-client';
import * as SoundActionCreators from '~/actions/SoundActionCreators';
import {Logger} from '~/lib/Logger';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import UserStore from '~/stores/UserStore';
import * as CustomSoundDB from '~/utils/CustomSoundDB';
import {SoundType} from '~/utils/SoundUtils';

const logger = new Logger('VoiceEntranceSoundManager');

export async function playEntranceSound(room: Room | null): Promise<void> {
	const user = UserStore.getCurrentUser();
	if (!user?.isPremium()) {
		SoundActionCreators.playSound(SoundType.UserJoin);
		return;
	}

	try {
		if (LocalVoiceStateStore.getSelfMute() || LocalVoiceStateStore.getSelfDeaf()) {
			SoundActionCreators.playSound(SoundType.UserJoin);
			return;
		}

		const entranceSound = await CustomSoundDB.getEntranceSound();
		if (!entranceSound) {
			SoundActionCreators.playSound(SoundType.UserJoin);
			return;
		}

		if (!room?.localParticipant) {
			SoundActionCreators.playSound(SoundType.UserJoin);
			return;
		}

		const audioContext = new AudioContext();
		const arrayBuffer = await entranceSound.blob.arrayBuffer();
		const audioBuffer = await audioContext.decodeAudioData(arrayBuffer);
		const source = audioContext.createBufferSource();
		source.buffer = audioBuffer;
		source.connect(audioContext.destination);

		const dest = audioContext.createMediaStreamDestination();
		source.connect(dest);

		const audioTrack = dest.stream.getAudioTracks()[0];
		if (!audioTrack) {
			SoundActionCreators.playSound(SoundType.UserJoin);
			await audioContext.close();
			return;
		}

		const participant = room.localParticipant;
		await participant.publishTrack(audioTrack, {
			name: 'entrance-sound',
			source: Track.Source.Microphone,
		});
		source.start();
		source.onended = async () => {
			try {
				const pubs = Array.from(participant.audioTrackPublications.values());
				const pub = pubs.find((p) => p.trackName === 'entrance-sound');
				if (pub?.track) {
					await participant.unpublishTrack(pub.track);
				}
				await audioContext.close();
			} catch (e) {
				logger.error('[playEntranceSound] Cleanup failed', e);
			}
		};
		logger.info('[playEntranceSound] Custom entrance sound played');
	} catch (e) {
		logger.error('[playEntranceSound] Failed', e);
		SoundActionCreators.playSound(SoundType.UserJoin);
	}
}
