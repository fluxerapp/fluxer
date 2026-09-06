// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {type ChannelID, createUserID, type EntranceSoundID, type UserID} from '../../BrandedTypes';
import type {IChannelRepository} from '../../channel/IChannelRepository';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import {Logger} from '../../Logger';
import type {EntranceSoundService} from './EntranceSoundService';

interface PlayEntranceSoundParams {
	userId: UserID;
	channelId: ChannelID;
	soundId: EntranceSoundID;
}

export class EntranceSoundPlayService {
	constructor(
		private readonly entranceSoundService: EntranceSoundService,
		private readonly gatewayService: IGatewayService,
		private readonly channelRepository: IChannelRepository,
	) {}

	async play(params: PlayEntranceSoundParams): Promise<void> {
		const {userId, channelId, soundId} = params;
		const channel = await this.channelRepository.findUnique(channelId);
		const guildId = channel?.guildId ?? undefined;
		const voiceStates = channel
			? (await this.gatewayService.getVoiceStatesForChannel({guildId, channelId})).voiceStates
			: [];
		const senderInChannel = voiceStates.some((state) => state.userId === userId.toString());
		if (!senderInChannel) {
			throw InputValidationError.fromCode('channel_id', ValidationErrorCodes.ENTRANCE_SOUND_INVALID_SCOPE);
		}
		const library = await this.entranceSoundService.getSoundWithUrl(userId, soundId);
		if (!library) {
			throw InputValidationError.fromCode('sound_id', ValidationErrorCodes.ENTRANCE_SOUND_NOT_FOUND);
		}
		const eventData = {
			user_id: userId.toString(),
			channel_id: channelId.toString(),
			guild_id: guildId ? guildId.toString() : null,
			sound_id: soundId.toString(),
			hash: library.sound.hash,
			url: library.url,
			duration_ms: library.sound.durationMs,
			content_type: library.sound.contentType,
		};
		const senderIdString = userId.toString();
		const deliveredTo = new Set<string>();
		for (const state of voiceStates) {
			if (state.userId === senderIdString) continue;
			if (deliveredTo.has(state.userId)) continue;
			deliveredTo.add(state.userId);
			try {
				await this.gatewayService.dispatchPresence({
					userId: createUserID(BigInt(state.userId)),
					event: 'ENTRANCE_SOUND_PLAY',
					data: eventData,
				});
			} catch (error) {
				Logger.warn(
					{error, recipient: state.userId, channelId: channelId.toString()},
					'Failed to dispatch ENTRANCE_SOUND_PLAY',
				);
			}
		}
	}
}
