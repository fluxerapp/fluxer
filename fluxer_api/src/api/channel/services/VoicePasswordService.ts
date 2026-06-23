// SPDX-License-Identifier: AGPL-3.0-or-later

import {VoiceChannelPasswordIncorrectError} from '@fluxer/errors/src/domains/channel/VoiceChannelPasswordIncorrectError';
import {VoiceChannelPasswordRequiredError} from '@fluxer/errors/src/domains/channel/VoiceChannelPasswordRequiredError';
import type {ChannelDataService} from './ChannelDataService';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import {createChannelID} from '../../database/helpers/ChannelHelpers';
import type {UserID} from '../../models/User';
import type {ChannelID} from '../../models/Channel';

export interface ValidateVoiceChannelPasswordOptions {
	userId: UserID;
	channelId: ChannelID;
	password: string;
	requestCache: RequestCache;
}

export class VoicePasswordService {
	constructor(private readonly channelData: ChannelDataService) {}

	async validateVoiceChannelPassword(options: ValidateVoiceChannelPasswordOptions): Promise<void> {
		const {userId, channelId, password, requestCache} = options;
		const channel = await this.channelData.operations.getChannel({userId, channelId, requestCache});

		if (channel.voicePassword === null || channel.voicePassword === undefined) {
			return;
		}

		if (!password) {
			throw new VoiceChannelPasswordRequiredError();
		}

		if (channel.voicePassword !== password) {
			throw new VoiceChannelPasswordIncorrectError();
		}
	}
}
