// SPDX-License-Identifier: AGPL-3.0-or-later

import {BaseChannelAuthService, type ChannelAuthOptions} from '../BaseChannelAuthService';

export class MessagePinAuthService extends BaseChannelAuthService {
	protected readonly options: ChannelAuthOptions = {
		errorOnMissingGuild: 'unknown_channel',
		validateNsfw: true,
	};
}
