// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID} from '@app/api/BrandedTypes';
import type {MessageReference} from '@app/api/database/types/MessageTypes';
import type {MessageReferenceTypeValue} from '@fluxer/constants/src/ChannelConstants';

export class MessageRef {
	readonly channelId: ChannelID;
	readonly messageId: MessageID;
	readonly guildId: GuildID | null;
	readonly type: MessageReferenceTypeValue;

	constructor(ref: MessageReference) {
		this.channelId = ref.channel_id;
		this.messageId = ref.message_id;
		this.guildId = ref.guild_id ?? null;
		this.type = ref.type as MessageReferenceTypeValue;
	}

	toMessageReference(): MessageReference {
		return {
			channel_id: this.channelId,
			message_id: this.messageId,
			guild_id: this.guildId,
			type: this.type,
		};
	}
}
