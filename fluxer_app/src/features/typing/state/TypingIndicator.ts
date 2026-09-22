// SPDX-License-Identifier: AGPL-3.0-or-later

import RollingTypingStore from '@app/features/typing/rolling/RollingTypingStore';
import type {Message} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

class TypingIndicator {
	startRemoteTyping(channelId: string, userId: string): void {
		RollingTypingStore.start(channelId, userId, 'gateway');
	}

	stopTypingOnMessageCreate(message: Message): void {
		RollingTypingStore.remove(message.channel_id, message.author.id);
	}

	reset(): void {
		RollingTypingStore.reset();
	}

	isTyping(channelId: string, userId: string): boolean {
		return RollingTypingStore.isTyping(channelId, userId);
	}

	isMemberListTyping(channelId: string, userId: string, currentUserId: string | null | undefined): boolean {
		if (currentUserId && userId === currentUserId) {
			return RollingTypingStore.isConfirmedTyping(channelId, userId);
		}
		return RollingTypingStore.isTyping(channelId, userId);
	}
}

export default new TypingIndicator();
