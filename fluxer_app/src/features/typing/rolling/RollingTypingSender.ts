// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import Authentication from '@app/features/auth/state/Authentication';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import RollingTypingStore from '@app/features/typing/rolling/RollingTypingStore';
import {
	planTypingSend,
	TYPING_ROLLING_SKIP_ABOVE_TRACKED,
	type TypingSendSlot,
} from '@app/features/typing/rolling/TypingSendThrottle';

const logger = new Logger('Typing');

class RollingTypingSender {
	private slot: TypingSendSlot | null = null;

	startTyping(channelId: string): void {
		const userId = Authentication.currentUserId;
		if (userId == null) {
			return;
		}
		const now = Date.now();
		const plan = planTypingSend(this.slot, channelId, userId, now);
		if (plan.dropSlot) {
			this.dropSlot();
		}
		if (plan.action === 'throttled') {
			return;
		}
		const slot: TypingSendSlot = {channelId, userId, timeout: null, prevSend: now};
		slot.timeout = setTimeout(() => this.fire(slot), plan.delayMs);
		this.slot = slot;
		RollingTypingStore.start(channelId, userId, 'local');
	}

	stopTyping(channelId: string): void {
		const slot = this.slot;
		if (slot === null || slot.channelId !== channelId || slot.timeout === null) {
			return;
		}
		clearTimeout(slot.timeout);
		this.slot = null;
		RollingTypingStore.remove(channelId, slot.userId);
	}

	handleOwnMessageSent(channelId: string): void {
		this.dropSlot();
		const userId = Authentication.currentUserId;
		if (userId != null) {
			RollingTypingStore.remove(channelId, userId);
		}
	}

	reset(): void {
		this.dropSlot();
	}

	private dropSlot(): void {
		const slot = this.slot;
		if (slot === null) {
			return;
		}
		if (slot.timeout !== null) {
			clearTimeout(slot.timeout);
		}
		this.slot = null;
	}

	private fire(captured: TypingSendSlot): void {
		const slot = this.slot;
		if (slot !== captured || Authentication.currentUserId !== captured.userId || captured.timeout === null) {
			return;
		}
		captured.timeout = null;
		if (RollingTypingStore.countTypists(captured.channelId) > TYPING_ROLLING_SKIP_ABOVE_TRACKED) {
			return;
		}
		void this.postTyping(captured.channelId);
	}

	private async postTyping(channelId: string): Promise<void> {
		try {
			await http.post(Endpoints.CHANNEL_TYPING(channelId));
		} catch (error) {
			logger.error(`Failed to send typing indicator to channel ${channelId}:`, error);
		}
	}
}

export default new RollingTypingSender();
