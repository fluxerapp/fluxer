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

import * as TypingActionCreators from '~/actions/TypingActionCreators';
import AuthenticationStore from '~/stores/AuthenticationStore';
import TypingStore from '~/stores/TypingStore';

class TypingManager {
	private currentChannelId: string | null = null;
	private nextSend: number | null = null;
	private timeoutId: NodeJS.Timeout | null = null;
	private hasStarted = false;

	typing(channelId: string): void {
		if (this.shouldReturn(channelId)) {
			return;
		}

		this.updateStateForTyping(channelId);

		if (!this.hasStarted || this.currentChannelId !== channelId) {
			const currentUserId = AuthenticationStore.currentUserId;
			if (!currentUserId) {
				return;
			}
			TypingActionCreators.startTyping(channelId, currentUserId);
			this.hasStarted = true;
		}

		if (this.timeoutId) {
			clearTimeout(this.timeoutId);
		}

		this.timeoutId = setTimeout(() => {
			this.sendTyping(channelId);
		}, 1500);

		this.nextSend = Date.now() + 10_000 * 0.8;
	}

	clear(channelId: string): void {
		if (this.currentChannelId !== channelId || !this.hasStarted) {
			return;
		}

		if (this.timeoutId) {
			clearTimeout(this.timeoutId);
			this.timeoutId = null;
		}

		const currentUserId = AuthenticationStore.currentUserId;
		if (!currentUserId) {
			return;
		}
		TypingActionCreators.stopTyping(channelId, currentUserId);

		this.resetState(channelId);
	}

	private shouldReturn(channelId: string): boolean {
		return this.currentChannelId === channelId && this.nextSend != null && this.nextSend > Date.now();
	}

	private updateStateForTyping(channelId: string): void {
		this.currentChannelId = channelId;
		const count = TypingStore.getCount(channelId);
		if (count > 5) {
			this.nextSend = Date.now() + 10_000;
		}
	}

	private sendTyping(channelId: string): void {
		TypingActionCreators.sendTyping(channelId);
		this.timeoutId = null;
	}

	private resetState(channelId: string): void {
		this.hasStarted = this.currentChannelId === channelId ? false : this.hasStarted;
		if (this.currentChannelId === channelId) {
			this.nextSend = null;
		}
	}
}

export const TypingUtils = new TypingManager();
