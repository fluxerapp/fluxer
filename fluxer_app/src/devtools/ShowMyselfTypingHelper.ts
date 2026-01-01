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

import {autorun, type IReactionDisposer} from 'mobx';
import * as TypingActionCreators from '~/actions/TypingActionCreators';
import AuthenticationStore from '~/stores/AuthenticationStore';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';

const SELF_TYPING_REFRESH_MS = 5000;

class ShowMyselfTypingHelper {
	private intervalId: NodeJS.Timeout | null = null;
	private disposer: IReactionDisposer | null = null;
	private activeChannelId: string | null = null;

	start(): void {
		if (this.disposer) {
			return;
		}

		this.disposer = autorun(() => {
			const enabled = DeveloperOptionsStore.showMyselfTyping;
			const channelId = SelectedChannelStore.currentChannelId;
			const userId = AuthenticationStore.currentUserId;
			const shouldMirror = Boolean(enabled && channelId && userId);

			if (!shouldMirror) {
				this.reset();
				return;
			}

			if (channelId !== this.activeChannelId) {
				this.activeChannelId = channelId!;
				this.trigger(channelId!, userId!);
				this.restartInterval(channelId!, userId!);
				return;
			}

			if (!this.intervalId) {
				this.restartInterval(channelId!, userId!);
			}
		});
	}

	stop(): void {
		this.reset();
		if (this.disposer) {
			this.disposer();
			this.disposer = null;
		}
	}

	private trigger(channelId: string, userId: string): void {
		TypingActionCreators.startTyping(channelId, userId);
	}

	private restartInterval(channelId: string, userId: string): void {
		if (this.intervalId) {
			clearInterval(this.intervalId);
		}

		this.intervalId = setInterval(() => this.trigger(channelId, userId), SELF_TYPING_REFRESH_MS);
	}

	private reset(): void {
		if (this.intervalId) {
			clearInterval(this.intervalId);
			this.intervalId = null;
		}
		this.activeChannelId = null;
	}
}

export const showMyselfTypingHelper = new ShowMyselfTypingHelper();
