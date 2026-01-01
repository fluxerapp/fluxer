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

import {action, makeAutoObservable, reaction} from 'mobx';
import {Logger} from '~/lib/Logger';
import ChannelStore from './ChannelStore';
import DimensionStore from './DimensionStore';
import ReadStateStore from './ReadStateStore';
import SelectedChannelStore from './SelectedChannelStore';
import WindowStore from './WindowStore';

const logger = new Logger('AutoAckStore');

type ChannelId = string;

class AutoAckStore {
	private readonly windowChannels = new Map<string, Set<ChannelId>>();

	private readonly windowConditions = new Map<
		string,
		{
			channelId: ChannelId | null;
			isAtBottom: boolean;
			canAutoAck: boolean;
		}
	>();

	constructor() {
		makeAutoObservable<this, 'windowChannels' | 'windowConditions'>(
			this,
			{
				windowChannels: false,
				windowConditions: false,
			},
			{autoBind: true},
		);

		this.setupReactions();
	}

	private setupReactions(): void {
		reaction(
			() => {
				const windowId = WindowStore.windowId;
				const channelId = SelectedChannelStore.currentChannelId;
				const isWindowFocused = WindowStore.focused;

				if (!channelId) {
					return {windowId, channelId: null, isAtBottom: false, canAutoAck: false};
				}

				const isAtBottom = DimensionStore.isAtBottom(channelId) ?? false;
				const readState = ReadStateStore.getIfExists(channelId);
				const isManualAck = readState?.isManualAck ?? false;

				const canAutoAck = !isManualAck && isWindowFocused;

				return {windowId, channelId, isAtBottom, canAutoAck};
			},
			(conditions) => {
				this.updateAutoAckState(conditions);
			},
			{
				name: 'AutoAckStore.updateAutoAckState',
				fireImmediately: true,
			},
		);
	}

	@action
	private updateAutoAckState(conditions: {
		windowId: string;
		channelId: ChannelId | null;
		isAtBottom: boolean;
		canAutoAck: boolean;
	}): void {
		const {windowId, channelId, isAtBottom, canAutoAck} = conditions;

		const prevConditions = this.windowConditions.get(windowId);
		this.windowConditions.set(windowId, {channelId, isAtBottom, canAutoAck});

		if (prevConditions?.channelId && prevConditions.channelId !== channelId) {
			this.disableAutomaticAckInternal(prevConditions.channelId, windowId);
		}

		if (!channelId) {
			return;
		}

		const shouldEnable = isAtBottom && canAutoAck;

		if (shouldEnable) {
			this.enableAutomaticAckInternal(channelId, windowId);
		} else {
			this.disableAutomaticAckInternal(channelId, windowId);
		}
	}

	@action
	private enableAutomaticAckInternal(channelId: ChannelId, windowId: string): void {
		const channel = ChannelStore.getChannel(channelId);
		if (channel == null) {
			logger.debug(`Ignoring enableAutomaticAck for non-existent channel ${channelId}`);
			return;
		}

		let channels = this.windowChannels.get(windowId);
		if (channels == null) {
			channels = new Set();
			this.windowChannels.set(windowId, channels);
		}

		if (!channels.has(channelId)) {
			channels.add(channelId);
			logger.debug(`Enabled automatic ack for ${channelId} in window ${windowId}`);
		}
	}

	@action
	private disableAutomaticAckInternal(channelId: ChannelId, windowId: string): void {
		const channels = this.windowChannels.get(windowId);
		if (channels == null) return;

		if (channels.has(channelId)) {
			channels.delete(channelId);
			logger.debug(`Disabled automatic ack for ${channelId} in window ${windowId}`);
		}

		if (channels.size === 0) {
			this.windowChannels.delete(windowId);
		}
	}

	isAutomaticAckEnabled(channelId: ChannelId): boolean {
		for (const channels of this.windowChannels.values()) {
			if (channels.has(channelId)) return true;
		}
		return false;
	}

	@action
	disableForChannel(channelId: ChannelId): void {
		for (const [windowId, channels] of this.windowChannels.entries()) {
			if (channels.has(channelId)) {
				channels.delete(channelId);
				logger.debug(`Force-disabled automatic ack for ${channelId} in window ${windowId}`);
			}
			if (channels.size === 0) {
				this.windowChannels.delete(windowId);
			}
		}
	}
}

export default new AutoAckStore();
