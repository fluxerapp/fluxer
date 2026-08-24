// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import {
	type AutoAckWindowSnapshot,
	createAutoAckWindowSnapshot,
	selectAutoAckWindowCommands,
	transitionAutoAckWindowSnapshot,
} from '@app/features/notification/state/NotificationAutoAckStateMachine';
import {isTextChatVisibleForAutoAck} from '@app/features/notification/utils/AutoAckVisibility';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {deferUntilModulesLoaded} from '@app/features/platform/utils/DeferUntilModulesLoaded';
import ReadStates from '@app/features/read_state/state/ReadStates';
import Dimension from '@app/features/ui/state/Dimension';
import MediaViewer from '@app/features/ui/state/MediaViewer';
import CompactVoiceCallHeight, {getGuildVoiceCallExpansionKey} from '@app/features/voice/state/CompactVoiceCallHeight';
import VoiceCallFullscreen from '@app/features/voice/state/VoiceCallFullscreen';
import Window from '@app/features/window/state/Window';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {action, makeAutoObservable, reaction} from 'mobx';

const logger = new Logger('AutoAck');

class AutoAck {
	private readonly windowChannels = new Map<string, Set<string>>();
	private readonly windowSnapshots = new Map<string, AutoAckWindowSnapshot>();

	constructor() {
		makeAutoObservable<this, 'windowChannels' | 'windowSnapshots'>(
			this,
			{
				windowChannels: false,
				windowSnapshots: false,
			},
			{autoBind: true},
		);
		this.setupReactions();
	}

	private setupReactions(): void {
		deferUntilModulesLoaded(() => {
			reaction(
				() => {
					const windowId = Window.windowId;
					const channelId = SelectedChannel.currentChannelId;
					const isWindowFocused = Window.focused;
					if (!channelId) {
						return {windowId, channelId: null, isAtBottom: false, canAutoAck: false};
					}
					const isAtBottom = Dimension.channelPinnedToEnd(channelId) ?? false;
					const readState = ReadStates.getIfExists(channelId);
					const ackedManually = readState?.ackedManually ?? false;
					const channel = Channels.getChannel(channelId);
					const isGuildVoiceCallExpanded =
						channel?.type === ChannelTypes.GUILD_VOICE
							? CompactVoiceCallHeight.getExpandedForKey(getGuildVoiceCallExpansionKey(channelId), true)
							: false;
					const isTextChatVisible = isTextChatVisibleForAutoAck({
						channelId,
						channelType: channel?.type,
						isGuildVoiceCallExpanded,
						activeVoiceCallFullscreenScopeKey: VoiceCallFullscreen.activeScopeKey,
					});
					const isMediaViewerOpen = MediaViewer.isOpen;
					const canAutoAck = !ackedManually && isWindowFocused && isTextChatVisible && !isMediaViewerOpen;
					return {windowId, channelId, isAtBottom, canAutoAck};
				},
				(conditions) => {
					this.updateAutoAckState(conditions);
				},
				{
					name: 'AutoAck.updateAutoAckState',
					fireImmediately: true,
				},
			);
		});
	}

	@action
	private updateAutoAckState(conditions: {
		windowId: string;
		channelId: string | null;
		isAtBottom: boolean;
		canAutoAck: boolean;
	}): void {
		const {windowId, channelId, isAtBottom, canAutoAck} = conditions;
		const previousSnapshot = this.windowSnapshots.get(windowId) ?? createAutoAckWindowSnapshot();
		const snapshot = transitionAutoAckWindowSnapshot(previousSnapshot, {
			type: 'autoAck.conditionsChanged',
			conditions: {channelId, isAtBottom, canAutoAck},
		});
		this.windowSnapshots.set(windowId, snapshot);
		for (const command of selectAutoAckWindowCommands(snapshot)) {
			switch (command.type) {
				case 'enable':
					this.enableAutomaticAckInternal(command.channelId, windowId);
					break;
				case 'disable':
					this.disableAutomaticAckInternal(command.channelId, windowId);
					break;
			}
		}
	}

	@action
	private enableAutomaticAckInternal(channelId: string, windowId: string): void {
		const channel = Channels.getChannel(channelId);
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
	private disableAutomaticAckInternal(channelId: string, windowId: string): void {
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

	isAutomaticAckEnabled(channelId: string): boolean {
		for (const channels of this.windowChannels.values()) {
			if (channels.has(channelId)) return true;
		}
		return false;
	}

	@action
	disableForChannel(channelId: string): void {
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

export default new AutoAck();
