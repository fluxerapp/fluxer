// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import {Link} from '@app/features/platform/components/router/RouterReact';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {FocusRingWrapper} from '@app/features/ui/components/FocusRingWrapper';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {usePopout} from '@app/features/ui/hooks/usePopout';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import {SignalStrengthIcon} from '@app/features/voice/components/SignalStrengthIcon';
import styles from '@app/features/voice/components/VoiceConnectionStatus.module.css';
import {
	useVoiceParticipantAvatarEntries,
	VoiceParticipantSpeakingAvatarStack,
} from '@app/features/voice/components/VoiceParticipantAvatarList';
import {AudioProcessingButton} from '@app/features/voice/components/voice_connection_status/AudioProcessingButton';
import {AudioProcessingModal} from '@app/features/voice/components/voice_connection_status/AudioProcessingModal';
import {LocalParticipantControls} from '@app/features/voice/components/voice_connection_status/LocalParticipantControls';
import {
	getAudioProcessingTooltip,
	isAudioProcessingActive,
} from '@app/features/voice/components/voice_connection_status/shared';
import {VoiceDetailsPopout} from '@app/features/voice/components/voice_connection_status/VoiceDetailsPopout';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {readEffectiveNoiseSuppressionBackend} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionRuntime';
import {VOICE_DISCONNECT_DESCRIPTOR, VOICE_IN_CHAT_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {getActiveVoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowClockwiseIcon, DesktopIcon, DeviceMobileIcon, PhoneXIcon, UsersIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type MouseEvent as ReactMouseEvent, useCallback} from 'react';

const CONNECTING_DESCRIPTOR = msg({
	message: 'Connecting...',
	comment: 'Voice connection status while joining or reconnecting.',
	context: 'voice-connection-status',
});
const DISCONNECTED_DESCRIPTOR = msg({
	message: 'Disconnected',
	comment: 'Voice connection status when not connected.',
	context: 'voice-connection-status',
});
const CONNECTION_FAILED_DESCRIPTOR = msg({
	message: 'Connection failed',
	comment: 'Voice connection status when the most recent attempt to join the channel failed.',
	context: 'voice-connection-status',
});
const RETRY_DESCRIPTOR = msg({
	message: 'Try again',
	comment: 'Button that retries connecting to the voice channel after a failed attempt.',
	context: 'voice-connection-status',
});
const DISMISS_DESCRIPTOR = msg({
	message: 'Dismiss',
	comment: 'Button that dismisses the failed voice connection status banner.',
	context: 'voice-connection-status',
});
const SHOW_CALL_AVATARS_DESCRIPTOR = msg({
	message: 'Show call avatars',
	comment: 'Developer voice status menu option for displaying participant avatars.',
});
const SHOW_CONNECTION_ID_DESCRIPTOR = msg({
	message: 'Show connection ID',
	comment: 'Developer voice status menu option for displaying the voice connection identifier.',
});
const PING_MS_DESCRIPTOR = msg({
	message: 'Ping: {currentLatency}ms',
	comment: 'Compact latency badge on the voice control bar. {currentLatency} is an integer millisecond count.',
});
const MEASURING_LATENCY_DESCRIPTOR = msg({
	message: 'Measuring latency...',
	comment: 'Tooltip on the latency badge while voice latency is still being sampled.',
});
const JUMP_TO_DESCRIPTOR = msg({
	message: 'Jump to {channelSourceLabel}',
	comment:
		'Tooltip / aria label on a button that jumps to the channel. {channelSourceLabel} is the channel or DM name.',
});

interface ResolvedVoiceConnectionStatusProps {
	channel: Channel;
	connectedChannelId: string;
	connectedGuildId: string | null;
	guild: Guild | null;
}

const ResolvedVoiceConnectionStatusInner = observer(function ResolvedVoiceConnectionStatusInner({
	channel,
	connectedChannelId,
	connectedGuildId,
	guild,
}: ResolvedVoiceConnectionStatusProps) {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	const voiceState = MediaEngine.getCurrentUserVoiceState();
	const isConnecting = MediaEngine.connecting;
	const storeIsConnected = MediaEngine.connected;
	const voiceSettings = VoiceSettings;
	const processingMode = getActiveVoiceProcessingMode(voiceSettings);
	const noiseSuppressionBackend = readEffectiveNoiseSuppressionBackend();
	const isProcessingActive = isAudioProcessingActive(processingMode, noiseSuppressionBackend);
	const showVoiceConnectionId = voiceSettings.showVoiceConnectionId;
	const openNoiseSuppressionModal = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<AudioProcessingModal data-flx="voice.voice-connection-status.open-noise-suppression-modal.audio-processing-modal" />
			)),
		);
	}, []);
	const noiseSuppressionTooltip = getAudioProcessingTooltip(i18n, processingMode, noiseSuppressionBackend);
	const currentLatency = MediaEngine.currentLatency;
	const latencyHistory = MediaEngine.latencyHistory.slice(-30);
	const connectionId = MediaEngine.connectionId;
	const isMobile = voiceState?.is_mobile ?? false;
	const participantAvatarEntries = useVoiceParticipantAvatarEntries({
		guildId: connectedGuildId,
		channelId: connectedChannelId,
	});
	const showVoiceConnectionAvatarStack = voiceSettings.showVoiceConnectionAvatarStack;
	const {openProps: popoutProps} = usePopout('voice-details-popout');
	const isConnected = storeIsConnected;
	const handleVoiceConnectionStatusContextMenu = useCallback(
		(event: ReactMouseEvent<HTMLElement>) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<MenuGroup data-flx="voice.voice-connection-status.handle-voice-connection-status-context-menu.menu-group">
					<CheckboxItem
						icon={
							<UsersIcon
								weight="regular"
								className={styles.icon}
								data-flx="voice.voice-connection-status.handle-voice-connection-status-context-menu.icon"
							/>
						}
						checked={showVoiceConnectionAvatarStack}
						onCheckedChange={(checked) => {
							VoiceSettingsCommands.update({showVoiceConnectionAvatarStack: checked});
							onClose();
						}}
						data-flx="voice.voice-connection-status.handle-voice-connection-status-context-menu.checkbox-item"
					>
						{i18n._(SHOW_CALL_AVATARS_DESCRIPTOR)}
					</CheckboxItem>
					<CheckboxItem
						checked={showVoiceConnectionId}
						onCheckedChange={(checked) => {
							VoiceSettingsCommands.update({showVoiceConnectionId: checked});
							onClose();
						}}
						data-flx="voice.voice-connection-status.handle-voice-connection-status-context-menu.checkbox-item--2"
					>
						{i18n._(SHOW_CONNECTION_ID_DESCRIPTOR)}
					</CheckboxItem>
				</MenuGroup>
			));
		},
		[showVoiceConnectionAvatarStack, showVoiceConnectionId, i18n],
	);
	const isPrivateChannel = channel.isPrivate();
	const getStatusText = () => {
		if (isConnecting) return i18n._(CONNECTING_DESCRIPTOR);
		if (isConnected) return i18n._(VOICE_IN_CHAT_DESCRIPTOR);
		return i18n._(DISCONNECTED_DESCRIPTOR);
	};
	const getStatusClass = () => {
		if (isConnecting) return styles.statusConnecting;
		if (isConnected) return styles.statusConnected;
		return styles.statusDisconnected;
	};
	const disconnectLabel = i18n._(VOICE_DISCONNECT_DESCRIPTOR);
	const shouldShowVoiceConnectionAvatarStack =
		isConnected && showVoiceConnectionAvatarStack && participantAvatarEntries.length > 0;
	let channelRoute: string;
	if (isPrivateChannel) {
		channelRoute = Routes.dmChannel(channel.id);
	} else if (guild) {
		channelRoute = Routes.guildChannel(guild.id, channel.id);
	} else {
		return null;
	}
	const avatarGuildId = guild?.id ?? channel.guildId ?? null;
	const channelDisplayName = isPrivateChannel
		? ChannelUtils.getDMDisplayName(channel)
		: channel.name?.trim() || ChannelUtils.getName(channel);
	const guildDisplayName = guild?.name ?? '';
	const channelSourceLabel = isPrivateChannel ? channelDisplayName : `${channelDisplayName} / ${guildDisplayName}`;
	return (
		<div
			className={styles.voiceConnectionContainer}
			data-flx="voice.voice-connection-status.voice-connection-status-inner.voice-connection-container"
		>
			<div
				className={styles.statusRow}
				data-flx="voice.voice-connection-status.voice-connection-status-inner.status-row"
			>
				{isConnected && (
					<Tooltip
						text={
							currentLatency !== null
								? i18n._(PING_MS_DESCRIPTOR, {currentLatency})
								: i18n._(MEASURING_LATENCY_DESCRIPTOR)
						}
						data-flx="voice.voice-connection-status.voice-connection-status-inner.tooltip"
					>
						<div
							className={styles.signalIcon}
							data-flx="voice.voice-connection-status.voice-connection-status-inner.signal-icon"
						>
							<SignalStrengthIcon
								latency={currentLatency}
								latencyHistory={latencyHistory}
								size={16}
								data-flx="voice.voice-connection-status.voice-connection-status-inner.signal-strength-icon"
							/>
						</div>
					</Tooltip>
				)}
				<Popout
					data-flx="voice.voice-connection-status.voice-connection-status-inner.popout"
					{...popoutProps}
					position="top"
					offsetMainAxis={16}
					render={({onClose}) => (
						<VoiceDetailsPopout
							onClose={onClose}
							data-flx="voice.voice-connection-status.voice-connection-status-inner.voice-details-popout"
						/>
					)}
				>
					<FocusRingWrapper
						focusRingOffset={-2}
						data-flx="voice.voice-connection-status.voice-connection-status-inner.focus-ring-wrapper"
					>
						<button
							type="button"
							className={clsx(styles.statusButton, getStatusClass())}
							onContextMenu={handleVoiceConnectionStatusContextMenu}
							data-flx="voice.voice-connection-status.voice-connection-status-inner.status-button.voice-connection-status-context-menu"
						>
							{getStatusText()}
						</button>
					</FocusRingWrapper>
				</Popout>
				<div
					className={styles.controls}
					data-flx="voice.voice-connection-status.voice-connection-status-inner.controls"
				>
					<AudioProcessingButton
						active={isProcessingActive}
						label={noiseSuppressionTooltip}
						onClick={openNoiseSuppressionModal}
						pressed={isProcessingActive}
						data-flx="voice.voice-connection-status.voice-connection-status-inner.audio-processing-button.open-noise-suppression-modal"
					/>
					<Tooltip
						text={disconnectLabel}
						data-flx="voice.voice-connection-status.voice-connection-status-inner.tooltip--2"
					>
						<FocusRing offset={-2} data-flx="voice.voice-connection-status.voice-connection-status-inner.focus-ring">
							<button
								type="button"
								className={styles.controlButton}
								onClick={async () => {
									await MediaEngine.disconnectFromVoiceChannel();
								}}
								aria-label={disconnectLabel}
								data-flx="voice.voice-connection-status.voice-connection-status-inner.control-button"
							>
								<PhoneXIcon
									weight="fill"
									className={styles.icon}
									data-flx="voice.voice-connection-status.voice-connection-status-inner.icon"
								/>
							</button>
						</FocusRing>
					</Tooltip>
				</div>
			</div>
			<div
				className={styles.connectionInfo}
				data-flx="voice.voice-connection-status.voice-connection-status-inner.connection-info"
			>
				<div
					className={styles.channelSourceRow}
					data-flx="voice.voice-connection-status.voice-connection-status-inner.channel-source-row"
				>
					<FocusRing offset={-2} data-flx="voice.voice-connection-status.voice-connection-status-inner.focus-ring--2">
						<Link
							to={channelRoute}
							className={styles.channelSourceLink}
							aria-label={i18n._(JUMP_TO_DESCRIPTOR, {channelSourceLabel})}
							onContextMenu={handleVoiceConnectionStatusContextMenu}
							data-flx="voice.voice-connection-status.voice-connection-status-inner.channel-source-link.voice-connection-status-context-menu"
						>
							{isPrivateChannel ? (
								<span
									className={styles.channelSourceText}
									data-flx="voice.voice-connection-status.voice-connection-status-inner.channel-source-text"
								>
									{channelDisplayName}
								</span>
							) : (
								<span
									className={styles.channelSourceText}
									data-flx="voice.voice-connection-status.voice-connection-status-inner.channel-source-text--2"
								>
									<span
										className={styles.channelSourceChannel}
										data-flx="voice.voice-connection-status.voice-connection-status-inner.channel-source-channel"
									>
										{channelDisplayName}
									</span>
									<span
										className={styles.channelSourceSeparator}
										data-flx="voice.voice-connection-status.voice-connection-status-inner.channel-source-separator"
									>
										{' '}
										/{' '}
									</span>
									<span
										className={styles.channelSourceGuild}
										data-flx="voice.voice-connection-status.voice-connection-status-inner.channel-source-guild"
									>
										{guildDisplayName}
									</span>
								</span>
							)}
						</Link>
					</FocusRing>
				</div>
				{showVoiceConnectionId && connectionId && (
					<div
						className={styles.connectionIdRow}
						data-flx="voice.voice-connection-status.voice-connection-status-inner.connection-id-row"
					>
						{isMobile ? (
							<DeviceMobileIcon
								weight="regular"
								className={styles.connectionIdIcon}
								data-flx="voice.voice-connection-status.voice-connection-status-inner.connection-id-icon"
							/>
						) : (
							<DesktopIcon
								weight="regular"
								className={styles.connectionIdIcon}
								data-flx="voice.voice-connection-status.voice-connection-status-inner.connection-id-icon--2"
							/>
						)}
						<div
							className={styles.connectionIdValue}
							data-flx="voice.voice-connection-status.voice-connection-status-inner.connection-id-value"
						>
							<Tooltip
								text={connectionId}
								position="top"
								align="center"
								data-flx="voice.voice-connection-status.voice-connection-status-inner.tooltip--3"
							>
								<span
									className={styles.connectionIdValueText}
									data-flx="voice.voice-connection-status.voice-connection-status-inner.connection-id-value-text"
								>
									{connectionId}
								</span>
							</Tooltip>
						</div>
					</div>
				)}
				{shouldShowVoiceConnectionAvatarStack && (
					<div
						className={styles.channelAvatarStack}
						data-flx="voice.voice-connection-status.voice-connection-status-inner.channel-avatar-stack"
					>
						<VoiceParticipantSpeakingAvatarStack
							entries={participantAvatarEntries}
							guildId={avatarGuildId}
							channelId={channel.id}
							size={20}
							maxVisible={4}
							deduplicateUsers
							data-flx="voice.voice-connection-status.voice-connection-status-inner.voice-participant-speaking-avatar-stack"
						/>
					</div>
				)}
			</div>
			<div
				className={styles.mediaSection}
				data-flx="voice.voice-connection-status.voice-connection-status-inner.media-section"
			>
				<LocalParticipantControls data-flx="voice.voice-connection-status.voice-connection-status-inner.local-participant-controls" />
			</div>
		</div>
	);
});
const FailedVoiceConnectionStatus = observer(function FailedVoiceConnectionStatus() {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	const target = MediaEngine.connectFailedTarget;
	const handleRetry = useCallback(() => {
		void MediaEngine.retryFailedVoiceConnection();
	}, []);
	const handleDismiss = useCallback(() => {
		MediaEngine.dismissFailedVoiceConnection();
	}, []);
	if (!target) {
		return null;
	}
	const retryLabel = i18n._(RETRY_DESCRIPTOR);
	const dismissLabel = i18n._(DISMISS_DESCRIPTOR);
	return (
		<div
			className={styles.voiceConnectionContainer}
			data-flx="voice.voice-connection-status.failed-voice-connection-status.voice-connection-container"
		>
			<div
				className={styles.statusRow}
				data-flx="voice.voice-connection-status.failed-voice-connection-status.status-row"
			>
				<button
					type="button"
					className={clsx(styles.statusButton, styles.statusDisconnected)}
					onClick={handleRetry}
					data-flx="voice.voice-connection-status.failed-voice-connection-status.status-button"
				>
					{i18n._(CONNECTION_FAILED_DESCRIPTOR)}
				</button>
				<div
					className={styles.controls}
					data-flx="voice.voice-connection-status.failed-voice-connection-status.controls"
				>
					<Tooltip
						text={retryLabel}
						data-flx="voice.voice-connection-status.failed-voice-connection-status.tooltip-retry"
					>
						<FocusRing
							offset={-2}
							data-flx="voice.voice-connection-status.failed-voice-connection-status.focus-ring-retry"
						>
							<button
								type="button"
								className={styles.controlButton}
								onClick={handleRetry}
								aria-label={retryLabel}
								data-flx="voice.voice-connection-status.failed-voice-connection-status.retry-button"
							>
								<ArrowClockwiseIcon
									weight="bold"
									className={styles.icon}
									data-flx="voice.voice-connection-status.failed-voice-connection-status.retry-icon"
								/>
							</button>
						</FocusRing>
					</Tooltip>
					<Tooltip
						text={dismissLabel}
						data-flx="voice.voice-connection-status.failed-voice-connection-status.tooltip-dismiss"
					>
						<FocusRing
							offset={-2}
							data-flx="voice.voice-connection-status.failed-voice-connection-status.focus-ring-dismiss"
						>
							<button
								type="button"
								className={styles.controlButton}
								onClick={handleDismiss}
								aria-label={dismissLabel}
								data-flx="voice.voice-connection-status.failed-voice-connection-status.dismiss-button"
							>
								<XIcon
									weight="bold"
									className={styles.icon}
									data-flx="voice.voice-connection-status.failed-voice-connection-status.dismiss-icon"
								/>
							</button>
						</FocusRing>
					</Tooltip>
				</div>
			</div>
		</div>
	);
});

export const VoiceConnectionStatusInner = observer(() => {
	useMediaEngineVersion();
	const connectedGuildId = MediaEngine.guildId;
	const connectedChannelId = MediaEngine.channelId;
	if (MediaEngine.connectFailed) {
		return (
			<FailedVoiceConnectionStatus data-flx="voice.voice-connection-status.voice-connection-status-inner.failed-voice-connection-status" />
		);
	}
	if (!connectedChannelId) {
		return null;
	}
	const channel = Channels.getChannel(connectedChannelId);
	if (!channel) {
		return null;
	}
	const isPrivateChannel = channel.isPrivate();
	const guild = connectedGuildId ? (Guilds.getGuild(connectedGuildId) ?? null) : null;
	if (!isPrivateChannel && !guild) {
		return null;
	}
	return (
		<ResolvedVoiceConnectionStatusInner
			channel={channel}
			connectedChannelId={connectedChannelId}
			connectedGuildId={connectedGuildId}
			guild={guild}
			data-flx="voice.voice-connection-status.voice-connection-status-inner.resolved-voice-connection-status-inner"
		/>
	);
});
