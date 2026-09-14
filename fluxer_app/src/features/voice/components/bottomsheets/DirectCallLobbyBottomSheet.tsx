// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {useCallHeaderState} from '@app/features/channel/components/channel_view/useCallHeaderState';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import * as VoiceStateCommands from '@app/features/devtools/commands/VoiceStateCommands';
import {
	CAMERA_ON_DESCRIPTOR,
	INCOMING_CALL_DESCRIPTOR,
	SETTINGS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {navigateToWithMobileHistory} from '@app/features/navigation/utils/MobileNavigation';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	CameraOffIcon,
	CameraOnIcon,
	DeafenIcon,
	DisconnectCallIcon,
	MicrophoneOffIcon,
	MicrophoneOnIcon,
	SettingsIcon,
	UndeafenIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Button} from '@app/features/ui/button/Button';
import * as LayoutCommands from '@app/features/ui/commands/LayoutCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import * as CallCommands from '@app/features/voice/commands/CallCommands';
import styles from '@app/features/voice/components/bottomsheets/DirectCallLobbyBottomSheet.module.css';
import {CompactVoiceCallView} from '@app/features/voice/components/CompactVoiceCallView';
import {CameraPreviewModalInRoom} from '@app/features/voice/components/modals/CameraPreviewModal';
import {
	formatMilliseconds,
	formatPacketLossPercent,
} from '@app/features/voice/components/voice_connection_status/shared';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {VOICE_CAMERA_USER_LIMIT_REACHED_DESCRIPTOR} from '@app/features/voice/engine/media_engine_facade/shared';
import {useCameraUserCapBlocked} from '@app/features/voice/hooks/useCameraUserCapBlocked';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import {
	getOpenVoiceVideoSettingsLabel,
	INCOMING_CALL_ACCEPT_ACTION_DESCRIPTOR,
	INCOMING_CALL_IGNORE_ACTION_DESCRIPTOR,
	INCOMING_CALL_REJECT_ACTION_DESCRIPTOR,
	VOICE_CALL_DESCRIPTOR,
	VOICE_DEAFEN_DESCRIPTOR,
	VOICE_UNDEAFEN_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {VOICE_CHANNEL_CAMERA_USER_LIMIT} from '@fluxer/constants/src/LimitConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const CALL_AVAILABLE_DESCRIPTOR = msg({
	message: 'Call available',
	comment: 'Status label in the mobile direct-call lobby bottom sheet. Indicates an ongoing call you can join.',
});
const CONNECTING_DESCRIPTOR = msg({
	message: 'Connecting…',
	comment:
		'Status label in the mobile direct-call lobby bottom sheet while joining the call. Trailing ellipsis indicates in-progress.',
});
const IN_CALL_DESCRIPTOR = msg({
	message: 'In call',
	comment: 'Status label in the mobile direct-call lobby bottom sheet when the user is connected.',
});
const IN_CALL_ON_ANOTHER_DEVICE_DESCRIPTOR = msg({
	message: 'In call on another device',
	comment: 'Status label in the mobile direct-call lobby bottom sheet. The user joined this call from another device.',
});
const JOIN_CALL_DESCRIPTOR = msg({
	message: 'Join call',
	comment: 'Primary CTA button in the mobile direct-call lobby bottom sheet. Joins the active call.',
});
const OPEN_CALL_VIEW_DESCRIPTOR = msg({
	message: 'Open call view',
	comment:
		'Primary CTA button in the mobile direct-call lobby bottom sheet when already in the call. Opens the full call UI.',
});
const JOIN_ON_THIS_DEVICE_DESCRIPTOR = msg({
	message: 'Join on this device',
	comment: 'CTA button shown when the user is already in the call on another device. Moves the call to this device.',
});
const LEAVE_CALL_DESCRIPTOR = msg({
	message: 'Leave call',
	comment: 'Destructive button in the mobile direct-call lobby bottom sheet. Disconnects from the call.',
});
const UNMUTE_DESCRIPTOR = msg({
	message: 'Unmute',
	context: 'voice-control-action',
	comment: 'Mic toggle button label in the mobile direct-call lobby bottom sheet (currently muted).',
});
const MUTE_DESCRIPTOR = msg({
	message: 'Mute',
	context: 'voice-control-action',
	comment: 'Mic toggle button label in the mobile direct-call lobby bottom sheet (currently unmuted).',
});
const CAMERA_OFF_DESCRIPTOR = msg({
	message: 'Camera off',
	comment: 'Camera toggle button label in the mobile direct-call lobby bottom sheet (currently on).',
});
const CONNECTED_TO_CALL_DESCRIPTOR = msg({
	message: 'Connected to call',
	comment: 'Section header in the mobile direct-call lobby bottom sheet when connected. Groups in-call controls.',
});
const YOU_RE_IN_THE_CALL_DESCRIPTOR = msg({
	message: "You're in the call",
	comment: 'Subtitle below the connected-call header confirming the user is connected.',
});
const PING_DESCRIPTOR = msg({
	message: 'Ping',
	comment: 'Voice stats field label (mobile lobby). Shows current latency in ms.',
});
const ENDPOINT_DESCRIPTOR = msg({
	message: 'Endpoint',
	comment: 'Voice stats field label (mobile lobby). Shows the voice server endpoint hostname.',
});
const CONNECTION_ID_DESCRIPTOR = msg({
	message: 'Connection ID',
	comment: 'Voice stats field label (mobile lobby). Shows the voice connection identifier. Developer / debug surface.',
});
const PACKET_LOSS_DESCRIPTOR = msg({
	message: 'Packet loss',
	comment: 'Voice stats field label (mobile lobby). Shows packet loss percentage.',
});
const JITTER_DESCRIPTOR = msg({
	message: 'Jitter',
	comment: 'Voice stats field label (mobile lobby). Shows network jitter in ms.',
});
const logger = new Logger('DirectCallLobbyBottomSheet');

interface DirectCallLobbyBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channel: Channel;
}

export const DirectCallLobbyBottomSheet = observer(function DirectCallLobbyBottomSheet({
	isOpen,
	onClose,
	channel,
}: DirectCallLobbyBottomSheetProps) {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	const callHeaderState = useCallHeaderState(channel);
	const voiceState = MediaEngine.getCurrentUserVoiceState(channel.guildId ?? null);
	const localSelfMute = LocalVoiceState.getSelfMute();
	const localSelfDeaf = LocalVoiceState.getSelfDeaf();
	const localSelfVideo = LocalVoiceState.getSelfVideo();
	const currentLatency = MediaEngine.currentLatency;
	const voiceStats = MediaEngine.voiceStats;
	const voiceServerEndpoint = MediaEngine.voiceServerEndpoint;
	const connectionId = MediaEngine.connectionId;
	const isConnected = MediaEngine.connected && MediaEngine.channelId === channel.id;
	const isMuted = voiceState ? voiceState.self_mute : localSelfMute;
	const isDeafened = voiceState ? voiceState.self_deaf : localSelfDeaf;
	const isCameraOn = voiceState ? Boolean(voiceState.self_video) : localSelfVideo;
	const cameraCapBlocked = useCameraUserCapBlocked(isCameraOn);
	const cameraCapBlockedLabel = i18n._(VOICE_CAMERA_USER_LIMIT_REACHED_DESCRIPTOR, {
		voiceChannelCameraUserLimit: VOICE_CHANNEL_CAMERA_USER_LIMIT,
	});
	const callStatusLabel = useMemo(() => {
		switch (callHeaderState.controlsVariant) {
			case 'incoming':
				return i18n._(INCOMING_CALL_DESCRIPTOR);
			case 'join':
				return i18n._(CALL_AVAILABLE_DESCRIPTOR);
			case 'connecting':
				return i18n._(CONNECTING_DESCRIPTOR);
			case 'inCall':
				return callHeaderState.isDeviceInRoomForChannelCall
					? i18n._(IN_CALL_DESCRIPTOR)
					: i18n._(IN_CALL_ON_ANOTHER_DEVICE_DESCRIPTOR);
			default:
				return i18n._(VOICE_CALL_DESCRIPTOR);
		}
	}, [callHeaderState.controlsVariant, callHeaderState.isDeviceInRoomForChannelCall, i18n.locale]);
	const handleToggleMute = useCallback(() => {
		VoiceStateCommands.toggleSelfMute(null);
	}, []);
	const handleToggleDeafen = useCallback(() => {
		VoiceStateCommands.toggleSelfDeaf(null);
	}, []);
	const handleOpenVoiceSettings = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<UserSettingsModal
					initialTab="voice_video"
					data-flx="voice.direct-call-lobby-bottom-sheet.handle-open-voice-settings.user-settings-modal"
				/>
			)),
		);
	}, [onClose]);
	const handleOpenCallView = useCallback(() => {
		onClose();
		const isMobile = MobileLayout.isMobileLayout();
		navigateToWithMobileHistory(Routes.dmChannel(channel.id), isMobile);
		LayoutCommands.updateMobileLayoutState(false, true);
	}, [channel.id, onClose]);
	const handleDisconnect = useCallback(() => {
		onClose();
		void CallCommands.leaveCall(channel.id);
	}, [channel.id, onClose]);
	const handleRejectIncomingCall = useCallback(() => {
		CallCommands.rejectCall(channel.id);
	}, [channel.id]);
	const handleIgnoreIncomingCall = useCallback(() => {
		CallCommands.ignoreCall(channel.id);
	}, [channel.id]);
	const handleToggleCamera = useCallback(async () => {
		try {
			if (isCameraOn) {
				await MediaEngine.setCameraEnabled(false);
			} else {
				ModalCommands.push(
					modal(() => (
						<CameraPreviewModalInRoom data-flx="voice.direct-call-lobby-bottom-sheet.handle-toggle-camera.camera-preview-modal-in-room" />
					)),
				);
			}
		} catch (err) {
			logger.error('Failed to toggle camera:', err);
		}
	}, [isCameraOn]);
	const handlePrimaryAction = useCallback(() => {
		switch (callHeaderState.controlsVariant) {
			case 'incoming':
				CallCommands.joinCall(channel.id);
				return;
			case 'join':
				CallCommands.joinCall(channel.id);
				return;
			case 'connecting':
				return;
			case 'inCall':
				if (!callHeaderState.isDeviceInRoomForChannelCall) {
					CallCommands.joinCall(channel.id);
					return;
				}
				handleOpenCallView();
				return;
			default:
				return;
		}
	}, [callHeaderState.controlsVariant, callHeaderState.isDeviceInRoomForChannelCall, channel.id, handleOpenCallView]);
	const primaryButtonLabel = useMemo(() => {
		switch (callHeaderState.controlsVariant) {
			case 'incoming':
				return i18n._(INCOMING_CALL_ACCEPT_ACTION_DESCRIPTOR);
			case 'join':
				return i18n._(JOIN_CALL_DESCRIPTOR);
			case 'connecting':
				return i18n._(CONNECTING_DESCRIPTOR);
			case 'inCall':
				return callHeaderState.isDeviceInRoomForChannelCall
					? i18n._(OPEN_CALL_VIEW_DESCRIPTOR)
					: i18n._(JOIN_ON_THIS_DEVICE_DESCRIPTOR);
			default:
				return i18n._(VOICE_CALL_DESCRIPTOR);
		}
	}, [callHeaderState.controlsVariant, callHeaderState.isDeviceInRoomForChannelCall, i18n.locale]);
	const prettyEndpoint = useMemo(() => {
		if (!voiceServerEndpoint) return null;
		try {
			const url = new URL(voiceServerEndpoint);
			return url.port ? `${url.hostname}:${url.port}` : url.hostname;
		} catch {
			return voiceServerEndpoint;
		}
	}, [voiceServerEndpoint]);
	const shouldShowControls = callHeaderState.controlsVariant !== 'hidden';
	const shouldShowDisconnect = callHeaderState.controlsVariant === 'inCall';
	const title = useMemo(() => {
		if (channel.name) return channel.name;
		const dmName = ChannelUtils.getDMDisplayName(channel);
		return dmName || callStatusLabel;
	}, [channel, callStatusLabel]);
	const Row = useMemo(
		() =>
			observer(({label, value, valueClassName}: {label: string; value: React.ReactNode; valueClassName?: string}) => (
				<div className={styles.statRow} data-flx="voice.direct-call-lobby-bottom-sheet.row.stat-row">
					<span className={styles.statLabel} data-flx="voice.direct-call-lobby-bottom-sheet.row.stat-label">
						{label}
					</span>
					<div
						className={clsx(styles.statValue, valueClassName)}
						data-flx="voice.direct-call-lobby-bottom-sheet.row.stat-value"
					>
						{value}
					</div>
				</div>
			)),
		[],
	);
	if (!shouldShowControls) return null;
	return (
		<BottomSheet
			isOpen={isOpen}
			onClose={onClose}
			title={title}
			surface="primary"
			snapPoints={[0.35, 0.7, 0.95]}
			data-flx="voice.direct-call-lobby-bottom-sheet.bottom-sheet"
		>
			<div className={styles.container} data-flx="voice.direct-call-lobby-bottom-sheet.container">
				<div className={styles.buttonRow} data-flx="voice.direct-call-lobby-bottom-sheet.button-row">
					<Button
						variant={
							callHeaderState.controlsVariant === 'incoming' ||
							callHeaderState.controlsVariant === 'join' ||
							callHeaderState.controlsVariant === 'connecting' ||
							(callHeaderState.controlsVariant === 'inCall' && !callHeaderState.isDeviceInRoomForChannelCall)
								? 'secondary'
								: 'primary'
						}
						onClick={handlePrimaryAction}
						className={styles.fullWidth}
						submitting={callHeaderState.controlsVariant === 'connecting'}
						data-flx="voice.direct-call-lobby-bottom-sheet.full-width.primary-action"
					>
						{primaryButtonLabel}
					</Button>
					{callHeaderState.controlsVariant === 'incoming' && (
						<>
							<Button
								variant="danger"
								onClick={handleRejectIncomingCall}
								className={styles.fullWidth}
								data-flx="voice.direct-call-lobby-bottom-sheet.full-width.reject-incoming-call"
							>
								{i18n._(INCOMING_CALL_REJECT_ACTION_DESCRIPTOR)}
							</Button>
							<Button
								variant="secondary"
								onClick={handleIgnoreIncomingCall}
								className={styles.fullWidth}
								data-flx="voice.direct-call-lobby-bottom-sheet.full-width.ignore-incoming-call"
							>
								{i18n._(INCOMING_CALL_IGNORE_ACTION_DESCRIPTOR)}
							</Button>
						</>
					)}
					{shouldShowDisconnect && (
						<Button
							variant="danger"
							onClick={handleDisconnect}
							leftIcon={
								<DisconnectCallIcon size={18} data-flx="voice.direct-call-lobby-bottom-sheet.disconnect-call-icon" />
							}
							className={styles.fullWidth}
							data-flx="voice.direct-call-lobby-bottom-sheet.full-width.disconnect"
						>
							{i18n._(LEAVE_CALL_DESCRIPTOR)}
						</Button>
					)}
				</div>
				<div className={styles.statusRow} data-flx="voice.direct-call-lobby-bottom-sheet.status-row">
					<span className={styles.statusLabel} data-flx="voice.direct-call-lobby-bottom-sheet.status-label">
						{callStatusLabel}
					</span>
				</div>
				{callHeaderState.controlsVariant === 'inCall' && callHeaderState.isDeviceInRoomForChannelCall && (
					<div className={styles.callPreview} data-flx="voice.direct-call-lobby-bottom-sheet.call-preview">
						<CompactVoiceCallView
							channel={channel}
							hideHeader={true}
							data-flx="voice.direct-call-lobby-bottom-sheet.compact-voice-call-view"
						/>
					</div>
				)}
				<div className={styles.actionButtons} data-flx="voice.direct-call-lobby-bottom-sheet.action-buttons">
					<button
						type="button"
						className={styles.actionButton}
						onClick={handleToggleMute}
						aria-pressed={isMuted}
						data-flx="voice.direct-call-lobby-bottom-sheet.action-button.toggle-mute"
					>
						<div
							className={clsx(styles.iconContainer, isMuted ? styles.iconContainerDanger : styles.iconContainerBrand)}
							data-flx="voice.direct-call-lobby-bottom-sheet.icon-container"
						>
							{isMuted ? (
								<MicrophoneOffIcon
									className={styles.actionIcon}
									size={24}
									data-flx="voice.direct-call-lobby-bottom-sheet.action-icon"
								/>
							) : (
								<MicrophoneOnIcon
									className={styles.actionIcon}
									size={24}
									data-flx="voice.direct-call-lobby-bottom-sheet.action-icon--2"
								/>
							)}
						</div>
						<span className={styles.actionText} data-flx="voice.direct-call-lobby-bottom-sheet.action-text">
							{isMuted ? i18n._(UNMUTE_DESCRIPTOR) : i18n._(MUTE_DESCRIPTOR)}
						</span>
					</button>
					<button
						type="button"
						className={styles.actionButton}
						onClick={handleToggleDeafen}
						aria-pressed={isDeafened}
						data-flx="voice.direct-call-lobby-bottom-sheet.action-button.toggle-deafen"
					>
						<div
							className={clsx(
								styles.iconContainer,
								isDeafened ? styles.iconContainerDanger : styles.iconContainerTertiary,
							)}
							data-flx="voice.direct-call-lobby-bottom-sheet.icon-container--2"
						>
							{isDeafened ? (
								<DeafenIcon
									className={styles.actionIconSecondary}
									size={24}
									data-flx="voice.direct-call-lobby-bottom-sheet.action-icon-secondary"
								/>
							) : (
								<UndeafenIcon
									className={styles.actionIconSecondary}
									size={24}
									data-flx="voice.direct-call-lobby-bottom-sheet.action-icon-secondary--2"
								/>
							)}
						</div>
						<span className={styles.actionText} data-flx="voice.direct-call-lobby-bottom-sheet.action-text--2">
							{i18n._(isDeafened ? VOICE_UNDEAFEN_DESCRIPTOR : VOICE_DEAFEN_DESCRIPTOR)}
						</span>
					</button>
					{isConnected &&
						(() => {
							const cameraToggleButton = (
								<button
									type="button"
									className={styles.actionButton}
									onClick={cameraCapBlocked ? undefined : handleToggleCamera}
									disabled={cameraCapBlocked}
									aria-label={cameraCapBlocked ? cameraCapBlockedLabel : undefined}
									aria-pressed={isCameraOn}
									data-flx="voice.direct-call-lobby-bottom-sheet.action-button.toggle-camera"
								>
									<div
										className={clsx(
											styles.iconContainer,
											isCameraOn ? styles.iconContainerSuccess : styles.iconContainerTertiary,
										)}
										data-flx="voice.direct-call-lobby-bottom-sheet.icon-container--3"
									>
										{isCameraOn ? (
											<CameraOnIcon
												className={styles.actionIcon}
												size={24}
												data-flx="voice.direct-call-lobby-bottom-sheet.action-icon--3"
											/>
										) : (
											<CameraOffIcon
												className={styles.actionIconSecondary}
												size={24}
												data-flx="voice.direct-call-lobby-bottom-sheet.action-icon-secondary--3"
											/>
										)}
									</div>
									<span className={styles.actionText} data-flx="voice.direct-call-lobby-bottom-sheet.action-text--3">
										{isCameraOn ? i18n._(CAMERA_ON_DESCRIPTOR) : i18n._(CAMERA_OFF_DESCRIPTOR)}
									</span>
								</button>
							);
							if (!cameraCapBlocked) return cameraToggleButton;
							return (
								<Tooltip
									text={cameraCapBlockedLabel}
									data-flx="voice.direct-call-lobby-bottom-sheet.tooltip.camera-cap"
								>
									{cameraToggleButton}
								</Tooltip>
							);
						})()}
					<button
						type="button"
						className={styles.actionButton}
						onClick={handleOpenVoiceSettings}
						aria-label={getOpenVoiceVideoSettingsLabel(i18n)}
						data-flx="voice.direct-call-lobby-bottom-sheet.action-button.open-voice-settings"
					>
						<div
							className={clsx(styles.iconContainer, styles.iconContainerTertiary)}
							data-flx="voice.direct-call-lobby-bottom-sheet.icon-container--4"
						>
							<SettingsIcon
								className={styles.actionIconSecondary}
								size={24}
								data-flx="voice.direct-call-lobby-bottom-sheet.action-icon-secondary--4"
							/>
						</div>
						<span className={styles.actionText} data-flx="voice.direct-call-lobby-bottom-sheet.action-text--4">
							{i18n._(SETTINGS_DESCRIPTOR)}
						</span>
					</button>
				</div>
				{isConnected && (
					<div className={styles.connectionInfo} data-flx="voice.direct-call-lobby-bottom-sheet.connection-info">
						<div className={styles.connectionHeader} data-flx="voice.direct-call-lobby-bottom-sheet.connection-header">
							<div
								className={styles.connectionStatusInfo}
								data-flx="voice.direct-call-lobby-bottom-sheet.connection-status-info"
							>
								<div
									className={styles.connectionTitle}
									data-flx="voice.direct-call-lobby-bottom-sheet.connection-title"
								>
									{i18n._(CONNECTED_TO_CALL_DESCRIPTOR)}
								</div>
								<div
									className={styles.connectionSubtitle}
									data-flx="voice.direct-call-lobby-bottom-sheet.connection-subtitle"
								>
									{i18n._(YOU_RE_IN_THE_CALL_DESCRIPTOR)}
								</div>
							</div>
							<div
								className={styles.connectionStatusDot}
								aria-hidden="true"
								data-flx="voice.direct-call-lobby-bottom-sheet.connection-status-dot"
							/>
						</div>
						<div className={styles.statsGrid} data-flx="voice.direct-call-lobby-bottom-sheet.stats-grid">
							{currentLatency !== null && (
								<Row
									label={i18n._(PING_DESCRIPTOR)}
									value={
										<span
											className={styles.statValuePrimary}
											data-flx="voice.direct-call-lobby-bottom-sheet.stat-value-primary"
										>
											{formatMilliseconds(currentLatency, i18n.locale)}
										</span>
									}
									data-flx="voice.direct-call-lobby-bottom-sheet.row"
								/>
							)}
							{prettyEndpoint && (
								<Row
									label={i18n._(ENDPOINT_DESCRIPTOR)}
									value={
										<Tooltip text={prettyEndpoint} data-flx="voice.direct-call-lobby-bottom-sheet.tooltip">
											<span
												className={styles.endpointValue}
												data-flx="voice.direct-call-lobby-bottom-sheet.endpoint-value"
											>
												{prettyEndpoint}
											</span>
										</Tooltip>
									}
									valueClassName={styles.maxWidth}
									data-flx="voice.direct-call-lobby-bottom-sheet.row--2"
								/>
							)}
							{connectionId && (
								<Row
									label={i18n._(CONNECTION_ID_DESCRIPTOR)}
									value={
										<Tooltip text={connectionId} data-flx="voice.direct-call-lobby-bottom-sheet.tooltip--2">
											<span
												className={styles.connectionIdValue}
												data-flx="voice.direct-call-lobby-bottom-sheet.connection-id-value"
											>
												{connectionId}
											</span>
										</Tooltip>
									}
									valueClassName={styles.maxWidth}
									data-flx="voice.direct-call-lobby-bottom-sheet.row--3"
								/>
							)}
							{typeof voiceStats?.audioPacketLoss === 'number' && voiceStats.audioPacketLoss > 0 && (
								<Row
									label={i18n._(PACKET_LOSS_DESCRIPTOR)}
									value={
										<span
											className={styles.statValuePrimary}
											data-flx="voice.direct-call-lobby-bottom-sheet.stat-value-primary--2"
										>
											{formatPacketLossPercent(voiceStats.audioPacketLoss, i18n.locale)}
										</span>
									}
									data-flx="voice.direct-call-lobby-bottom-sheet.row--4"
								/>
							)}
							{typeof voiceStats?.jitter === 'number' && voiceStats.jitter > 0 && (
								<Row
									label={i18n._(JITTER_DESCRIPTOR)}
									value={
										<span
											className={styles.statValuePrimary}
											data-flx="voice.direct-call-lobby-bottom-sheet.stat-value-primary--3"
										>
											{formatMilliseconds(voiceStats.jitter, i18n.locale)}
										</span>
									}
									data-flx="voice.direct-call-lobby-bottom-sheet.row--5"
								/>
							)}
						</div>
					</div>
				)}
			</div>
		</BottomSheet>
	);
});
