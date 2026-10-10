// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {UserSettingsModal} from '@app/features/app/components/dialogs/LoadableSettingsModals';
import type {Channel} from '@app/features/channel/models/Channel';
import * as VoiceStateCommands from '@app/features/devtools/commands/VoiceStateCommands';
import type {Guild} from '@app/features/guild/models/Guild';
import {navigateToWithMobileHistory} from '@app/features/navigation/utils/MobileNavigation';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {DisconnectCallIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Button} from '@app/features/ui/button/Button';
import * as LayoutCommands from '@app/features/ui/commands/LayoutCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import styles from '@app/features/voice/components/bottomsheets/VoiceLobbyBottomSheet.module.css';
import {
	VoiceLobbyConnectionStats,
	VoiceLobbyControls,
} from '@app/features/voice/components/bottomsheets/VoiceLobbyShared';
import {CameraPreviewModalInRoom} from '@app/features/voice/components/modals/CameraPreviewModal';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {VOICE_CAMERA_USER_LIMIT_REACHED_DESCRIPTOR} from '@app/features/voice/engine/media_engine_facade/shared';
import {useCameraUserCapBlocked} from '@app/features/voice/hooks/useCameraUserCapBlocked';
import {usePendingVoiceConnection} from '@app/features/voice/hooks/usePendingVoiceConnection';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import {
	getVoiceDeafenedByModeratorsStatusLabel,
	VOICE_DEAFEN_DESCRIPTOR,
	VOICE_DISCONNECT_DESCRIPTOR,
	VOICE_MUTED_BY_MODERATORS_DESCRIPTOR,
	VOICE_NO_SPEAK_PERMISSION_DESCRIPTOR,
	VOICE_UNDEAFEN_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {isVoicePermissionMuteActive} from '@app/features/voice/utils/VoicePermissionUtils';
import {VOICE_CHANNEL_CAMERA_USER_LIMIT} from '@fluxer/constants/src/LimitConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const OPEN_CALL_VIEW_DESCRIPTOR = msg({
	message: 'Open call view',
	comment: 'Primary CTA in the mobile voice-channel lobby bottom sheet. Opens the full call UI when already connected.',
});
const CONNECT_TO_VOICE_DESCRIPTOR = msg({
	message: 'Connect to voice',
	comment: 'Primary CTA in the mobile voice-channel lobby bottom sheet. Joins the voice channel.',
});
const UNMUTE_DESCRIPTOR = msg({
	message: 'Unmute',
	context: 'voice-control-action',
	comment: 'Mic toggle button label in the mobile voice-channel lobby (currently muted).',
});
const MUTE_DESCRIPTOR = msg({
	message: 'Mute',
	context: 'voice-control-action',
	comment: 'Mic toggle button label in the mobile voice-channel lobby (currently unmuted).',
});
const CAMERA_OFF_DESCRIPTOR = msg({
	message: 'Camera off',
	comment: 'Camera toggle button label in the mobile voice-channel lobby (currently on).',
});
const CONNECTED_TO_VOICE_DESCRIPTOR = msg({
	message: 'Connected to voice',
	comment: 'Section header in the mobile voice-channel lobby when connected. Groups in-call controls.',
});
const YOU_RE_IN_THE_VOICE_CHANNEL_DESCRIPTOR = msg({
	message: "You're in the voice channel",
	comment: 'Subtitle below the connected-to-voice header confirming the user is in the channel.',
});
const PING_DESCRIPTOR = msg({
	message: 'Ping',
	comment: 'Voice stats field label (mobile voice lobby). Shows current latency in ms.',
});
const ENDPOINT_DESCRIPTOR = msg({
	message: 'Endpoint',
	comment: 'Voice stats field label (mobile voice lobby). Shows the voice server endpoint hostname.',
});
const CONNECTION_ID_DESCRIPTOR = msg({
	message: 'Connection ID',
	comment: 'Voice stats field label (mobile voice lobby). Developer / debug surface.',
});
const PACKET_LOSS_DESCRIPTOR = msg({
	message: 'Packet loss',
	comment: 'Voice stats field label (mobile voice lobby). Shows packet loss percentage.',
});
const JITTER_DESCRIPTOR = msg({
	message: 'Jitter',
	comment: 'Voice stats field label (mobile voice lobby). Shows network jitter in ms.',
});
const logger = new Logger('VoiceLobbyBottomSheet');

interface VoiceLobbyBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channel: Channel;
	guild: Guild;
}

export const VoiceLobbyBottomSheet = observer(function VoiceLobbyBottomSheet({
	isOpen,
	onClose,
	channel,
	guild,
}: VoiceLobbyBottomSheetProps) {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	const connectedGuildId = MediaEngine.guildId;
	const connectedChannelId = MediaEngine.channelId;
	const voiceState = MediaEngine.getCurrentUserVoiceState(connectedGuildId);
	const localSelfMute = LocalVoiceState.getSelfMute();
	const localSelfDeaf = LocalVoiceState.getSelfDeaf();
	const localSelfVideo = LocalVoiceState.getSelfVideo();
	const currentLatency = MediaEngine.currentLatency;
	const voiceStats = MediaEngine.voiceStats;
	const voiceServerEndpoint = MediaEngine.voiceServerEndpoint;
	const connectionId = MediaEngine.connectionId;
	const isConnected = connectedGuildId === guild.id && connectedChannelId === channel.id;
	const isPermissionMuted = isVoicePermissionMuteActive(voiceState, guild.id, channel.id);
	const isGuildMuted = voiceState?.mute ?? false;
	const isGuildDeafened = voiceState?.deaf ?? false;
	const isMuted =
		(voiceState ? voiceState.self_mute : localSelfMute) || isGuildMuted || isGuildDeafened || isPermissionMuted;
	const isDeafened = (voiceState ? voiceState.self_deaf : localSelfDeaf) || isGuildDeafened;
	const isCameraOn = voiceState ? Boolean(voiceState.self_video) : localSelfVideo;
	const cameraCapBlocked = useCameraUserCapBlocked(isCameraOn);
	const cameraCapBlockedLabel = i18n._(VOICE_CAMERA_USER_LIMIT_REACHED_DESCRIPTOR, {
		voiceChannelCameraUserLimit: VOICE_CHANNEL_CAMERA_USER_LIMIT,
	});
	const disconnectLabel = i18n._(VOICE_DISCONNECT_DESCRIPTOR);
	const isMicLocked = isGuildMuted || isGuildDeafened || isPermissionMuted;
	const muteLabel = (() => {
		if (isGuildDeafened) return getVoiceDeafenedByModeratorsStatusLabel(i18n, true);
		if (isGuildMuted) return i18n._(VOICE_MUTED_BY_MODERATORS_DESCRIPTOR);
		if (isPermissionMuted) return i18n._(VOICE_NO_SPEAK_PERMISSION_DESCRIPTOR);
		return isMuted ? i18n._(UNMUTE_DESCRIPTOR) : i18n._(MUTE_DESCRIPTOR);
	})();
	const deafenLabel = isGuildDeafened
		? getVoiceDeafenedByModeratorsStatusLabel(i18n, true)
		: i18n._(isDeafened ? VOICE_UNDEAFEN_DESCRIPTOR : VOICE_DEAFEN_DESCRIPTOR);
	const handleToggleMute = () => {
		VoiceStateCommands.toggleSelfMute(null);
	};
	const handleToggleDeafen = () => {
		VoiceStateCommands.toggleSelfDeaf(null);
	};
	const handleOpenVoiceSettings = () => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(
				() => (
					<UserSettingsModal
						initialTab="voice_video"
						data-flx="voice.voice-lobby-bottom-sheet.handle-open-voice-settings.user-settings-modal"
					/>
				),
				'user-settings',
			),
		);
	};
	const handleEnterCall = () => {
		onClose();
		const isMobile = MobileLayout.isMobileLayout();
		navigateToWithMobileHistory(Routes.guildChannel(guild.id, channel.id), isMobile);
		if (isMobile) {
			LayoutCommands.updateMobileLayoutState(false, true);
		}
	};
	const handleDisconnect = () => {
		onClose();
		void MediaEngine.disconnectFromVoiceChannel('user');
	};
	const handleVoiceConnected = useCallback(() => {
		onClose();
		const isMobile = MobileLayout.isMobileLayout();
		navigateToWithMobileHistory(Routes.guildChannel(guild.id, channel.id), isMobile);
		if (isMobile) {
			LayoutCommands.updateMobileLayoutState(false, true);
		}
	}, [guild.id, channel.id, onClose]);
	const {isPending: isConnecting, startConnection: handleConnect} = usePendingVoiceConnection({
		guildId: guild.id,
		channelId: channel.id,
		onConnected: handleVoiceConnected,
	});
	const handleConnectClick = useCallback(() => {
		handleConnect({skipConfirm: true});
	}, [handleConnect]);
	const handleToggleCamera = async () => {
		try {
			if (isCameraOn) {
				await MediaEngine.setCameraEnabled(false);
			} else {
				ModalCommands.push(
					modal(() => (
						<CameraPreviewModalInRoom data-flx="voice.voice-lobby-bottom-sheet.handle-toggle-camera.camera-preview-modal-in-room" />
					)),
				);
			}
		} catch (err) {
			logger.error('Failed to toggle camera:', err);
		}
	};
	return (
		<BottomSheet
			isOpen={isOpen}
			onClose={onClose}
			title={channel.name}
			data-flx="voice.voice-lobby-bottom-sheet.bottom-sheet"
		>
			<div className={styles.container} data-flx="voice.voice-lobby-bottom-sheet.container">
				<div className={styles.buttonRow} data-flx="voice.voice-lobby-bottom-sheet.button-row">
					{isConnected ? (
						<>
							<Button
								variant="primary"
								onClick={handleEnterCall}
								className={styles.fullWidth}
								data-flx="voice.voice-lobby-bottom-sheet.full-width.enter-call"
							>
								{i18n._(OPEN_CALL_VIEW_DESCRIPTOR)}
							</Button>
							<Button
								variant="danger"
								onClick={handleDisconnect}
								leftIcon={
									<DisconnectCallIcon size={18} data-flx="voice.voice-lobby-bottom-sheet.disconnect-call-icon" />
								}
								className={styles.fullWidth}
								data-flx="voice.voice-lobby-bottom-sheet.full-width.disconnect"
							>
								{disconnectLabel}
							</Button>
						</>
					) : (
						<Button
							variant="primary"
							onClick={handleConnectClick}
							className={styles.fullWidth}
							submitting={isConnecting}
							data-flx="voice.voice-lobby-bottom-sheet.full-width.connect"
						>
							{i18n._(CONNECT_TO_VOICE_DESCRIPTOR)}
						</Button>
					)}
				</div>
				<VoiceLobbyControls
					data-flx="voice.voice-lobby-bottom-sheet"
					muteButtonDataFlx="voice.voice-lobby-bottom-sheet.action-button.undefined"
					isConnected={isConnected}
					isMuted={isMuted}
					isDeafened={isDeafened}
					isCameraOn={isCameraOn}
					micLocked={isMicLocked}
					deafenLocked={isGuildDeafened}
					muteLabel={muteLabel}
					muteAriaLabel={muteLabel}
					deafenLabel={deafenLabel}
					deafenAriaLabel={deafenLabel}
					cameraCapBlocked={cameraCapBlocked}
					cameraCapBlockedLabel={cameraCapBlockedLabel}
					cameraOffLabel={i18n._(CAMERA_OFF_DESCRIPTOR)}
					onToggleMute={handleToggleMute}
					onToggleDeafen={handleToggleDeafen}
					onToggleCamera={handleToggleCamera}
					onOpenVoiceSettings={handleOpenVoiceSettings}
				/>
				{isConnected && (
					<VoiceLobbyConnectionStats
						data-flx="voice.voice-lobby-bottom-sheet"
						title={i18n._(CONNECTED_TO_VOICE_DESCRIPTOR)}
						subtitle={i18n._(YOU_RE_IN_THE_VOICE_CHANNEL_DESCRIPTOR)}
						labels={{
							ping: i18n._(PING_DESCRIPTOR),
							endpoint: i18n._(ENDPOINT_DESCRIPTOR),
							connectionId: i18n._(CONNECTION_ID_DESCRIPTOR),
							packetLoss: i18n._(PACKET_LOSS_DESCRIPTOR),
							jitter: i18n._(JITTER_DESCRIPTOR),
						}}
						currentLatency={currentLatency}
						voiceServerEndpoint={voiceServerEndpoint}
						connectionId={connectionId}
						audioPacketLoss={voiceStats?.audioPacketLoss}
						jitter={voiceStats?.jitter}
					/>
				)}
			</div>
		</BottomSheet>
	);
});
