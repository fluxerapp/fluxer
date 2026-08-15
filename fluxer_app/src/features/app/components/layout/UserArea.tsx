// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserAreaPopout} from '@app/features/app/components/floating/UserAreaPopout';
import styles from '@app/features/app/components/layout/UserArea.module.css';
import {
	selectUserAreaMicrophoneState,
	type UserAreaMuteReason,
} from '@app/features/app/components/layout/UserAreaState';
import {CustomStatusDisplay} from '@app/features/app/components/shared/custom_status_display/CustomStatusDisplay';
import {getStatusTypeLabel} from '@app/features/app/constants/AppConstants';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import * as VoiceStateCommands from '@app/features/devtools/commands/VoiceStateCommands';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import Keybind from '@app/features/input/state/InputKeybind';
import {formatKeyCombo} from '@app/features/input/utils/KeybindUtils';
import Presence from '@app/features/presence/state/Presence';
import {SettingsContextMenu} from '@app/features/ui/action_menu/SettingsContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {FocusRingWrapper} from '@app/features/ui/components/FocusRingWrapper';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {usePopout} from '@app/features/ui/hooks/usePopout';
import {TooltipWithKeybind} from '@app/features/ui/keybind_hint/KeybindHint';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {USER_SETTINGS_LABEL_DESCRIPTOR} from '@app/features/user/components/settings_utils/SettingsConstants';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {VoiceConnectionStatus} from '@app/features/voice/components/VoiceConnectionStatus';
import {VoiceAudioSettingsMenu} from '@app/features/voice/components/VoiceSettingsMenus';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {getEffectiveAudioState} from '@app/features/voice/engine/VoiceEffectiveAudioState';
import {useMediaDevices} from '@app/features/voice/hooks/useMediaDevices';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import {
	getVoiceDeafenedByModeratorsStatusLabel,
	VOICE_DEAFEN_SELF_DESCRIPTOR,
	VOICE_MUTED_BY_MODERATORS_DESCRIPTOR,
	VOICE_NO_SPEAK_PERMISSION_DESCRIPTOR,
	VOICE_UNDEAFEN_SELF_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {GearIcon, MicrophoneIcon, MicrophoneSlashIcon, SpeakerHighIcon, SpeakerSlashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useEffect, useRef} from 'react';

const PUSH_TO_TALK_IS_ON_HOLD_TO_SPEAK_DESCRIPTOR = msg({
	message: 'Push to talk is on. Hold {pushToTalkHint} to speak.',
	comment:
		'User-area voice status text shown when push-to-talk is active. The hint is the user-configured keybind label.',
});
const UNMUTE_MICROPHONE_DESCRIPTOR = msg({
	message: 'Unmute microphone',
	comment: 'Short label in the app layout user area.',
});
const MUTE_MICROPHONE_DESCRIPTOR = msg({
	message: 'Mute microphone',
	comment: 'Short label in the app layout user area.',
});
const USER_CONTROLS_DESCRIPTOR = msg({
	message: 'User controls',
	comment: 'Short label in the app layout user area.',
});
const OPEN_USER_MENU_FOR_DESCRIPTOR = msg({
	message: 'Open user menu for {displayName}',
	comment: 'Short label in the app layout user area. Preserve {displayName}; it is inserted by code.',
});
const VOICE_CONNECTION_HEIGHT_VARIABLE = '--layout-voice-connection-height';

function getResizeObserverEntryBlockSize(entry: ResizeObserverEntry): number {
	const borderBoxSize = entry.borderBoxSize;
	const firstBorderBoxSize = Array.isArray(borderBoxSize) ? borderBoxSize[0] : borderBoxSize;
	return firstBorderBoxSize?.blockSize ?? entry.contentRect.height;
}

interface UserAreaInnerProps {
	user: User;
	isMuted: boolean;
	isDeafened: boolean;
	isGuildMuted?: boolean;
	isGuildDeafened?: boolean;
	isPermissionMuted?: boolean;
	muteReason?: UserAreaMuteReason;
}

const UserAreaInner = observer(
	({
		user,
		isMuted,
		isDeafened,
		isGuildMuted = false,
		isGuildDeafened = false,
		isPermissionMuted = false,
		muteReason = null,
	}: UserAreaInnerProps) => {
		const {i18n} = useLingui();
		const {isOpen, openProps} = usePopout('user-area');
		const status = Presence.getStatus(user.id);
		const customStatus = Presence.getCustomStatus(user.id);
		const {inputDevices, outputDevices} = useMediaDevices();
		const voiceConnectionRef = useRef<HTMLDivElement | null>(null);
		const voiceConnectionHeightFrameRef = useRef<number | null>(null);
		const voiceConnectionHeightRef = useRef<number | null>(null);
		const micButtonRef = useRef<HTMLButtonElement | null>(null);
		const micRingRef = useRef<HTMLDivElement | null>(null);
		const speakerButtonRef = useRef<HTMLButtonElement | null>(null);
		const speakerRingRef = useRef<HTMLDivElement | null>(null);
		const settingsButtonRef = useRef<HTMLButtonElement | null>(null);
		const micContextMenuOpen = useContextMenuHoverState(micButtonRef);
		const speakerContextMenuOpen = useContextMenuHoverState(speakerButtonRef);
		const settingsContextMenuOpen = useContextMenuHoverState(settingsButtonRef);
		const handleMicContextMenu = (event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, (props) => (
				<VoiceAudioSettingsMenu
					inputDevices={inputDevices}
					outputDevices={outputDevices}
					onClose={props.onClose}
					data-flx="app.user-area.handle-mic-context-menu.voice-audio-settings-menu"
				/>
			));
		};
		const handleSpeakerContextMenu = (event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, (props) => (
				<VoiceAudioSettingsMenu
					inputDevices={inputDevices}
					outputDevices={outputDevices}
					onClose={props.onClose}
					data-flx="app.user-area.handle-speaker-context-menu.voice-audio-settings-menu"
				/>
			));
		};
		const handleSettingsClick = () => {
			ModalCommands.push(
				modal(() => <UserSettingsModal data-flx="app.user-area.handle-settings-click.user-settings-modal" />),
			);
		};
		const storeConnectedChannelId = MediaEngine.channelId;
		const forceShowVoiceConnection = DeveloperOptions.forceShowVoiceConnection;
		const hasVoiceConnection = !MobileLayout.enabled && (forceShowVoiceConnection || !!storeConnectedChannelId);
		useEffect(() => {
			const root = document.documentElement;
			const clearHeight = () => {
				voiceConnectionHeightRef.current = null;
				root.style.removeProperty(VOICE_CONNECTION_HEIGHT_VARIABLE);
			};
			if (!hasVoiceConnection) {
				clearHeight();
				return;
			}
			const element = voiceConnectionRef.current;
			if (!element || typeof ResizeObserver === 'undefined') {
				clearHeight();
				return;
			}
			let pendingHeight: number | null = null;
			const applyHeight = (height: number) => {
				const roundedHeight = Math.round(height);
				if (roundedHeight > 0) {
					if (voiceConnectionHeightRef.current !== roundedHeight) {
						voiceConnectionHeightRef.current = roundedHeight;
						root.style.setProperty(VOICE_CONNECTION_HEIGHT_VARIABLE, `${roundedHeight}px`);
					}
				} else {
					clearHeight();
				}
			};
			const flushHeight = () => {
				voiceConnectionHeightFrameRef.current = null;
				if (pendingHeight === null) return;
				applyHeight(pendingHeight);
				pendingHeight = null;
			};
			const scheduleHeight = (height: number) => {
				pendingHeight = height;
				if (voiceConnectionHeightFrameRef.current !== null) return;
				voiceConnectionHeightFrameRef.current = window.requestAnimationFrame(flushHeight);
			};
			const observer = new ResizeObserver((entries) => {
				const entry = entries[0];
				if (!entry) return;
				scheduleHeight(getResizeObserverEntryBlockSize(entry));
			});
			observer.observe(element);
			return () => {
				observer.disconnect();
				if (voiceConnectionHeightFrameRef.current !== null) {
					window.cancelAnimationFrame(voiceConnectionHeightFrameRef.current);
					voiceConnectionHeightFrameRef.current = null;
				}
				clearHeight();
			};
		}, [hasVoiceConnection]);
		const wrapperClassName = styles.userAreaInnerWrapper;
		const pushToTalkCombo = Keybind.getByAction('voice_push_to_talk').combo;
		const pushToTalkHint = formatKeyCombo(pushToTalkCombo);
		const isPushToTalkEffective = Keybind.isPushToTalkEffective();
		const microphoneState = selectUserAreaMicrophoneState({
			effectiveAudioMuted: isMuted,
			effectiveAudioDeafened: isDeafened,
			isGuildMuted,
			isGuildDeafened,
			isPermissionMuted,
			muteReason,
			isPushToTalkEffective,
			isPushToTalkHeld: Keybind.pushToTalkHeld,
			isPushToMuteEffective: Keybind.isPushToMuteEffective(),
			isPushToMuteHeld: Keybind.pushToMuteHeld,
		});
		const effectiveMuted = microphoneState.effectiveMuted;
		const isMuteToggleLocked = microphoneState.muteToggleLocked;
		const micTooltipLabel = (() => {
			if (isGuildDeafened) return getVoiceDeafenedByModeratorsStatusLabel(i18n, true);
			if (isGuildMuted) return i18n._(VOICE_MUTED_BY_MODERATORS_DESCRIPTOR);
			if (isPermissionMuted || muteReason === 'permission') return i18n._(VOICE_NO_SPEAK_PERMISSION_DESCRIPTOR);
			if (isPushToTalkEffective) return i18n._(PUSH_TO_TALK_IS_ON_HOLD_TO_SPEAK_DESCRIPTOR, {pushToTalkHint});
			if (effectiveMuted) return i18n._(UNMUTE_MICROPHONE_DESCRIPTOR);
			return i18n._(MUTE_MICROPHONE_DESCRIPTOR);
		})();
		const micAriaLabel = (() => {
			if (isGuildDeafened) return getVoiceDeafenedByModeratorsStatusLabel(i18n, true);
			if (isGuildMuted) return i18n._(VOICE_MUTED_BY_MODERATORS_DESCRIPTOR);
			if (isPermissionMuted || muteReason === 'permission') return i18n._(VOICE_NO_SPEAK_PERMISSION_DESCRIPTOR);
			if (isPushToTalkEffective) return i18n._(PUSH_TO_TALK_IS_ON_HOLD_TO_SPEAK_DESCRIPTOR, {pushToTalkHint});
			if (effectiveMuted) return i18n._(UNMUTE_MICROPHONE_DESCRIPTOR);
			return i18n._(MUTE_MICROPHONE_DESCRIPTOR);
		})();
		const speakerLabel = (() => {
			if (isGuildDeafened) return getVoiceDeafenedByModeratorsStatusLabel(i18n, true);
			if (isDeafened) return i18n._(VOICE_UNDEAFEN_SELF_DESCRIPTOR);
			return i18n._(VOICE_DEAFEN_SELF_DESCRIPTOR);
		})();
		const displayName = NicknameUtils.getNickname(user);
		return (
			<section
				className={wrapperClassName}
				aria-label={i18n._(USER_CONTROLS_DESCRIPTOR)}
				data-flx="app.user-area.user-area-inner.section"
			>
				{hasVoiceConnection && (
					<div ref={voiceConnectionRef} data-flx="app.user-area.user-area-inner.div">
						<div
							className={styles.voiceConnectionWrapper}
							data-flx="app.user-area.user-area-inner.voice-connection-wrapper"
						>
							<VoiceConnectionStatus data-flx="app.user-area.user-area-inner.voice-connection-status" />
						</div>
					</div>
				)}
				<div className={styles.userAreaContainer} data-flx="app.user-area.user-area-inner.user-area-container">
					<Popout
						data-flx="app.user-area.user-area-inner.popout"
						{...openProps}
						render={() => <UserAreaPopout data-flx="app.user-area.user-area-inner.user-area-popout" />}
						containerClass={styles.popoutContainer}
						position="top"
						offsetMainAxis={12}
					>
						<FocusRingWrapper focusRingOffset={-2} data-flx="app.user-area.user-area-inner.focus-ring-wrapper">
							<div
								className={clsx(styles.userInfo, isOpen && styles.active)}
								role="button"
								aria-label={i18n._(OPEN_USER_MENU_FOR_DESCRIPTOR, {displayName})}
								aria-haspopup="dialog"
								aria-expanded={isOpen}
								tabIndex={0}
								data-flx="app.user-area.user-area-inner.user-info"
							>
								<StatusAwareAvatar user={user} size={32} data-flx="app.user-area.user-area-inner.status-aware-avatar" />
								<div className={styles.userInfoText} data-flx="app.user-area.user-area-inner.user-info-text">
									<div className={styles.userName} data-flx="app.user-area.user-area-inner.user-name">
										{displayName}
									</div>
									<div className={styles.userStatus} data-flx="app.user-area.user-area-inner.user-status">
										<div
											className={clsx(styles.hoverRoll, isOpen && styles.forceHover)}
											data-flx="app.user-area.user-area-inner.hover-roll"
										>
											<div className={styles.hovered} data-flx="app.user-area.user-area-inner.hovered">
												{NicknameUtils.formatTagForStreamerMode(user.tag)}
											</div>
											<div className={styles.defaultState} data-flx="app.user-area.user-area-inner.default-state">
												{customStatus ? (
													<CustomStatusDisplay
														customStatus={customStatus}
														className={styles.userCustomStatus}
														showTooltip
														constrained
														animateOnParentHover
														data-flx="app.user-area.user-area-inner.user-custom-status"
													/>
												) : (
													<span
														className={styles.userStatusLabel}
														data-flx="app.user-area.user-area-inner.user-status-label"
													>
														{getStatusTypeLabel(i18n, status)}
													</span>
												)}
											</div>
										</div>
									</div>
								</div>
							</div>
						</FocusRingWrapper>
					</Popout>
					<div className={styles.controlsContainer} data-flx="app.user-area.user-area-inner.controls-container">
						<Tooltip
							text={() => (
								<TooltipWithKeybind
									label={micTooltipLabel}
									action={isMuteToggleLocked ? undefined : 'voice_toggle_mute'}
									data-flx="app.user-area.user-area-inner.tooltip-with-keybind"
								/>
							)}
							data-flx="app.user-area.user-area-inner.tooltip"
						>
							<FocusRing
								offset={-2}
								enabled={!isMuteToggleLocked}
								focusTarget={micButtonRef}
								ringTarget={micRingRef}
								data-flx="app.user-area.user-area-inner.focus-ring"
							>
								<div ref={micRingRef} data-flx="app.user-area.user-area-inner.div--2">
									<button
										ref={micButtonRef}
										type="button"
										aria-label={micAriaLabel}
										className={clsx(
											styles.controlButton,
											effectiveMuted && styles.active,
											isMuteToggleLocked && styles.disabled,
											micContextMenuOpen && styles.contextMenuHover,
										)}
										onClick={isMuteToggleLocked ? undefined : () => VoiceStateCommands.toggleSelfMute(null)}
										onContextMenu={handleMicContextMenu}
										disabled={isMuteToggleLocked}
										data-flx="app.user-area.user-area-inner.control-button.undefined"
									>
										{effectiveMuted ? (
											<MicrophoneSlashIcon
												weight="fill"
												className={styles.controlIcon}
												data-flx="app.user-area.user-area-inner.control-icon"
											/>
										) : (
											<MicrophoneIcon
												weight="fill"
												className={styles.controlIcon}
												data-flx="app.user-area.user-area-inner.control-icon--2"
											/>
										)}
									</button>
								</div>
							</FocusRing>
						</Tooltip>
						<Tooltip
							text={() => (
								<TooltipWithKeybind
									label={speakerLabel}
									action={isGuildDeafened ? undefined : 'voice_toggle_deafen'}
									data-flx="app.user-area.user-area-inner.tooltip-with-keybind--2"
								/>
							)}
							data-flx="app.user-area.user-area-inner.tooltip--2"
						>
							<FocusRing
								offset={-2}
								enabled={!isGuildDeafened}
								focusTarget={speakerButtonRef}
								ringTarget={speakerRingRef}
								data-flx="app.user-area.user-area-inner.focus-ring--2"
							>
								<div ref={speakerRingRef} data-flx="app.user-area.user-area-inner.div--3">
									<button
										ref={speakerButtonRef}
										type="button"
										aria-label={speakerLabel}
										className={clsx(
											styles.controlButton,
											(isDeafened || isGuildDeafened) && styles.active,
											isGuildDeafened && styles.disabled,
											speakerContextMenuOpen && styles.contextMenuHover,
										)}
										onClick={isGuildDeafened ? undefined : () => VoiceStateCommands.toggleSelfDeaf(null)}
										onContextMenu={handleSpeakerContextMenu}
										disabled={isGuildDeafened}
										data-flx="app.user-area.user-area-inner.control-button.undefined--2"
									>
										{isDeafened || isGuildDeafened ? (
											<SpeakerSlashIcon
												className={styles.controlIcon}
												data-flx="app.user-area.user-area-inner.control-icon--3"
											/>
										) : (
											<SpeakerHighIcon
												className={styles.controlIcon}
												data-flx="app.user-area.user-area-inner.control-icon--4"
											/>
										)}
									</button>
								</div>
							</FocusRing>
						</Tooltip>
						<Tooltip
							text={() => (
								<TooltipWithKeybind
									label={i18n._(USER_SETTINGS_LABEL_DESCRIPTOR)}
									action="system_toggle_settings"
									data-flx="app.user-area.user-area-inner.tooltip-with-keybind--3"
								/>
							)}
							data-flx="app.user-area.user-area-inner.tooltip--3"
						>
							<FocusRing offset={-2} data-flx="app.user-area.user-area-inner.focus-ring--3">
								<button
									ref={settingsButtonRef}
									type="button"
									aria-label={i18n._(USER_SETTINGS_LABEL_DESCRIPTOR)}
									className={clsx(styles.controlButton, settingsContextMenuOpen && styles.contextMenuHover)}
									onClick={handleSettingsClick}
									onContextMenu={(event) => {
										event.preventDefault();
										event.stopPropagation();
										ContextMenuCommands.openFromEvent(event, (props) => (
											<SettingsContextMenu
												onClose={props.onClose}
												data-flx="app.user-area.user-area-inner.settings-context-menu"
											/>
										));
									}}
									data-flx="app.user-area.user-area-inner.control-button.settings-click"
								>
									<GearIcon className={styles.controlIcon} data-flx="app.user-area.user-area-inner.control-icon--5" />
								</button>
							</FocusRing>
						</Tooltip>
					</div>
				</div>
			</section>
		);
	},
);
export const UserArea = observer(function UserArea({user}: {user: User}) {
	const connectedGuildId = MediaEngine.guildId;
	const voiceState = MediaEngine.getVoiceState(connectedGuildId);
	const localSelfMute = LocalVoiceState.getSelfMute();
	const localSelfDeaf = LocalVoiceState.getSelfDeaf();
	const isMobile = MobileLayout.isMobileLayout();
	if (isMobile) {
		return null;
	}
	const isGuildMuted = voiceState?.mute ?? false;
	const isGuildDeafened = voiceState?.deaf ?? false;
	const muteReason = MediaEngine.getMuteReason(voiceState);
	const isPermissionMuted = muteReason === 'permission';
	const effectiveAudioState = getEffectiveAudioState({
		selfMute: localSelfMute,
		selfDeaf: localSelfDeaf,
		serverMute: isGuildMuted || isPermissionMuted,
		serverDeaf: voiceState?.deaf,
	});
	return (
		<UserAreaInner
			user={user}
			isMuted={effectiveAudioState.effectiveMute}
			isDeafened={effectiveAudioState.effectiveDeaf}
			isGuildMuted={isGuildMuted}
			isGuildDeafened={isGuildDeafened}
			isPermissionMuted={isPermissionMuted}
			muteReason={muteReason}
			data-flx="app.user-area.user-area-inner"
		/>
	);
});
