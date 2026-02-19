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

import {getStatusTypeLabel} from '@app/AppConstants';
import * as ContextMenuActionCreators from '@app/actions/ContextMenuActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as VoiceStateActionCreators from '@app/actions/VoiceStateActionCreators';
import {CustomStatusDisplay} from '@app/components/common/custom_status_display/CustomStatusDisplay';
import styles from '@app/components/layout/UserArea.module.css';
import {UserSettingsModal} from '@app/components/modals/UserSettingsModal';
import {UserAreaPopout} from '@app/components/popouts/UserAreaPopout';
import {SettingsContextMenu} from '@app/components/uikit/context_menu/SettingsContextMenu';
import {FocusRingWrapper} from '@app/components/uikit/FocusRingWrapper';
import FocusRing from '@app/components/uikit/focus_ring/FocusRing';
import {TooltipWithKeybind} from '@app/components/uikit/keybind_hint/KeybindHint';
import {Popout} from '@app/components/uikit/popout/Popout';
import {StatusAwareAvatar} from '@app/components/uikit/StatusAwareAvatar';
import {Tooltip} from '@app/components/uikit/tooltip/Tooltip';
import {VoiceConnectionStatus} from '@app/components/voice/VoiceConnectionStatus';
import {VoiceInputSettingsMenu, VoiceOutputSettingsMenu} from '@app/components/voice/VoiceSettingsMenus';
import {useContextMenuHoverState} from '@app/hooks/useContextMenuHoverState';
import {useMediaDevices} from '@app/hooks/useMediaDevices';
import {usePopout} from '@app/hooks/usePopout';
import type {UserRecord} from '@app/records/UserRecord';
import DeveloperOptionsStore from '@app/stores/DeveloperOptionsStore';
import KeybindStore from '@app/stores/KeybindStore';
import LocalVoiceStateStore from '@app/stores/LocalVoiceStateStore';
import MobileLayoutStore from '@app/stores/MobileLayoutStore';
import PresenceStore from '@app/stores/PresenceStore';
import MediaEngineStore from '@app/stores/voice/MediaEngineFacade';
import {formatKeyCombo} from '@app/utils/KeybindUtils';
import * as NicknameUtils from '@app/utils/NicknameUtils';
import {useLingui} from '@lingui/react/macro';
import {GearIcon, MicrophoneIcon, MicrophoneSlashIcon, SpeakerHighIcon, SpeakerSlashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useLayoutEffect, useRef} from 'react';

const VOICE_CONNECTION_HEIGHT_VARIABLE = '--layout-voice-connection-height';

interface UserAreaInnerProps {
	user: UserRecord;
	isMuted: boolean;
	isDeafened: boolean;
	isGuildMuted?: boolean;
	isGuildDeafened?: boolean;
	muteReason?: 'guild' | 'push_to_talk' | 'self' | null;
}

const UserAreaInner = observer(
	({
		user,
		isMuted,
		isDeafened,
		isGuildMuted = false,
		isGuildDeafened = false,
		muteReason = null,
	}: UserAreaInnerProps) => {
		const {t, i18n} = useLingui();
		const {isOpen, openProps} = usePopout('user-area');
		const status = PresenceStore.getStatus(user.id);
		const customStatus = PresenceStore.getCustomStatus(user.id);
		const {inputDevices, outputDevices} = useMediaDevices();
		const voiceConnectionRef = useRef<HTMLDivElement | null>(null);
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
			ContextMenuActionCreators.openFromEvent(event, (props) => (
				<VoiceInputSettingsMenu inputDevices={inputDevices} onClose={props.onClose} />
			));
		};

		const handleSpeakerContextMenu = (event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuActionCreators.openFromEvent(event, (props) => (
				<VoiceOutputSettingsMenu outputDevices={outputDevices} onClose={props.onClose} />
			));
		};

		const handleSettingsClick = () => {
			ModalActionCreators.push(modal(() => <UserSettingsModal />));
		};

		const storeConnectedChannelId = MediaEngineStore.channelId;
		const forceShowVoiceConnection = DeveloperOptionsStore.forceShowVoiceConnection;
		const hasVoiceConnection = !MobileLayoutStore.enabled && (forceShowVoiceConnection || !!storeConnectedChannelId);

		useLayoutEffect(() => {
			const root = document.documentElement;

			if (!hasVoiceConnection) {
				root.style.removeProperty(VOICE_CONNECTION_HEIGHT_VARIABLE);
				return;
			}

			const element = voiceConnectionRef.current;
			if (!element) {
				root.style.removeProperty(VOICE_CONNECTION_HEIGHT_VARIABLE);
				return;
			}

			const updateHeight = () => {
				const height = element.getBoundingClientRect().height;
				if (height > 0) {
					root.style.setProperty(VOICE_CONNECTION_HEIGHT_VARIABLE, `${Math.round(height)}px`);
				} else {
					root.style.removeProperty(VOICE_CONNECTION_HEIGHT_VARIABLE);
				}
			};

			updateHeight();

			const observer = new ResizeObserver(updateHeight);
			observer.observe(element);

			return () => {
				observer.disconnect();
				root.style.removeProperty(VOICE_CONNECTION_HEIGHT_VARIABLE);
			};
		}, [hasVoiceConnection]);

		const wrapperClassName = clsx(
			styles.userAreaInnerWrapper,
			hasVoiceConnection && styles.userAreaInnerWrapperHasVoiceConnection,
		);

		const pushToTalkCombo = KeybindStore.getByAction('push_to_talk').combo;
		const pushToTalkHint = formatKeyCombo(pushToTalkCombo);
		const effectiveMuted = muteReason !== null || isMuted;

		const micTooltipLabel = (() => {
			if (isGuildMuted) return t`Community Muted`;
			if (muteReason === 'push_to_talk') return t`Push-to-talk enabled — hold ${pushToTalkHint} to speak`;
			if (effectiveMuted) return t`Unmute`;
			return t`Mute`;
		})();

		const micAriaLabel = (() => {
			if (isGuildMuted) return t`Community Muted`;
			if (effectiveMuted) return t`Unmute`;
			return t`Mute`;
		})();

		const speakerLabel = (() => {
			if (isGuildDeafened) return t`Community Deafened`;
			if (isDeafened) return t`Undeafen`;
			return t`Deafen`;
		})();

		return (
			<div className={wrapperClassName}>
				{hasVoiceConnection && (
					<div ref={voiceConnectionRef}>
						<div className={styles.separator} aria-hidden />
						<div className={styles.voiceConnectionWrapper}>
							<VoiceConnectionStatus />
						</div>
						<div className={styles.separator} aria-hidden />
					</div>
				)}
				{!hasVoiceConnection && <div className={styles.separator} aria-hidden />}
				<div className={styles.userAreaContainer}>
					<Popout {...openProps} render={() => <UserAreaPopout />} position="top" offsetMainAxis={12}>
						<FocusRingWrapper focusRingOffset={-2}>
							<div className={clsx(styles.userInfo, isOpen && styles.active)} role="button" tabIndex={0}>
								<StatusAwareAvatar user={user} size={36} />
								<div className={styles.userInfoText}>
									<div className={styles.userName}>{NicknameUtils.getNickname(user)}</div>
									<div className={styles.userStatus}>
										<div className={clsx(styles.hoverRoll, isOpen && styles.forceHover)}>
											<div className={styles.hovered}>{user.tag}</div>
											<div className={styles.defaultState}>
												{customStatus ? (
													<CustomStatusDisplay
														customStatus={customStatus}
														className={styles.userCustomStatus}
														showTooltip
														constrained
														animateOnParentHover
													/>
												) : (
													<span className={styles.userStatusLabel}>{getStatusTypeLabel(i18n, status)}</span>
												)}
											</div>
										</div>
									</div>
								</div>
							</div>
						</FocusRingWrapper>
					</Popout>

					<div className={styles.controlsContainer}>
						<Tooltip
							text={() => (
								<TooltipWithKeybind label={micTooltipLabel} action={isGuildMuted ? undefined : 'toggle_mute'} />
							)}
						>
							<FocusRing offset={-2} enabled={!isGuildMuted} focusTarget={micButtonRef} ringTarget={micRingRef}>
								<div ref={micRingRef}>
									<button
										ref={micButtonRef}
										type="button"
										aria-label={micAriaLabel}
										className={clsx(
											styles.controlButton,
											(effectiveMuted || isGuildMuted) && styles.active,
											isGuildMuted && styles.disabled,
											micContextMenuOpen && styles.contextMenuHover,
										)}
										onClick={isGuildMuted ? undefined : () => VoiceStateActionCreators.toggleSelfMute(null)}
										onContextMenu={handleMicContextMenu}
										disabled={isGuildMuted}
									>
										{effectiveMuted || isGuildMuted ? (
											<MicrophoneSlashIcon weight="fill" className={styles.controlIcon} />
										) : (
											<MicrophoneIcon weight="fill" className={styles.controlIcon} />
										)}
									</button>
								</div>
							</FocusRing>
						</Tooltip>
						<Tooltip
							text={() => (
								<TooltipWithKeybind label={speakerLabel} action={isGuildDeafened ? undefined : 'toggle_deafen'} />
							)}
						>
							<FocusRing
								offset={-2}
								enabled={!isGuildDeafened}
								focusTarget={speakerButtonRef}
								ringTarget={speakerRingRef}
							>
								<div ref={speakerRingRef}>
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
										onClick={isGuildDeafened ? undefined : () => VoiceStateActionCreators.toggleSelfDeaf(null)}
										onContextMenu={handleSpeakerContextMenu}
										disabled={isGuildDeafened}
									>
										{isDeafened || isGuildDeafened ? (
											<SpeakerSlashIcon className={styles.controlIcon} />
										) : (
											<SpeakerHighIcon className={styles.controlIcon} />
										)}
									</button>
								</div>
							</FocusRing>
						</Tooltip>
						<Tooltip text={() => <TooltipWithKeybind label={t`User Settings`} action="toggle_settings" />}>
							<FocusRing offset={-2}>
								<button
									ref={settingsButtonRef}
									type="button"
									aria-label={t`User Settings`}
									className={clsx(styles.controlButton, settingsContextMenuOpen && styles.contextMenuHover)}
									onClick={handleSettingsClick}
									onContextMenu={(event) => {
										event.preventDefault();
										event.stopPropagation();
										ContextMenuActionCreators.openFromEvent(event, (props) => (
											<SettingsContextMenu onClose={props.onClose} />
										));
									}}
								>
									<GearIcon className={styles.controlIcon} />
								</button>
							</FocusRing>
						</Tooltip>
					</div>
				</div>
			</div>
		);
	},
);

export const UserArea = observer(function UserArea({user}: {user: UserRecord}) {
	const connectedGuildId = MediaEngineStore.guildId;
	const voiceState = MediaEngineStore.getVoiceState(connectedGuildId);
	const localSelfMute = LocalVoiceStateStore.selfMute;
	const localSelfDeaf = LocalVoiceStateStore.selfDeaf;
	const isMobile = MobileLayoutStore.isMobileLayout();

	if (isMobile) {
		return null;
	}

	const isMuted = voiceState ? voiceState.self_mute : localSelfMute;
	const isDeafened = voiceState ? voiceState.self_deaf : localSelfDeaf;
	const isGuildMuted = voiceState?.mute ?? false;
	const isGuildDeafened = voiceState?.deaf ?? false;
	const muteReason = MediaEngineStore.getMuteReason(voiceState);

	return (
		<UserAreaInner
			user={user}
			isMuted={isMuted}
			isDeafened={isDeafened}
			isGuildMuted={isGuildMuted}
			isGuildDeafened={isGuildDeafened}
			muteReason={muteReason}
		/>
	);
});
