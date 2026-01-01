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

import {useLingui} from '@lingui/react/macro';
import {GearIcon, MicrophoneIcon, MicrophoneSlashIcon, SpeakerHighIcon, SpeakerSlashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useLayoutEffect, useRef} from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as VoiceStateActionCreators from '~/actions/VoiceStateActionCreators';
import {getStatusTypeLabel} from '~/Constants';
import {CustomStatusDisplay} from '~/components/common/CustomStatusDisplay/CustomStatusDisplay';
import styles from '~/components/layout/UserArea.module.css';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import {UserAreaPopout} from '~/components/popouts/UserAreaPopout';
import {SettingsContextMenu} from '~/components/uikit/ContextMenu/SettingsContextMenu';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {FocusRingWrapper} from '~/components/uikit/FocusRingWrapper';
import {TooltipWithKeybind} from '~/components/uikit/KeybindHint/KeybindHint';
import {Popout} from '~/components/uikit/Popout/Popout';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {VoiceConnectionStatus} from '~/components/voice/VoiceConnectionStatus';
import {VoiceInputSettingsMenu, VoiceOutputSettingsMenu} from '~/components/voice/VoiceSettingsMenus';
import {useMediaDevices} from '~/hooks/useMediaDevices';
import {usePopout} from '~/hooks/usePopout';
import type {UserRecord} from '~/records/UserRecord';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import KeybindStore from '~/stores/KeybindStore';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PresenceStore from '~/stores/PresenceStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import {formatKeyCombo} from '~/utils/KeybindUtils';
import * as NicknameUtils from '~/utils/NicknameUtils';

const VOICE_CONNECTION_HEIGHT_VARIABLE = '--layout-voice-connection-height';

const UserAreaWithRoom = observer(function UserAreaWithRoom({user}: {user: UserRecord}) {
	const connectedGuildId = MediaEngineStore.guildId;
	const voiceState = MediaEngineStore.getVoiceState(connectedGuildId);
	const localSelfMute = LocalVoiceStateStore.selfMute;
	const localSelfDeaf = LocalVoiceStateStore.selfDeaf;

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

const UserAreaInner = observer(
	({
		user,
		isMuted,
		isDeafened,
		isGuildMuted = false,
		isGuildDeafened = false,
		muteReason = null,
	}: {
		user: UserRecord;
		isMuted: boolean;
		isDeafened: boolean;
		isGuildMuted?: boolean;
		isGuildDeafened?: boolean;
		muteReason?: 'guild' | 'push_to_talk' | 'self' | null;
	}) => {
		const {t, i18n} = useLingui();
		const {isOpen, openProps} = usePopout('user-area');
		const status = PresenceStore.getStatus(user.id);
		const customStatus = PresenceStore.getCustomStatus(user.id);
		const {inputDevices, outputDevices} = useMediaDevices();
		const voiceConnectionRef = useRef<HTMLDivElement | null>(null);

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

		const storeConnectedGuildId = MediaEngineStore.guildId;
		const storeConnectedChannelId = MediaEngineStore.channelId;
		const forceShowVoiceConnection = DeveloperOptionsStore.forceShowVoiceConnection;
		const hasVoiceConnection =
			!MobileLayoutStore.enabled &&
			(forceShowVoiceConnection || (!!storeConnectedGuildId && !!storeConnectedChannelId));

		useLayoutEffect(() => {
			const root = document.documentElement;

			if (!hasVoiceConnection) {
				root.style.removeProperty(VOICE_CONNECTION_HEIGHT_VARIABLE);
				return;
			}

			const height = voiceConnectionRef.current?.getBoundingClientRect().height ?? 0;
			if (height > 0) {
				root.style.setProperty(VOICE_CONNECTION_HEIGHT_VARIABLE, `${Math.round(height)}px`);
			} else {
				root.style.removeProperty(VOICE_CONNECTION_HEIGHT_VARIABLE);
			}

			return () => {
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

		return (
			<div className={wrapperClassName}>
				{hasVoiceConnection && (
					<>
						<div className={styles.separator} aria-hidden />
						<div ref={voiceConnectionRef} className={styles.voiceConnectionWrapper}>
							<VoiceConnectionStatus />
						</div>
						<div className={styles.separator} aria-hidden />
					</>
				)}
				{!hasVoiceConnection && <div className={styles.separator} aria-hidden />}
				<div className={styles.userAreaContainer}>
					<Popout {...openProps} render={() => <UserAreaPopout />} position="top">
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
								<TooltipWithKeybind
									label={
										isGuildMuted
											? t`Community Muted`
											: muteReason === 'push_to_talk'
												? t`Push-to-talk enabled — hold ${pushToTalkHint} to speak`
												: effectiveMuted
													? t`Unmute`
													: t`Mute`
									}
									action={isGuildMuted ? undefined : 'toggle_mute'}
								/>
							)}
						>
							<FocusRing offset={-2} enabled={!isGuildMuted}>
								<div>
									<button
										type="button"
										aria-label={isGuildMuted ? t`Community Muted` : effectiveMuted ? t`Unmute` : t`Mute`}
										className={clsx(
											styles.controlButton,
											(effectiveMuted || isGuildMuted) && styles.active,
											isGuildMuted && styles.disabled,
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
								<TooltipWithKeybind
									label={isGuildDeafened ? t`Community Deafened` : isDeafened ? t`Undeafen` : t`Deafen`}
									action={isGuildDeafened ? undefined : 'toggle_deafen'}
								/>
							)}
						>
							<FocusRing offset={-2} enabled={!isGuildDeafened}>
								<div>
									<button
										type="button"
										aria-label={isGuildDeafened ? t`Community Deafened` : isDeafened ? t`Undeafen` : t`Deafen`}
										className={clsx(
											styles.controlButton,
											(isDeafened || isGuildDeafened) && styles.active,
											isGuildDeafened && styles.disabled,
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
									type="button"
									aria-label={t`User Settings`}
									className={styles.controlButton}
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
	const room = MediaEngineStore.room;
	const connectedGuildId = MediaEngineStore.guildId;
	const voiceState = MediaEngineStore.getVoiceState(connectedGuildId);
	const localSelfMute = LocalVoiceStateStore.selfMute;
	const localSelfDeaf = LocalVoiceStateStore.selfDeaf;
	const isMobile = MobileLayoutStore.isMobileLayout();

	if (isMobile) {
		return null;
	}

	if (room) {
		return <UserAreaWithRoom user={user} />;
	}

	const isMuted = voiceState ? voiceState.self_mute : localSelfMute;
	const isDeafened = voiceState ? voiceState.self_deaf : localSelfDeaf;

	return <UserAreaInner user={user} isMuted={isMuted} isDeafened={isDeafened} />;
});
