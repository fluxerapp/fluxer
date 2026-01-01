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
import {CaretDownIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import {PreloadableUserPopout} from '~/components/channel/PreloadableUserPopout';
import {AvatarWithPresence} from '~/components/uikit/avatars/AvatarWithPresence';
import {VoiceParticipantContextMenu} from '~/components/uikit/ContextMenu/VoiceParticipantContextMenu';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip';
import type {UserRecord} from '~/records/UserRecord';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import UserStore from '~/stores/UserStore';
import type {VoiceState} from '~/stores/voice/MediaEngineFacade';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import * as NicknameUtils from '~/utils/NicknameUtils';
import styles from './GroupedVoiceParticipant.module.css';
import {VoiceParticipantItem} from './VoiceParticipantItem';
import {VoiceStateIcons} from './VoiceStateIcons';

interface GroupedVoiceParticipantProps {
	user: UserRecord;
	voiceStates: Array<VoiceState>;
	guildId: string;
	anySpeaking?: boolean;
}

export const GroupedVoiceParticipant = observer(function GroupedVoiceParticipant({
	user,
	voiceStates,
	guildId,
	anySpeaking: propAnySpeaking,
}: GroupedVoiceParticipantProps) {
	const {t} = useLingui();
	const [isExpanded, setIsExpanded] = React.useState(false);
	const currentUser = UserStore.getCurrentUser();
	const isCurrentUser = currentUser?.id === user.id;
	const currentConnectionId = MediaEngineStore.connectionId;
	const localSelfVideo = LocalVoiceStateStore.selfVideo;
	const localSelfStream = LocalVoiceStateStore.selfStream;

	const toggleExpanded = React.useCallback(() => setIsExpanded((prev) => !prev), []);

	const handleContextMenu = React.useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<VoiceParticipantContextMenu
					user={user}
					participantName={NicknameUtils.getNickname(user)}
					onClose={onClose}
					guildId={guildId}
					isGroupedItem={true}
					isParentGroupedItem={true}
				/>
			));
		},
		[user, guildId],
	);

	const connectionCount = voiceStates.length;

	const stateAgg = React.useMemo(() => {
		let anyCameraOn = false;
		let anyLive = false;
		let guildMuted = false;
		let guildDeaf = false;
		let allSelfMuted = true;
		let allSelfDeaf = true;

		for (const state of voiceStates) {
			const connectionId = state.connection_id ?? '';
			const participant = MediaEngineStore.getParticipantByUserIdAndConnectionId(user.id, connectionId);

			const selfMuted = state.self_mute ?? (participant ? !participant.isMicrophoneEnabled : false);
			const selfDeaf = !!state.self_deaf;
			const camera = state.self_video === true || (participant ? participant.isCameraEnabled : false);
			const live = state.self_stream === true || (participant ? participant.isScreenShareEnabled : false);

			anyCameraOn = anyCameraOn || camera;
			anyLive = anyLive || live;
			guildMuted = guildMuted || !!state.mute;
			guildDeaf = guildDeaf || !!state.deaf;
			allSelfMuted = allSelfMuted && !!selfMuted;
			allSelfDeaf = allSelfDeaf && !!selfDeaf;
		}

		if (isCurrentUser) {
			anyCameraOn = anyCameraOn || localSelfVideo;
			anyLive = anyLive || localSelfStream;
		}

		let anySpeaking = propAnySpeaking !== undefined ? propAnySpeaking : false;
		if (propAnySpeaking === undefined) {
			for (const state of voiceStates) {
				const connectionId = state.connection_id ?? '';
				const participant = MediaEngineStore.getParticipantByUserIdAndConnectionId(user.id, connectionId);
				const selfMuted = state.self_mute ?? (participant ? !participant.isMicrophoneEnabled : false);
				const speaking = !!(participant?.isSpeaking && !selfMuted && !(state.mute ?? false));
				anySpeaking = anySpeaking || speaking;
			}
		}

		return {anySpeaking, anyCameraOn, anyLive, guildMuted, guildDeaf, allSelfMuted, allSelfDeaf};
	}, [voiceStates, user.id, isCurrentUser, localSelfVideo, localSelfStream, propAnySpeaking]);

	return (
		<div className={styles.container}>
			<PreloadableUserPopout
				user={user}
				isWebhook={false}
				guildId={guildId}
				position="right-start"
				disableContextMenu={true}
			>
				<div
					className={clsx(styles.participantButton, stateAgg.anySpeaking && styles.participantButtonSpeaking)}
					role="button"
					tabIndex={0}
					aria-label={`Open profile for ${NicknameUtils.getNickname(user)}`}
					onContextMenu={handleContextMenu}
				>
					<div className={styles.avatarAndName}>
						<AvatarWithPresence user={user} size={24} speaking={stateAgg.anySpeaking} guildId={guildId} />
						<div className={styles.nameContainer}>
							<span
								className={clsx(
									styles.participantName,
									stateAgg.anySpeaking && styles.participantNameSpeaking,
									isCurrentUser && !stateAgg.anySpeaking && styles.participantNameCurrent,
								)}
							>
								{NicknameUtils.getNickname(user)}
							</span>
							{connectionCount > 1 && (
								<Tooltip text={connectionCount === 1 ? t`${connectionCount} device` : t`${connectionCount} devices`}>
									<span className={styles.deviceCount}>({connectionCount})</span>
								</Tooltip>
							)}
						</div>
					</div>

					<div className={styles.iconsAndToggle}>
						<VoiceStateIcons
							isSelfMuted={stateAgg.allSelfMuted && !stateAgg.guildMuted}
							isSelfDeafened={stateAgg.allSelfDeaf && !stateAgg.guildDeaf}
							isGuildMuted={stateAgg.guildMuted}
							isGuildDeafened={stateAgg.guildDeaf}
							isCameraOn={stateAgg.anyCameraOn}
							isScreenSharing={stateAgg.anyLive}
							className={styles.flexShrinkZero}
						/>

						<Tooltip text={isExpanded ? 'Collapse devices' : 'Expand devices'}>
							<FocusRing offset={-2}>
								<button
									type="button"
									aria-label={isExpanded ? 'Collapse devices' : 'Expand devices'}
									aria-expanded={isExpanded}
									onClick={(e) => {
										e.stopPropagation();
										toggleExpanded();
									}}
									className={styles.toggleButton}
								>
									<CaretDownIcon
										weight="bold"
										className={clsx(styles.toggleIcon, !isExpanded && styles.toggleIconCollapsed)}
									/>
								</button>
							</FocusRing>
						</Tooltip>
					</div>
				</div>
			</PreloadableUserPopout>

			{isExpanded && (
				<div className={styles.devicesContainer}>
					{[...voiceStates]
						.sort((a, b) => (a.connection_id || '').localeCompare(b.connection_id || ''))
						.map((voiceState, index) => (
							<VoiceParticipantItem
								key={voiceState.connection_id || `${user.id}-${index}`}
								user={user}
								voiceState={voiceState}
								guildId={guildId}
								isGroupedItem={true}
								isCurrentUserConnection={isCurrentUser && voiceState.connection_id === currentConnectionId}
							/>
						))}
				</div>
			)}
		</div>
	);
});
