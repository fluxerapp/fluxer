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

import {Trans, useLingui} from '@lingui/react/macro';
import {ChatTeardropIcon, PhoneIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';

import * as CallActionCreators from '~/actions/CallActionCreators';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import {ChannelTypes} from '~/Constants';
import {BlockedUserBarrier, UnclaimedDMBarrier} from '~/components/channel/barriers/BarrierComponents';
import {ChannelChatLayout} from '~/components/channel/ChannelChatLayout';
import {ChannelHeader} from '~/components/channel/ChannelHeader';
import {ChannelMembers} from '~/components/channel/ChannelMembers';
import {ChannelTextarea} from '~/components/channel/ChannelTextarea';
import dmStyles from '~/components/channel/dm/DMChannelView.module.css';
import {Avatar} from '~/components/uikit/Avatar';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import {Button} from '~/components/uikit/Button/Button';
import {UserContextMenu} from '~/components/uikit/ContextMenu/UserContextMenu';
import {CompactVoiceCallView} from '~/components/voice/CompactVoiceCallView';
import {useChannelMemberListVisibility} from '~/hooks/useChannelMemberListVisibility';
import {useChannelSearchVisibility} from '~/hooks/useChannelSearchVisibility';
import {useFluxerDocumentTitle} from '~/hooks/useFluxerDocumentTitle';
import {useMemberListVisible} from '~/hooks/useMemberListVisible';
import {Logger} from '~/lib/Logger';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import CallStateStore, {type Call} from '~/stores/CallStateStore';
import ChannelStore from '~/stores/ChannelStore';
import RelationshipStore from '~/stores/RelationshipStore';
import UserStore from '~/stores/UserStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import WindowStore from '~/stores/WindowStore';
import * as ChannelUtils from '~/utils/ChannelUtils';
import {isMobileExperienceEnabled} from '~/utils/mobileExperience';
import styles from '../ChannelIndexPage.module.css';
import {ChannelSearchResults} from '../ChannelSearchResults';
import {Messages} from '../Messages';
import {ChannelViewScaffold} from './ChannelViewScaffold';
import {useCallHeaderState} from './useCallHeaderState';
import {useChannelSearchState} from './useChannelSearchState';

interface DMChannelViewProps {
	channelId: string;
}

const logger = new Logger('DMChannelView');
const CALL_AVATAR_PIXEL_SIZES = [100, 60, 40, 30] as const;
const CALL_AVATAR_PADDING = 16;
const CALL_AVATAR_LAYOUT_OFFSET = 320;

interface CallParticipant {
	user: UserRecord;
	isRinging: boolean;
}

interface CallParticipantsRowProps {
	call: Call;
	channel: ChannelRecord;
}

const getCallAvatarSize = (count: number, windowWidth: number): number => {
	if (count <= 0) return CALL_AVATAR_PIXEL_SIZES[CALL_AVATAR_PIXEL_SIZES.length - 1];
	const availableWidth = Math.max(0, windowWidth - CALL_AVATAR_LAYOUT_OFFSET);
	for (const size of CALL_AVATAR_PIXEL_SIZES) {
		const slotWidth = size + CALL_AVATAR_PADDING;
		if (availableWidth > count * slotWidth) {
			return size;
		}
	}
	return CALL_AVATAR_PIXEL_SIZES[CALL_AVATAR_PIXEL_SIZES.length - 1];
};

const CallParticipantsRow = observer(({call, channel}: CallParticipantsRowProps) => {
	const {t} = useLingui();
	const windowWidth = WindowStore.windowSize.width;
	const currentUserId = AuthenticationStore.currentUserId;
	const callParticipantIds = call.participants;
	const liveParticipantIds = CallStateStore.getParticipants(channel.id);
	const orderedIds = [
		currentUserId,
		...callParticipantIds,
		...liveParticipantIds,
		...channel.recipientIds,
		...call.ringing,
	].filter((id): id is string => Boolean(id));

	const ringingSet = new Set(call.ringing);
	const participantSet = new Set([...callParticipantIds, ...liveParticipantIds]);
	const participants: Array<CallParticipant> = [];
	const seen = new Set<string>();

	const addParticipant = (id: string) => {
		if (seen.has(id)) return;
		const isInCall = participantSet.has(id);
		const isRinging = ringingSet.has(id) && !isInCall;
		if (!isInCall && !isRinging) return;
		const user = UserStore.getUser(id);
		if (!user) return;
		participants.push({user, isRinging: !isInCall && isRinging});
		seen.add(id);
	};

	for (const id of orderedIds) {
		addParticipant(id);
	}

	for (const id of liveParticipantIds) {
		addParticipant(id);
	}

	for (const id of call.ringing) {
		addParticipant(id);
	}

	const handleContextMenu = React.useCallback(
		(event: React.MouseEvent, user: UserRecord) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<UserContextMenu user={user} onClose={onClose} channelId={channel.id} isCallContext />
			));
		},
		[channel.id],
	);

	if (participants.length === 0) return null;

	const avatarSize = getCallAvatarSize(participants.length, windowWidth);

	return (
		<div className={dmStyles.callParticipantsRow} role="group" aria-label={t`Call participants`}>
			{participants.map(({user, isRinging}) => (
				<button
					type="button"
					key={user.id}
					className={`${dmStyles.callParticipant} ${isRinging ? dmStyles.callParticipantRinging : ''}`.trim()}
					onContextMenu={(event) => handleContextMenu(event, user)}
					aria-label={user.username}
				>
					{isRinging && (
						<>
							<span className={`${dmStyles.callParticipantRipple} ${dmStyles.callParticipantRipple0}`} />
							<span className={`${dmStyles.callParticipantRipple} ${dmStyles.callParticipantRipple1}`} />
							<span className={`${dmStyles.callParticipantRipple} ${dmStyles.callParticipantRipple2}`} />
						</>
					)}
					<div className={dmStyles.callParticipantAvatar}>
						<Avatar user={user} size={avatarSize} status={null} showOffline={false} />
					</div>
				</button>
			))}
		</div>
	);
});

export const DMChannelView = observer(({channelId}: DMChannelViewProps) => {
	const {t} = useLingui();
	const channel = ChannelStore.getChannel(channelId);
	const recipientId = channel?.recipientIds?.[0];
	const recipient = recipientId ? UserStore.getUser(recipientId) : null;
	const isRecipientBlocked = recipientId ? RelationshipStore.isBlocked(recipientId) : false;
	const isCurrentUserUnclaimed = !UserStore.currentUser?.isClaimed();
	const isMemberListVisible = useMemberListVisible();

	const mediaConnected = MediaEngineStore.connected;
	const mediaChannelId = MediaEngineStore.channelId;
	const mediaGuildId = MediaEngineStore.guildId;
	const room = MediaEngineStore.room;

	const searchState = useChannelSearchState(channel);
	const {
		isSearchActive,
		handleSearchClose,
		handleSearchSubmit,
		searchRefreshKey,
		activeSearchQuery,
		activeSearchSegments,
	} = searchState;

	useChannelSearchVisibility(channelId, isSearchActive);
	useChannelMemberListVisibility(channelId, isMemberListVisible);

	const isDM = channel?.type === ChannelTypes.DM;
	const displayName = channel ? ChannelUtils.getDMDisplayName(channel) : null;
	const title = isDM && displayName ? `@${displayName}` : displayName;
	useFluxerDocumentTitle(title);
	const isGroupDM = channel?.type === ChannelTypes.GROUP_DM;
	const callHeaderState = useCallHeaderState(channel);
	const call = callHeaderState.call;
	const showCompactVoiceView = callHeaderState.controlsVariant === 'inCall';
	const callExistsAndOngoing = callHeaderState.callExistsAndOngoing;
	const controlsVariant = callHeaderState.controlsVariant;
	const showCallBackground = callExistsAndOngoing && controlsVariant !== 'hidden';
	const isMobileExperience = isMobileExperienceEnabled();
	const [isCallSheetOpen, setIsCallSheetOpen] = React.useState(false);

	const handleOpenCallSheet = React.useCallback(() => setIsCallSheetOpen(true), []);
	const handleCloseCallSheet = React.useCallback(() => setIsCallSheetOpen(false), []);

	React.useEffect(() => {
		if (!callExistsAndOngoing) {
			setIsCallSheetOpen(false);
		}
	}, [callExistsAndOngoing]);

	React.useEffect(() => {
		logger.debug('voice connection state', {
			channelId,
			connected: mediaConnected,
			mediaChannelId,
			mediaGuildId,
			hasRoom: Boolean(room),
			showCompactVoiceView,
		});
	}, [channelId, mediaConnected, mediaChannelId, mediaGuildId, room, showCompactVoiceView]);

	React.useEffect(() => {
		logger.debug('compact voice view render decision', {
			channelId,
			showCompactVoiceView,
			roomId: (room as any)?.sid ?? null,
		});
	}, [channelId, showCompactVoiceView, room]);

	const currentChannelId = channel?.id ?? null;
	const handleJoinCall = React.useCallback(() => {
		if (currentChannelId) {
			CallActionCreators.joinCall(currentChannelId);
		}
	}, [currentChannelId]);

	const handleRejectIncomingCall = React.useCallback(() => {
		if (currentChannelId) {
			CallActionCreators.rejectCall(currentChannelId);
		}
	}, [currentChannelId]);

	const handleIgnoreIncomingCall = React.useCallback(() => {
		if (currentChannelId) {
			CallActionCreators.ignoreCall(currentChannelId);
		}
	}, [currentChannelId]);

	const shouldRenderMemberList = Boolean(isGroupDM && isMemberListVisible && !isSearchActive);

	const callStatusLabel = React.useMemo(() => {
		switch (controlsVariant) {
			case 'incoming':
				return t`Incoming call`;
			case 'join':
				return t`Call available`;
			case 'connecting':
				return t`Connecting…`;
			case 'inCall':
				return t`In call`;
			default:
				return t`Voice call`;
		}
	}, [controlsVariant]);

	const callSheetButtonLabel = React.useMemo(
		() => (controlsVariant === 'incoming' ? t`View incoming call` : t`View call`),
		[controlsVariant],
	);

	const renderCallControls = React.useCallback(() => {
		if (controlsVariant === 'incoming') {
			return (
				<>
					<Button variant="primary" leftIcon={<PhoneIcon size={16} weight="fill" />} onClick={handleJoinCall}>
						<Trans>Accept</Trans>
					</Button>
					<Button
						variant="danger-primary"
						leftIcon={<XIcon size={16} weight="bold" />}
						onClick={handleRejectIncomingCall}
					>
						<Trans>Reject</Trans>
					</Button>
					<Button variant="secondary" onClick={handleIgnoreIncomingCall}>
						<Trans>Ignore</Trans>
					</Button>
				</>
			);
		}

		if (controlsVariant === 'join') {
			return (
				<Button
					variant="primary"
					leftIcon={<PhoneIcon size={16} weight="fill" />}
					onClick={handleJoinCall}
					disabled={!currentChannelId}
				>
					<Trans>Join call</Trans>
				</Button>
			);
		}

		if (controlsVariant === 'connecting') {
			return (
				<Button variant="secondary" leftIcon={<PhoneIcon size={16} weight="fill" />} disabled>
					<Trans>Connecting…</Trans>
				</Button>
			);
		}

		return null;
	}, [controlsVariant, handleJoinCall, handleRejectIncomingCall, handleIgnoreIncomingCall, currentChannelId]);

	const headerCallControls = React.useMemo(() => renderCallControls(), [renderCallControls]);
	const sheetCallControls = renderCallControls();

	if (!channel) {
		return (
			<div className={dmStyles.emptyState}>
				<ChatTeardropIcon weight="fill" className={dmStyles.emptyStateIcon} />
				<h2 className={dmStyles.emptyStateTitle}>
					<Trans>This conversation has been erased</Trans>
				</h2>
				<p className={dmStyles.emptyStateDescription}>
					<Trans>Think, McFly! Did you type the right address?</Trans>
				</p>
			</div>
		);
	}

	if (isDM && !recipient) {
		return (
			<div className={dmStyles.emptyState}>
				<ChatTeardropIcon weight="fill" className={dmStyles.emptyStateIcon} />
				<h2 className={dmStyles.emptyStateTitle}>
					<Trans>User has vanished</Trans>
				</h2>
				<p className={dmStyles.emptyStateDescription}>
					<Trans>They might have taken the DeLorean elsewhere.</Trans>
				</p>
			</div>
		);
	}

	return (
		<>
			<ChannelViewScaffold
				className={showCallBackground ? styles.channelGridVoiceCallActive : undefined}
				header={
					<div className={showCallBackground ? styles.voiceActiveHeaderWrapper : undefined}>
						<ChannelHeader
							channel={channel}
							showMembersToggle={Boolean(isGroupDM)}
							showPins={true}
							onSearchSubmit={handleSearchSubmit}
							onSearchClose={handleSearchClose}
							isSearchResultsOpen={isSearchActive}
							forceVoiceCallStyle={showCompactVoiceView}
						/>
						{callExistsAndOngoing &&
							call &&
							channel &&
							(isMobileExperience ? (
								<div className={dmStyles.callBannerMobile}>
									<div className={dmStyles.callBannerMobileLabel}>{callStatusLabel}</div>
									{controlsVariant !== 'inCall' && headerCallControls && (
										<div className={dmStyles.callControlsMobile}>{headerCallControls}</div>
									)}
									<Button
										variant="secondary"
										onClick={handleOpenCallSheet}
										leftIcon={<PhoneIcon size={16} weight="fill" />}
									>
										{callSheetButtonLabel}
									</Button>
								</div>
							) : controlsVariant === 'inCall' ? (
								<>
									<CallParticipantsRow call={call} channel={channel} />
									<CompactVoiceCallView channel={channel} className={dmStyles.compactVoiceCallView} />
								</>
							) : (
								<div className={dmStyles.callBanner}>
									<CallParticipantsRow call={call} channel={channel} />
									{headerCallControls && <div className={dmStyles.callControls}>{headerCallControls}</div>}
								</div>
							))}
					</div>
				}
				chatArea={
					<ChannelChatLayout
						channel={channel}
						messages={<Messages key={channel.id} channel={channel} />}
						textarea={
							isDM && isRecipientBlocked && recipient ? (
								<BlockedUserBarrier userId={recipient.id} username={recipient.username} />
							) : isCurrentUserUnclaimed ? (
								<UnclaimedDMBarrier />
							) : (
								<ChannelTextarea channel={channel} />
							)
						}
					/>
				}
				sidePanel={
					isSearchActive ? (
						<div className={styles.searchPanel}>
							<ChannelSearchResults
								channel={channel}
								searchQuery={activeSearchQuery}
								searchSegments={activeSearchSegments}
								refreshKey={searchRefreshKey}
								onClose={() => searchState.setIsSearchActive(false)}
							/>
						</div>
					) : shouldRenderMemberList ? (
						<ChannelMembers channel={channel} />
					) : null
				}
			/>
			{isMobileExperience && callExistsAndOngoing && call && channel && (
				<BottomSheet
					isOpen={isCallSheetOpen}
					onClose={handleCloseCallSheet}
					title={channel.name ?? displayName ?? callStatusLabel}
					snapPoints={[0.35, 0.65, 0.95]}
					surface="primary"
					disablePadding
				>
					<div className={dmStyles.callSheetContent}>
						<CallParticipantsRow call={call} channel={channel} />
						<div className={dmStyles.callSheetStatus}>{callStatusLabel}</div>
						{controlsVariant === 'inCall' && (
							<CompactVoiceCallView channel={channel} className={dmStyles.compactVoiceCallView} />
						)}
						{sheetCallControls && <div className={dmStyles.callSheetControls}>{sheetCallControls}</div>}
					</div>
				</BottomSheet>
			)}
		</>
	);
});
