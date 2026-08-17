// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import {
	CALL_AVATAR_DEFAULT_SIZE,
	CALL_AVATAR_SPRING,
	CALL_PARTICIPANTS_DESCRIPTOR,
	type CallParticipant,
	type CallParticipantLayoutMetrics,
	type CallParticipantsRowProps,
	getCallParticipantLayoutMetrics,
} from '@app/features/channel/components/channel_view/dm_channel_view/shared';
import dmStyles from '@app/features/channel/components/direct_message/DMChannelView.module.css';
import {UserContextMenu} from '@app/features/ui/action_menu/UserContextMenu';
import {VoiceParticipantContextMenu} from '@app/features/ui/action_menu/VoiceParticipantContextMenu';
import {AvatarWithPresence} from '@app/features/ui/avatars/AvatarWithPresence';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {appZoomLayoutPx, getAppRemScale} from '@app/features/ui/utils/AppZoomUtils';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {VoiceParticipantAvatarEntry} from '@app/features/voice/components/VoiceParticipantAvatarList';
import CallState from '@app/features/voice/state/CallState';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useLayoutEffect, useMemo, useRef, useState} from 'react';

export const CallParticipantsRow = observer(
	({call, channel, participantAvatarEntries, className}: CallParticipantsRowProps) => {
		const {i18n} = useLingui();
		const rowRef = useRef<HTMLDivElement | null>(null);
		const currentUserId = Authentication.currentUserId;
		const callParticipantIds = call.participants;
		const liveParticipantIds = CallState.getParticipants(channel.id);
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
			const user = Users.getUser(id);
			if (!user) return;
			participants.push({user, isRinging: !isInCall && isRinging});
			seen.add(id);
		};
		for (const id of orderedIds) addParticipant(id);
		for (const id of liveParticipantIds) addParticipant(id);
		for (const id of call.ringing) addParticipant(id);
		const participantEntryByUserId = useMemo(() => {
			const byUserId = new Map<string, VoiceParticipantAvatarEntry>();
			participantAvatarEntries.forEach((entry) => {
				if (byUserId.has(entry.userId)) return;
				byUserId.set(entry.userId, entry);
			});
			return byUserId;
		}, [participantAvatarEntries]);
		const handleContextMenu = useCallback(
			(event: React.MouseEvent, user: User) => {
				event.preventDefault();
				event.stopPropagation();
				const participantEntry = participantEntryByUserId.get(user.id);
				const participantName = NicknameUtils.getNickname(user, channel.guildId ?? null, channel.id) || user.id;
				ContextMenuCommands.openFromEvent(event, ({onClose}) =>
					participantEntry ? (
						<VoiceParticipantContextMenu
							user={user}
							participantName={participantName}
							onClose={onClose}
							guildId={channel.guildId ?? undefined}
							connectionId={participantEntry.connectionId}
							data-flx="channel.channel-view.dm-channel-view.handle-context-menu.voice-participant-context-menu"
						/>
					) : (
						<UserContextMenu
							user={user}
							onClose={onClose}
							channelId={channel.id}
							isCallContext
							data-flx="channel.channel-view.dm-channel-view.handle-context-menu.user-context-menu"
						/>
					),
				);
			},
			[channel.guildId, channel.id, participantEntryByUserId],
		);
		const [layoutMetrics, setLayoutMetrics] = useState<CallParticipantLayoutMetrics>({
			avatarSize: CALL_AVATAR_DEFAULT_SIZE,
			gap: 12,
		});
		useLayoutEffect(() => {
			const rowNode = rowRef.current;
			if (!rowNode) return;
			const recomputeLayoutMetrics = () => {
				const rowRect = rowNode.getBoundingClientRect();
				const nextMetrics = getCallParticipantLayoutMetrics(
					participants.length,
					appZoomLayoutPx(rowRect.width),
					appZoomLayoutPx(rowRect.height),
				);
				setLayoutMetrics((previousMetrics) => {
					if (previousMetrics.avatarSize === nextMetrics.avatarSize && previousMetrics.gap === nextMetrics.gap) {
						return previousMetrics;
					}
					return nextMetrics;
				});
			};
			recomputeLayoutMetrics();
			if (typeof ResizeObserver === 'undefined') {
				window.addEventListener('resize', recomputeLayoutMetrics);
				return () => window.removeEventListener('resize', recomputeLayoutMetrics);
			}
			const resizeObserver = new ResizeObserver(() => {
				recomputeLayoutMetrics();
			});
			resizeObserver.observe(rowNode);
			return () => resizeObserver.disconnect();
		}, [participants.length]);
		const avatarSize = layoutMetrics.avatarSize;
		const scaledAvatarSize = avatarSize / getAppRemScale();
		const callRippleStyle = useMemo(
			() =>
				({
					'--call-participant-avatar-size': `${avatarSize}px`,
					'--call-participant-gap': `${layoutMetrics.gap}px`,
					'--call-participant-ring-pulse-spread': `${Math.max(8, Math.round(avatarSize * 0.18))}px`,
				}) as React.CSSProperties,
			[avatarSize, layoutMetrics.gap],
		);
		if (participants.length === 0) return null;
		return (
			<div
				ref={rowRef}
				className={clsx(dmStyles.callParticipantsRow, className)}
				style={callRippleStyle}
				role="group"
				aria-label={i18n._(CALL_PARTICIPANTS_DESCRIPTOR)}
				data-flx="channel.channel-view.dm-channel-view.call-participants-row.group"
			>
				<AnimatePresence
					initial={false}
					data-flx="channel.channel-view.dm-channel-view.call-participants-row.animate-presence"
				>
					{participants.map(({user, isRinging}) => {
						const participantEntry = participantEntryByUserId.get(user.id);
						const displayName = NicknameUtils.getNickname(user, channel.guildId ?? null, channel.id);
						return (
							<motion.button
								type="button"
								key={user.id}
								className={clsx(dmStyles.callParticipant, isRinging && dmStyles.callParticipantRinging)}
								onContextMenu={(event) => handleContextMenu(event, user)}
								aria-label={displayName}
								layout
								initial={{opacity: 0, scale: 0.75}}
								animate={{opacity: 1, scale: 1}}
								exit={{opacity: 0, scale: 0.1}}
								transition={CALL_AVATAR_SPRING}
								data-flx="channel.channel-view.dm-channel-view.call-participants-row.button.context-menu"
							>
								<div
									className={dmStyles.callParticipantAvatar}
									data-flx="channel.channel-view.dm-channel-view.call-participants-row.div"
								>
									<AvatarWithPresence
										user={user}
										size={scaledAvatarSize}
										speaking={participantEntry?.speaking}
										muted={participantEntry?.selfMute}
										deafened={participantEntry?.selfDeaf}
										guildId={channel.guildId}
										data-flx="channel.channel-view.dm-channel-view.call-participants-row.avatar-with-presence"
									/>
								</div>
							</motion.button>
						);
					})}
				</AnimatePresence>
			</div>
		);
	},
);
