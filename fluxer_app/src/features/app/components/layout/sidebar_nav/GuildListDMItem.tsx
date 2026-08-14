// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {LongPressable} from '@app/features/app/components/LongPressable';
import guildStyles from '@app/features/app/components/layout/GuildsLayout.module.css';
import type {ScrollIndicatorSeverity} from '@app/features/app/components/layout/ScrollIndicatorOverlay';
import styles from '@app/features/app/components/layout/sidebar_nav/GuildListDMItem.module.css';
import {VoiceBadge, type VoiceBadgeActivity} from '@app/features/app/components/layout/sidebar_nav/VoiceBadge';
import {getChannelUnreadState} from '@app/features/app/components/layout/utils/ChannelUnreadState';
import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import {useHover} from '@app/features/app/hooks/useHover';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import {DMBottomSheet} from '@app/features/channel/components/bottomsheets/DMBottomSheet';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {MENTION_COUNT_ARIA_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {DMContextMenu} from '@app/features/ui/action_menu/DMContextMenu';
import {GroupDMContextMenu} from '@app/features/ui/action_menu/GroupDMContextMenu';
import {AvatarStack} from '@app/features/ui/avatars/AvatarStack';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {MentionBadgeAnimated} from '@app/features/ui/components/MentionBadge';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {getAppZoomFactor} from '@app/features/ui/utils/AppZoomUtils';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import type {User} from '@app/features/user/models/User';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {
	createVoiceParticipantSortSnapshot,
	sortVoiceParticipantItemsWithSnapshot,
} from '@app/features/voice/components/VoiceParticipantSortUtils';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {useVoiceGatewayStateVersion} from '@app/features/voice/engine/v2/VoiceEngineV2AppVoiceStateAdapter';
import CallState from '@app/features/voice/state/CallState';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MonitorPlayIcon, SpeakerHighIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useRef, useState} from 'react';

const SELECTED_DESCRIPTOR = msg({
	message: 'selected',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild list DM item.',
});
const UNREAD_DESCRIPTOR = msg({
	message: 'unread',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild list DM item.',
});
const MUTED_DESCRIPTOR = msg({
	message: 'muted',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild list DM item.',
});
const ACTIVE_CALL_DESCRIPTOR = msg({
	message: 'active call',
	comment: 'Lowercase screen-reader fragment in the sidebar navigation guild list DM item.',
});

interface DMListItemProps {
	channel: Channel;
	isSelected: boolean;
	className?: string;
	voiceCallActive?: boolean;
	onHoverStart: (channelId: string) => void;
	onHoverEnd: (channelId: string) => void;
}

interface ResolvedDMListItemProps extends DMListItemProps {
	isGroupDM: boolean;
	recipient: User | null;
}

interface VoiceRow {
	key: 'voice' | 'screenshare';
	users: Array<User>;
}

interface DMVoiceSummary {
	voiceUsers: Array<User>;
	streamingUsers: Array<User>;
	hasScreenshare: boolean;
	hasVideo: boolean;
}

const ResolvedDMListItem = observer(function ResolvedDMListItem({
	channel,
	isSelected,
	className,
	voiceCallActive = false,
	onHoverStart,
	onHoverEnd,
	isGroupDM,
	recipient,
}: ResolvedDMListItemProps) {
	const {i18n} = useLingui();
	useVoiceGatewayStateVersion();
	const [hoverRef, isHovering] = useHover();
	const buttonRef = useRef<HTMLButtonElement | null>(null);
	const iconRef = useRef<HTMLDivElement | null>(null);
	const mergedButtonRef = useMergeRefs([hoverRef, buttonRef]);
	const [bottomSheetOpen, setBottomSheetOpen] = useState(false);
	const isMobileExperience = isMobileExperienceEnabled();
	const [isFocused, setIsFocused] = useState(false);
	const {keyboardModeEnabled} = KeyboardMode;
	const isMuted = UserGuildSettings.isChannelMuted(null, channel.id);
	const mentionCount = ReadStates.getMentionCount(channel.id);
	const unreadState = getChannelUnreadState({
		unreadCount: ReadStates.getUnreadCount(channel.id),
		mentionCount,
		isMuted,
		showFadedUnreadOnMutedChannels: Accessibility.showFadedUnreadOnMutedChannels,
	});
	const dmScrollSeverity: ScrollIndicatorSeverity | undefined = (() => {
		if (mentionCount > 0) return 'mention';
		if (unreadState.shouldShowUnreadIndicator) return 'unread';
		return undefined;
	})();
	const dmScrollId = `dm-${channel.id}`;
	const directMessageName = recipient ? NicknameUtils.getNickname(recipient) : null;
	const computedDisplayName = ChannelUtils.getDMDisplayName(channel);
	const displayName = isGroupDM ? computedDisplayName : (directMessageName ?? computedDisplayName);
	const hasActiveCall = CallState.hasActiveCall(channel.id);
	const voiceUserSortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
	const streamingUserSortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
	const dmChannelVoiceStates =
		hasActiveCall || voiceCallActive ? MediaEngine.getAllVoiceStatesInChannel(ME, channel.id) : null;
	const voiceSummary = useMemo<DMVoiceSummary>(() => {
		if (!dmChannelVoiceStates) {
			return {
				voiceUsers: [],
				streamingUsers: [],
				hasScreenshare: false,
				hasVideo: false,
			};
		}
		const voiceUsers: Array<User> = [];
		const streamingUsers: Array<User> = [];
		const seen = new Set<string>();
		let hasScreenshare = false;
		let hasVideo = false;
		for (const connectionId in dmChannelVoiceStates) {
			const voiceState = dmChannelVoiceStates[connectionId];
			if (!voiceState) continue;
			const isScreensharing = voiceState.self_stream === true;
			const isVideo = voiceState.self_video === true;
			if (isScreensharing) {
				hasScreenshare = true;
			}
			if (isVideo) {
				hasVideo = true;
			}
			if (seen.has(voiceState.user_id)) continue;
			const user = Users.getUser(voiceState.user_id);
			if (!user) continue;
			if (isScreensharing) {
				streamingUsers.push(user);
			} else {
				voiceUsers.push(user);
			}
			seen.add(user.id);
		}
		const sortedVoiceUsers = sortVoiceParticipantItemsWithSnapshot(voiceUsers, {
			snapshot: voiceUserSortSnapshotRef.current,
			getParticipantKey: (user) => user.id,
			getUserId: (user) => user.id,
			channelId: channel.id,
		});
		const sortedStreamingUsers = sortVoiceParticipantItemsWithSnapshot(streamingUsers, {
			snapshot: streamingUserSortSnapshotRef.current,
			getParticipantKey: (user) => user.id,
			getUserId: (user) => user.id,
			channelId: channel.id,
		});
		return {
			voiceUsers: sortedVoiceUsers,
			streamingUsers: sortedStreamingUsers,
			hasScreenshare,
			hasVideo,
		};
	}, [channel.id, dmChannelVoiceStates]);
	const voiceRows = useMemo<Array<VoiceRow>>(() => {
		const rows: Array<VoiceRow> = [];
		if (voiceSummary.voiceUsers.length > 0) {
			rows.push({key: 'voice', users: voiceSummary.voiceUsers});
		}
		if (voiceSummary.streamingUsers.length > 0) {
			rows.push({key: 'screenshare', users: voiceSummary.streamingUsers});
		}
		return rows;
	}, [voiceSummary.streamingUsers, voiceSummary.voiceUsers]);
	const hasVoiceActivity = voiceSummary.voiceUsers.length > 0 || voiceSummary.streamingUsers.length > 0;
	const voiceBadgeActivity = useMemo<VoiceBadgeActivity | null>(() => {
		if (!hasVoiceActivity) return voiceCallActive ? 'voice' : null;
		if (voiceSummary.hasScreenshare) return 'screenshare';
		if (voiceSummary.hasVideo) return 'video';
		return 'voice';
	}, [hasVoiceActivity, voiceCallActive, voiceSummary.hasScreenshare, voiceSummary.hasVideo]);
	const dmAriaLabel = useMemo(() => {
		const parts = [displayName];
		if (isSelected) parts.push(i18n._(SELECTED_DESCRIPTOR));
		if (mentionCount > 0) parts.push(i18n._(MENTION_COUNT_ARIA_DESCRIPTOR, {mentionCount}));
		else if (unreadState.hasUnreadMessages) parts.push(i18n._(UNREAD_DESCRIPTOR));
		if (isMuted) parts.push(i18n._(MUTED_DESCRIPTOR));
		if (hasActiveCall || voiceCallActive || hasVoiceActivity) parts.push(i18n._(ACTIVE_CALL_DESCRIPTOR));
		return parts.join(', ');
	}, [
		displayName,
		hasActiveCall,
		hasVoiceActivity,
		isMuted,
		isSelected,
		mentionCount,
		unreadState.hasUnreadMessages,
		voiceCallActive,
		i18n.locale,
	]);
	const handleSelect = () => {
		NavigationCommands.selectChannel(ME, channel.id);
	};
	const handleOpenBottomSheet = useCallback(() => {
		setBottomSheetOpen(true);
	}, []);
	const handleCloseBottomSheet = useCallback(() => {
		setBottomSheetOpen(false);
	}, []);
	const handleLongPress = useCallback(() => {
		if (isMobileExperience) {
			handleOpenBottomSheet();
		}
	}, [isMobileExperience, handleOpenBottomSheet]);
	const handleHoverStart = useCallback(() => {
		onHoverStart(channel.id);
	}, [channel.id, onHoverStart]);
	const handleHoverEnd = useCallback(() => {
		onHoverEnd(channel.id);
	}, [channel.id, onHoverEnd]);
	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			if (isMobileExperience) {
				return;
			}
			ContextMenuCommands.openFromEvent(event, (props) =>
				isGroupDM ? (
					<GroupDMContextMenu
						channel={channel}
						onClose={props.onClose}
						data-flx="app.sidebar-nav.guild-list-dm-item.handle-context-menu.group-dm-context-menu"
					/>
				) : (
					<DMContextMenu
						channel={channel}
						recipient={recipient}
						onClose={props.onClose}
						data-flx="app.sidebar-nav.guild-list-dm-item.handle-context-menu.dm-context-menu"
					/>
				),
			);
		},
		[channel, isGroupDM, recipient, isMobileExperience],
	);
	const indicatorHeight =
		(() => {
			if (isSelected) return 40;
			if (isHovering) return 20;
			return 8;
		})() * getAppZoomFactor();
	const tooltipContent = useMemo<string | (() => React.ReactNode)>(() => {
		if (!hasVoiceActivity) {
			return displayName;
		}
		return () => (
			<div
				className={guildStyles.guildTooltipContainer}
				data-flx="app.sidebar-nav.guild-list-dm-item.tooltip-content.div"
			>
				<div
					className={guildStyles.guildTooltipHeader}
					data-flx="app.sidebar-nav.guild-list-dm-item.tooltip-content.div--2"
				>
					<span
						className={guildStyles.guildTooltipName}
						data-flx="app.sidebar-nav.guild-list-dm-item.tooltip-content.span"
					>
						{displayName}
					</span>
				</div>
				{voiceRows.map((row) => (
					<div
						key={row.key}
						className={guildStyles.guildVoiceInfo}
						data-flx="app.sidebar-nav.guild-list-dm-item.tooltip-content.div--3"
					>
						{row.key === 'screenshare' ? (
							<MonitorPlayIcon
								weight="fill"
								className={guildStyles.guildVoiceIcon}
								data-flx="app.sidebar-nav.guild-list-dm-item.tooltip-content.monitor-play-icon"
							/>
						) : (
							<SpeakerHighIcon
								className={guildStyles.guildVoiceIcon}
								data-flx="app.sidebar-nav.guild-list-dm-item.tooltip-content.speaker-high-icon"
							/>
						)}
						<AvatarStack
							size={28}
							maxVisible={3}
							users={row.users}
							channelId={channel.id}
							data-flx="app.sidebar-nav.guild-list-dm-item.tooltip-content.avatar-stack"
						/>
					</div>
				))}
			</div>
		);
	}, [channel.id, displayName, hasVoiceActivity, voiceRows]);
	const showControls = isHovering || (keyboardModeEnabled && isFocused);
	return (
		<>
			<Tooltip
				position="right"
				size="large"
				text={tooltipContent}
				data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.tooltip"
			>
				<LongPressable
					className={clsx(guildStyles.dmListItem, className, isMuted && styles.muted)}
					onLongPress={handleLongPress}
					data-scroll-indicator={dmScrollSeverity}
					data-scroll-id={dmScrollId}
					data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.muted"
				>
					<FocusRing
						offset={-2}
						focusTarget={buttonRef}
						ringTarget={iconRef}
						data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.focus-ring"
					>
						<button
							type="button"
							className={styles.button}
							aria-label={dmAriaLabel}
							aria-current={isSelected ? 'page' : undefined}
							data-guild-list-focus-item="true"
							onClick={handleSelect}
							onContextMenu={handleContextMenu}
							onMouseEnter={handleHoverStart}
							onMouseLeave={handleHoverEnd}
							onFocus={() => setIsFocused(true)}
							onBlur={() => setIsFocused(false)}
							ref={mergedButtonRef}
							data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.button.select"
						>
							<AnimatePresence data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.animate-presence">
								{(unreadState.shouldShowUnreadIndicator || isSelected || showControls) && (
									<div
										className={guildStyles.guildIndicator}
										data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.div"
									>
										<motion.span
											className={guildStyles.guildIndicatorBar}
											initial={false}
											animate={{opacity: 1, scale: 1, height: indicatorHeight}}
											exit={
												Accessibility.useReducedMotion
													? {opacity: 1, scale: 1, height: indicatorHeight}
													: {opacity: 0, scale: 0, height: 0}
											}
											transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2, ease: [0.25, 0.1, 0.25, 1]}}
											data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.span"
										/>
									</div>
								)}
							</AnimatePresence>
							<div className={styles.relative} data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.relative">
								<motion.div
									ref={iconRef}
									className={guildStyles.dmIcon}
									animate={{borderRadius: isSelected || showControls ? '30%' : '50%'}}
									initial={false}
									transition={{duration: Accessibility.useReducedMotion ? 0 : 0.07, ease: 'easeOut'}}
									data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.div--2"
								>
									{isGroupDM ? (
										<GroupDMAvatar
											channel={channel}
											size={44}
											disableStatusIndicator
											data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.group-dm-avatar"
										/>
									) : (
										recipient && (
											<StatusAwareAvatar
												disablePresence={true}
												user={recipient}
												size={44}
												className={styles.fullSize}
												data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.full-size"
											/>
										)
									)}
								</motion.div>
								<div
									className={clsx(guildStyles.guildBadge, mentionCount > 0 && guildStyles.guildBadgeActive)}
									data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.div--3"
								>
									<MentionBadgeAnimated
										mentionCount={mentionCount}
										size="small"
										data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.mention-badge-animated"
									/>
								</div>
								{voiceBadgeActivity && (
									<VoiceBadge
										activity={voiceBadgeActivity}
										data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.voice-badge"
									/>
								)}
							</div>
						</button>
					</FocusRing>
				</LongPressable>
			</Tooltip>
			{isMobileExperience && (
				<DMBottomSheet
					isOpen={bottomSheetOpen}
					onClose={handleCloseBottomSheet}
					channel={channel}
					recipient={recipient}
					data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.dm-bottom-sheet"
				/>
			)}
		</>
	);
});
export const DMListItem = observer((props: DMListItemProps) => {
	const {channel} = props;
	const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
	const recipient = !isGroupDM ? (Users.getUser(channel.recipientIds[0]) ?? null) : null;
	if (!isGroupDM && !recipient) return null;
	return (
		<ResolvedDMListItem
			data-flx="app.sidebar-nav.guild-list-dm-item.dm-list-item.resolved-dm-list-item"
			{...props}
			isGroupDM={isGroupDM}
			recipient={recipient}
		/>
	);
});
