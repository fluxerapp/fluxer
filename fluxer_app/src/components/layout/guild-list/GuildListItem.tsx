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

import {useSortable} from '@dnd-kit/sortable';
import {CSS} from '@dnd-kit/utilities';
import {useLingui} from '@lingui/react/macro';
import {BellSlashIcon, ExclamationMarkIcon, PauseIcon, SealCheckIcon, SpeakerHighIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as NavigationActionCreators from '~/actions/NavigationActionCreators';
import {GuildFeatures, Permissions} from '~/Constants';
import {GuildHeaderBottomSheet} from '~/components/bottomsheets/GuildHeaderBottomSheet';
import {LongPressable} from '~/components/LongPressable';
import {Avatar} from '~/components/uikit/Avatar';
import {AvatarStack} from '~/components/uikit/avatars/AvatarStack';
import {GuildContextMenu} from '~/components/uikit/ContextMenu/GuildContextMenu';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {KeybindHint} from '~/components/uikit/KeybindHint/KeybindHint';
import {MentionBadgeAnimated} from '~/components/uikit/MentionBadge';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useHover} from '~/hooks/useHover';
import {useMergeRefs} from '~/hooks/useMergeRefs';
import {Routes} from '~/Routes';
import type {GuildRecord} from '~/records/GuildRecord';
import GuildReadStateStore from '~/stores/GuildReadStateStore';
import KeybindStore from '~/stores/KeybindStore';
import PermissionStore from '~/stores/PermissionStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import UserStore from '~/stores/UserStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';

import * as AvatarUtils from '~/utils/AvatarUtils';
import * as DateUtils from '~/utils/DateUtils';
import {getInitialsLength} from '~/utils/GuildInitialsUtils';
import * as ImageCacheUtils from '~/utils/ImageCacheUtils';
import {isMobileExperienceEnabled} from '~/utils/mobileExperience';
import * as RouterUtils from '~/utils/RouterUtils';
import * as StringUtils from '~/utils/StringUtils';
import styles from '../GuildsLayout.module.css';
import type {ScrollIndicatorSeverity} from '../ScrollIndicatorOverlay';
import {VoiceBadge} from './VoiceBadge';

interface GuildListItemProps {
	guild: GuildRecord;
	isSortingList?: boolean;
	isSelected: boolean;
	guildIndex?: number;
	selectedGuildIndex?: number;
}

export const GuildListItem = observer(
	({guild, isSortingList = false, isSelected, guildIndex, selectedGuildIndex}: GuildListItemProps) => {
		const {t} = useLingui();
		const initials = StringUtils.getInitialsFromName(guild.name);
		const initialsLength = React.useMemo(() => (initials ? getInitialsLength(initials) : null), [initials]);
		const [hoverRef, isHovering] = useHover();
		const [bottomSheetOpen, setBottomSheetOpen] = React.useState(false);
		const isMobileExperience = isMobileExperienceEnabled();
		const targetRadius = '50%';

		const guildReadSentinel = GuildReadStateStore.getGuildChangeSentinel(guild.id);
		const hasUnreadMessages = GuildReadStateStore.hasUnread(guild.id);
		const mentionCount = GuildReadStateStore.getMentionCount(guild.id);
		const guildScrollSeverity: ScrollIndicatorSeverity | undefined =
			mentionCount > 0 ? 'mention' : hasUnreadMessages ? 'unread' : undefined;
		const guildScrollId = `guild-${guild.id}`;
		const selectedChannel = SelectedChannelStore.selectedChannelIds.get(guild.id);

		const guildSettings = UserGuildSettingsStore.getSettings(guild.id);
		const isMuted = guildSettings?.muted || false;
		const muteConfig = guildSettings?.mute_config;
		const canManageGuild = PermissionStore.can(Permissions.MANAGE_GUILD, guild);

		const allVoiceStates = MediaEngineStore.getAllVoiceStates();
		const usersInVoice = React.useMemo(() => {
			const guildVoiceStates = allVoiceStates[guild.id];
			if (!guildVoiceStates) return [];
			const users: Array<NonNullable<ReturnType<typeof UserStore.getUser>>> = [];
			const seen = new Set<string>();
			for (const channelStates of Object.values(guildVoiceStates)) {
				for (const voiceState of Object.values(channelStates)) {
					if (seen.has(voiceState.user_id)) continue;
					const user = UserStore.getUser(voiceState.user_id);
					if (user) {
						users.push(user);
						seen.add(user.id);
					}
				}
			}
			return users;
		}, [allVoiceStates, guild.id]);
		const hasUsersInVoice = usersInVoice.length > 0;

		const iconUrl = AvatarUtils.getGuildIconURL(guild, false);
		const hoverIconUrl = AvatarUtils.getGuildIconURL(guild, true);

		const [isStaticLoaded, setIsStaticLoaded] = React.useState(ImageCacheUtils.hasImage(iconUrl));
		const [isAnimatedLoaded, setIsAnimatedLoaded] = React.useState(ImageCacheUtils.hasImage(hoverIconUrl));
		const [shouldPlayAnimated, setShouldPlayAnimated] = React.useState(false);

		const {attributes, listeners, setNodeRef, transform, transition, isDragging} = useSortable({id: guild.id});

		React.useEffect(() => {
			ImageCacheUtils.loadImage(iconUrl, () => setIsStaticLoaded(true));
			if (isHovering) {
				ImageCacheUtils.loadImage(hoverIconUrl, () => setIsAnimatedLoaded(true));
			}
		}, [iconUrl, hoverIconUrl, isHovering]);

		React.useEffect(() => {
			setShouldPlayAnimated(isHovering && isAnimatedLoaded);
		}, [isHovering, isAnimatedLoaded]);

		const handleSelect = () => {
			RouterUtils.transitionTo(Routes.guildChannel(guild.id, isMobileExperience ? undefined : selectedChannel));
			NavigationActionCreators.selectGuild(guild.id);
		};

		const handleContextMenu = React.useCallback(
			(event: React.MouseEvent) => {
				if (isSortingList) return;

				event.preventDefault();
				event.stopPropagation();

				if (isMobileExperience) {
					return;
				}

				ContextMenuActionCreators.openFromEvent(event, (props) => (
					<GuildContextMenu guild={guild} onClose={props.onClose} />
				));
			},
			[guild, isSortingList, isMobileExperience],
		);

		const handleOpenBottomSheet = React.useCallback(() => {
			setBottomSheetOpen(true);
		}, []);

		const handleCloseBottomSheet = React.useCallback(() => {
			setBottomSheetOpen(false);
		}, []);

		const handleLongPress = React.useCallback(() => {
			if (isSortingList) return;
			if (isMobileExperience) {
				handleOpenBottomSheet();
			}
		}, [handleOpenBottomSheet, isMobileExperience, isSortingList]);

		const indicatorHeight = isSelected ? 40 : isHovering ? 20 : 8;
		const isActive = isSelected || isHovering;

		const focusableRef = React.useRef<HTMLDivElement | null>(null);
		const focusRingTargetRef = React.useRef<HTMLDivElement | null>(null);
		const mergedRef = useMergeRefs([setNodeRef, hoverRef, focusableRef]);

		React.useEffect(() => {
			if (isSelected) {
				focusableRef.current?.scrollIntoView({block: 'nearest'});
			}
		}, [isSelected]);

		const getMutedText = () => {
			if (!isMuted) return null;
			const now = Date.now();
			if (muteConfig?.end_time && new Date(muteConfig.end_time).getTime() <= now) {
				return null;
			}
			if (muteConfig?.end_time) {
				return t`Muted until ${DateUtils.getFormattedDateTime(new Date(muteConfig.end_time))}`;
			}
			return t`Muted`;
		};

		const getNavigationKeybind = () => {
			if (guildIndex === undefined || selectedGuildIndex === undefined || selectedGuildIndex === -1) {
				return null;
			}
			if (guildIndex < selectedGuildIndex) {
				return KeybindStore.getByAction('navigate_server_previous').combo;
			}
			if (guildIndex > selectedGuildIndex) {
				return KeybindStore.getByAction('navigate_server_next').combo;
			}
			return null;
		};

		const navigationKeybind = getNavigationKeybind();

		return (
			<>
				<Tooltip
					position="right"
					maxWidth="xl"
					size="large"
					text={() =>
						!isSortingList && (
							<div className={styles.guildTooltipContainer}>
								<div className={styles.guildTooltipHeader}>
									{guild.features.has(GuildFeatures.VERIFIED) && <SealCheckIcon className={styles.guildVerifiedIcon} />}
									<span className={styles.guildTooltipName}>{guild.name}</span>
								</div>
								{guild.unavailable && (
									<span className={styles.guildTooltipMessage}>We fluxed up! Hang tight, we're working on it.</span>
								)}
								{guild.features.has(GuildFeatures.UNAVAILABLE_FOR_EVERYONE_BUT_STAFF) && (
									<span className={styles.guildTooltipError}>Only accessible to Fluxer staff</span>
								)}
								{canManageGuild && guild.features.has(GuildFeatures.INVITES_DISABLED) && (
									<span
										className={styles.guildTooltipMessage}
									>{t`Invites are currently paused in this community`}</span>
								)}
								{isMuted && (
									<div className={styles.guildMutedInfo}>
										<BellSlashIcon weight="fill" className={styles.guildMutedIcon} />
										<span className={styles.guildMutedText}>{getMutedText()}</span>
									</div>
								)}
								{hasUsersInVoice && (
									<div className={styles.guildVoiceInfo}>
										<SpeakerHighIcon className={styles.guildVoiceIcon} />
										<AvatarStack size={32} maxVisible={3}>
											{usersInVoice.map((user) => (
												<Avatar key={user.id} user={user} size={32} />
											))}
										</AvatarStack>
									</div>
								)}
								{navigationKeybind && <KeybindHint combo={navigationKeybind} />}
							</div>
						)
					}
				>
					<FocusRing focusTarget={focusableRef} ringTarget={focusRingTargetRef} offset={-2}>
						<LongPressable
							className={styles.guildListItem}
							aria-label={`${guild.name}${isSelected ? ' (selected)' : ''}`}
							aria-pressed={isSelected}
							onClick={handleSelect}
							onContextMenu={handleContextMenu}
							onKeyDown={(event) => event.key === 'Enter' && handleSelect()}
							ref={mergedRef as React.Ref<HTMLDivElement>}
							role="button"
							tabIndex={0}
							data-scroll-indicator={guildScrollSeverity}
							data-scroll-id={guildScrollId}
							data-guild-read-sentinel={guildReadSentinel}
							onLongPress={handleLongPress}
						>
							<AnimatePresence>
								{!isSortingList && (hasUnreadMessages || isSelected || isHovering) && (
									<div className={styles.guildIndicator}>
										<motion.span
											className={styles.guildIndicatorBar}
											initial={false}
											animate={{opacity: 1, scale: 1, height: indicatorHeight}}
											exit={{opacity: 0, scale: 0, height: 0}}
											transition={{duration: 0.2, ease: [0.25, 0.1, 0.25, 1]}}
										/>
									</div>
								)}
							</AnimatePresence>

							<div className={styles.relative}>
								<motion.div
									ref={focusRingTargetRef}
									{...attributes}
									{...listeners}
									tabIndex={-1}
									className={clsx(
										styles.guildIcon,
										!guild.icon && styles.guildIconNoImage,
										isSelected && styles.guildIconSelected,
									)}
									animate={{borderRadius: isActive ? '30%' : targetRadius}}
									initial={false}
									transition={{duration: 0.07, ease: 'easeOut'}}
									whileHover={{borderRadius: '30%'}}
									data-initials-length={initialsLength}
									style={{
										backgroundImage: isStaticLoaded
											? `url(${shouldPlayAnimated && isAnimatedLoaded ? hoverIconUrl : iconUrl})`
											: undefined,
										cursor: isDragging ? 'grabbing' : undefined,
										opacity: isDragging ? 0.5 : 1,
										transform: CSS.Transform.toString(transform),
										transition,
									}}
								>
									{!guild.icon && <span className={styles.guildIconInitials}>{initials}</span>}
								</motion.div>

								{!(isSortingList || guild.unavailable) && (
									<div className={clsx(styles.guildBadge, mentionCount > 0 && styles.guildBadgeActive)}>
										<MentionBadgeAnimated mentionCount={mentionCount} size="small" />
									</div>
								)}

								{hasUsersInVoice && !isSortingList && mentionCount === 0 && <VoiceBadge />}

								{canManageGuild &&
									guild.features.has(GuildFeatures.INVITES_DISABLED) &&
									!isSortingList &&
									mentionCount === 0 &&
									!hasUsersInVoice && (
										<div className={styles.guildInvitesPausedBadge}>
											<div className={styles.guildInvitesPausedBadgeInner}>
												<PauseIcon weight="fill" className={styles.guildInvitesPausedIcon} />
											</div>
										</div>
									)}

								{(guild.unavailable || guild.features.has(GuildFeatures.UNAVAILABLE_FOR_EVERYONE_BUT_STAFF)) &&
									!isSortingList && (
										<div className={styles.guildErrorBadge}>
											<div className={styles.guildErrorBadgeInner}>
												<ExclamationMarkIcon weight="regular" className={styles.guildErrorIcon} />
											</div>
										</div>
									)}
							</div>
						</LongPressable>
					</FocusRing>
				</Tooltip>
				{isMobileExperience && (
					<GuildHeaderBottomSheet isOpen={bottomSheetOpen} onClose={handleCloseBottomSheet} guild={guild} />
				)}
			</>
		);
	},
);
