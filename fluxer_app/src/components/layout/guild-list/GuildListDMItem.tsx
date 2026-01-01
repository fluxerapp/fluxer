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

import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import {ChannelTypes} from '~/Constants';
import {DMBottomSheet} from '~/components/bottomsheets/DMBottomSheet';
import {UserTag} from '~/components/channel/UserTag';
import {GroupDMAvatar} from '~/components/common/GroupDMAvatar';
import {LongPressable} from '~/components/LongPressable';
import {DMContextMenu} from '~/components/uikit/ContextMenu/DMContextMenu';
import {GroupDMContextMenu} from '~/components/uikit/ContextMenu/GroupDMContextMenu';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {MentionBadgeAnimated} from '~/components/uikit/MentionBadge';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useHover} from '~/hooks/useHover';
import {useMergeRefs} from '~/hooks/useMergeRefs';
import {Routes} from '~/Routes';
import type {ChannelRecord} from '~/records/ChannelRecord';
import KeyboardModeStore from '~/stores/KeyboardModeStore';
import ReadStateStore from '~/stores/ReadStateStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import UserStore from '~/stores/UserStore';
import * as ChannelUtils from '~/utils/ChannelUtils';
import {isMobileExperienceEnabled} from '~/utils/mobileExperience';
import * as NicknameUtils from '~/utils/NicknameUtils';
import * as RouterUtils from '~/utils/RouterUtils';
import guildStyles from '../GuildsLayout.module.css';
import type {ScrollIndicatorSeverity} from '../ScrollIndicatorOverlay';
import styles from './GuildListDMItem.module.css';
import {VoiceBadge} from './VoiceBadge';

interface DMListItemProps {
	channel: ChannelRecord;
	isSelected: boolean;
	className?: string;
	voiceCallActive?: boolean;
}

export const DMListItem = observer(({channel, isSelected, className, voiceCallActive = false}: DMListItemProps) => {
	const [hoverRef, isHovering] = useHover();
	const buttonRef = React.useRef<HTMLButtonElement | null>(null);
	const iconRef = React.useRef<HTMLDivElement | null>(null);
	const mergedButtonRef = useMergeRefs([hoverRef, buttonRef]);
	const [bottomSheetOpen, setBottomSheetOpen] = React.useState(false);
	const isMobileExperience = isMobileExperienceEnabled();
	const [isFocused, setIsFocused] = React.useState(false);
	const {keyboardModeEnabled} = KeyboardModeStore;

	const mentionCount = ReadStateStore.getMentionCount(channel.id);
	const hasUnreadMessages = ReadStateStore.hasUnread(channel.id);
	const dmScrollSeverity: ScrollIndicatorSeverity | undefined =
		mentionCount > 0 ? 'mention' : hasUnreadMessages ? 'unread' : undefined;
	const dmScrollId = `dm-${channel.id}`;

	const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
	const recipient = !isGroupDM ? UserStore.getUser(channel.recipientIds[0]) : null;
	const isMuted = UserGuildSettingsStore.isChannelMuted(null, channel.id);

	const handleSelect = () => {
		RouterUtils.transitionTo(Routes.dmChannel(channel.id));
	};

	const handleOpenBottomSheet = React.useCallback(() => {
		setBottomSheetOpen(true);
	}, []);

	const handleCloseBottomSheet = React.useCallback(() => {
		setBottomSheetOpen(false);
	}, []);

	const handleLongPress = React.useCallback(() => {
		if (isMobileExperience) {
			handleOpenBottomSheet();
		}
	}, [isMobileExperience, handleOpenBottomSheet]);

	const handleContextMenu = React.useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();

			if (isMobileExperience) {
				return;
			}

			ContextMenuActionCreators.openFromEvent(event, (props) =>
				isGroupDM ? (
					<GroupDMContextMenu channel={channel} onClose={props.onClose} />
				) : (
					<DMContextMenu channel={channel} recipient={recipient} onClose={props.onClose} />
				),
			);
		},
		[channel, isGroupDM, recipient, isMobileExperience],
	);

	if (!isGroupDM && !recipient) return null;

	const indicatorHeight = isSelected ? 40 : isHovering ? 20 : 8;
	const directMessageName = recipient ? NicknameUtils.getNickname(recipient) : null;
	const computedDisplayName = ChannelUtils.getDMDisplayName(channel);
	const displayName = isGroupDM ? computedDisplayName : (directMessageName ?? computedDisplayName);
	const tooltipText = displayName;
	const ariaLabel = displayName;

	const showControls = isHovering || (keyboardModeEnabled && isFocused);

	return (
		<>
			<Tooltip position="right" size="large" text={tooltipText}>
				<LongPressable
					className={clsx(guildStyles.dmListItem, className, isMuted && styles.muted)}
					onLongPress={handleLongPress}
					data-scroll-indicator={dmScrollSeverity}
					data-scroll-id={dmScrollId}
				>
					<FocusRing offset={-2} focusTarget={buttonRef} ringTarget={iconRef}>
						<button
							type="button"
							className={styles.button}
							aria-label={`${ariaLabel}${isSelected ? ' (selected)' : ''}`}
							aria-pressed={isSelected}
							onClick={handleSelect}
							onContextMenu={handleContextMenu}
							onFocus={() => setIsFocused(true)}
							onBlur={() => setIsFocused(false)}
							ref={mergedButtonRef}
						>
							<AnimatePresence>
								{(hasUnreadMessages || isSelected || showControls) && (
									<div className={guildStyles.guildIndicator}>
										<motion.span
											className={guildStyles.guildIndicatorBar}
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
									ref={iconRef}
									className={guildStyles.dmIcon}
									animate={{borderRadius: isSelected || showControls ? '30%' : '50%'}}
									initial={false}
									transition={{duration: 0.07, ease: 'easeOut'}}
									whileHover={{borderRadius: '30%'}}
								>
									{isGroupDM ? (
										<GroupDMAvatar channel={channel} size={48} disableStatusIndicator />
									) : (
										recipient && (
											<StatusAwareAvatar
												disablePresence={true}
												user={recipient}
												size={48}
												className={styles.fullSize}
											/>
										)
									)}
								</motion.div>

								<div className={clsx(guildStyles.guildBadge, mentionCount > 0 && guildStyles.guildBadgeActive)}>
									<MentionBadgeAnimated mentionCount={mentionCount} size="small" />
								</div>

								{voiceCallActive && <VoiceBadge />}

								{!isGroupDM && recipient?.bot && (
									<div className={styles.userTagWrapper}>
										<UserTag size="sm" system={recipient.system} />
									</div>
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
				/>
			)}
		</>
	);
});
