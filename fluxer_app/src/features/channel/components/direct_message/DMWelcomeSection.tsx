// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/direct_message/DMWelcomeSection.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Relationships from '@app/features/relationship/state/Relationships';
import * as RelationshipActionUtils from '@app/features/relationship/utils/RelationshipActionUtils';
import {
	ACCEPT_FRIEND_REQUEST_ACTION_DESCRIPTOR,
	CANCEL_FRIEND_REQUEST_DESCRIPTOR,
	IGNORE_FRIEND_REQUEST_ACTION_DESCRIPTOR,
	REMOVE_FRIEND_DESCRIPTOR,
} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import {DMContextMenu} from '@app/features/ui/action_menu/DMContextMenu';
import {AvatarStack} from '@app/features/ui/avatars/AvatarStack';
import {Button} from '@app/features/ui/button/Button';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import {UserProfileBadges} from '@app/features/user/components/popouts/UserProfileBadges';
import type {Profile} from '@app/features/user/models/Profile';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg, ph, plural} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const CLAIM_YOUR_ACCOUNT_TO_SEND_FRIEND_REQUESTS_DESCRIPTOR = msg({
	message: 'Claim your account to send friend requests.',
	comment: 'Description text in the channel and chat dm welcome section.',
});
const VERIFY_YOUR_EMAIL_ADDRESS_TO_SEND_FRIEND_REQUESTS_DESCRIPTOR = msg({
	message: 'Verify your email address to send friend requests.',
	comment: 'Description text in the channel and chat dm welcome section.',
});
const logger = new Logger('DMWelcomeSection');

interface DMWelcomeSectionProps {
	userId: string;
	channel?: Channel;
}

export const DMWelcomeSection: React.FC<DMWelcomeSectionProps> = observer(function DMWelcomeSection({userId, channel}) {
	const {i18n} = useLingui();
	const user = Users.getUser(userId);
	const relationship = Relationships.getRelationship(user?.id ?? '');
	const relationshipType = relationship?.type;
	const [profile, setProfile] = useState<Profile | null>(null);
	const mobileLayout = MobileLayout;
	const profileMutualGuilds = useMemo(() => profile?.mutualGuilds ?? [], [profile?.mutualGuilds]);
	const mutualGuildRecords = useMemo(() => {
		return profileMutualGuilds
			.map((mutualGuild) => Guilds.getGuild(mutualGuild.id))
			.filter((guild): guild is Guild => guild !== undefined);
	}, [profileMutualGuilds]);
	useEffect(() => {
		if (!user) return;
		UserProfileCommands.fetch(user.id)
			.then((fetchedProfile) => {
				setProfile(fetchedProfile);
			})
			.catch((error) => {
				logger.error('Failed to fetch user profile:', error);
			});
	}, [user]);
	const openFullProfile = useCallback(() => {
		if (!user) return;
		UserProfileCommands.openUserProfile(user.id);
	}, [user]);
	const handleUserContextMenu = useCallback(
		(event: React.MouseEvent) => {
			if (!channel || !user) return;
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<DMContextMenu
					channel={channel}
					recipient={user}
					onClose={onClose}
					data-flx="channel.direct-message.dm-welcome-section.handle-user-context-menu.dm-context-menu"
				/>
			));
		},
		[channel, user],
	);
	if (!user) {
		return null;
	}
	const displayName = NicknameUtils.getNickname(user, null, channel?.id);
	const fluxerTag = NicknameUtils.formatTagForStreamerMode(
		user.id === '0' ? `${displayName}#${user.discriminator}` : user.tag,
	);
	const handleSendFriendRequest = () => {
		RelationshipActionUtils.sendFriendRequest(i18n, user.id);
	};
	const handleAcceptFriendRequest = (event?: {shiftKey?: boolean}) => {
		RelationshipActionUtils.showAcceptFriendRequestConfirmation(i18n, user, {
			bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
			showShiftBypassConfirmationTip: true,
		});
	};
	const handleRemoveFriend = (event?: {shiftKey?: boolean}) => {
		RelationshipActionUtils.showRemoveFriendConfirmation(i18n, user, {
			bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
			showShiftBypassConfirmationTip: true,
		});
	};
	const handleCancelFriendRequest = () => {
		RelationshipActionUtils.cancelFriendRequest(i18n, user.id);
	};
	const handleIgnoreFriendRequest = () => {
		RelationshipActionUtils.ignoreFriendRequest(i18n, user.id);
	};
	const hasMutualGuilds = profileMutualGuilds.length > 0;
	const currentUserUnclaimed = !(Users.currentUser?.isClaimed() ?? true);
	const shouldShowActionButton =
		!user.bot &&
		(relationshipType === undefined ||
			relationshipType === RelationshipTypes.INCOMING_REQUEST ||
			relationshipType === RelationshipTypes.OUTGOING_REQUEST ||
			relationshipType === RelationshipTypes.FRIEND);
	const mutualGuildCount = profileMutualGuilds.length;
	const currentUserUnverified = Users.currentUser?.verified === false;
	const renderActionButton = () => {
		if (user.bot) return null;
		switch (relationshipType) {
			case undefined: {
				const isDisabled = currentUserUnclaimed || currentUserUnverified;
				const tooltipText = currentUserUnclaimed
					? i18n._(CLAIM_YOUR_ACCOUNT_TO_SEND_FRIEND_REQUESTS_DESCRIPTOR)
					: i18n._(VERIFY_YOUR_EMAIL_ADDRESS_TO_SEND_FRIEND_REQUESTS_DESCRIPTOR);
				const button = (
					<Button
						small={true}
						onClick={handleSendFriendRequest}
						disabled={isDisabled}
						data-flx="channel.direct-message.dm-welcome-section.render-action-button.button.send-friend-request"
					>
						<Trans>Send friend request</Trans>
					</Button>
				);
				if (isDisabled) {
					return (
						<Tooltip
							text={tooltipText}
							maxWidth="xl"
							data-flx="channel.direct-message.dm-welcome-section.render-action-button.tooltip"
						>
							<div data-flx="channel.direct-message.dm-welcome-section.render-action-button.div">{button}</div>
						</Tooltip>
					);
				}
				return button;
			}
			case RelationshipTypes.INCOMING_REQUEST:
				return (
					<div
						className={styles.actionButtonsContainer}
						data-flx="channel.direct-message.dm-welcome-section.render-action-button.action-buttons-container"
					>
						<Button
							small={true}
							onClick={handleAcceptFriendRequest}
							data-flx="channel.direct-message.dm-welcome-section.render-action-button.button.accept-friend-request"
						>
							{i18n._(ACCEPT_FRIEND_REQUEST_ACTION_DESCRIPTOR)}
						</Button>
						<Button
							variant="secondary"
							small={true}
							onClick={handleIgnoreFriendRequest}
							data-flx="channel.direct-message.dm-welcome-section.render-action-button.button.ignore-friend-request"
						>
							{i18n._(IGNORE_FRIEND_REQUEST_ACTION_DESCRIPTOR)}
						</Button>
					</div>
				);
			case RelationshipTypes.OUTGOING_REQUEST:
				return (
					<Button
						variant="secondary"
						small={true}
						onClick={handleCancelFriendRequest}
						data-flx="channel.direct-message.dm-welcome-section.render-action-button.button.cancel-friend-request"
					>
						{i18n._(CANCEL_FRIEND_REQUEST_DESCRIPTOR)}
					</Button>
				);
			case RelationshipTypes.FRIEND:
				return (
					<Button
						variant="secondary"
						small={true}
						onClick={handleRemoveFriend}
						data-flx="channel.direct-message.dm-welcome-section.render-action-button.button.remove-friend"
					>
						{i18n._(REMOVE_FRIEND_DESCRIPTOR)}
					</Button>
				);
			default:
				return null;
		}
	};
	const renderMutualGuilds = () => {
		if (!hasMutualGuilds) return null;
		return (
			<div
				className={styles.mutualGuildsContainer}
				data-flx="channel.direct-message.dm-welcome-section.render-mutual-guilds.mutual-guilds-container"
			>
				{mutualGuildRecords.length > 0 && (
					<AvatarStack
						size={32}
						maxVisible={3}
						data-flx="channel.direct-message.dm-welcome-section.render-mutual-guilds.avatar-stack"
					>
						{mutualGuildRecords.map((guild) => (
							<div
								key={guild.id}
								className={styles.guildIconWrapper}
								data-flx="channel.direct-message.dm-welcome-section.render-mutual-guilds.guild-icon-wrapper"
							>
								<GuildIcon
									id={guild.id}
									name={guild.name}
									icon={guild.icon}
									className={styles.guildIcon}
									sizePx={32}
									data-flx="channel.direct-message.dm-welcome-section.render-mutual-guilds.guild-icon"
								/>
							</div>
						))}
					</AvatarStack>
				)}
				<span
					className={styles.mutualGuildsText}
					data-flx="channel.direct-message.dm-welcome-section.render-mutual-guilds.mutual-guilds-text"
				>
					{plural(
						{count: mutualGuildCount},
						{
							one: '# mutual community',
							other: '# mutual communities',
						},
					)}
				</span>
			</div>
		);
	};
	return (
		<div className={styles.welcomeSection} data-flx="channel.direct-message.dm-welcome-section.welcome-section">
			<div className={styles.profileSection} data-flx="channel.direct-message.dm-welcome-section.profile-section">
				<FocusRing offset={-2} data-flx="channel.direct-message.dm-welcome-section.focus-ring">
					<button
						type="button"
						onClick={openFullProfile}
						onContextMenu={handleUserContextMenu}
						className={styles.avatarButton}
						data-flx="channel.direct-message.dm-welcome-section.avatar-button.open-full-profile"
					>
						<StatusAwareAvatar
							user={user}
							size={80}
							showOffline={true}
							data-flx="channel.direct-message.dm-welcome-section.status-aware-avatar"
						/>
					</button>
				</FocusRing>
				<FocusRing offset={-2} data-flx="channel.direct-message.dm-welcome-section.focus-ring--2">
					<button
						type="button"
						onClick={openFullProfile}
						onContextMenu={handleUserContextMenu}
						className={styles.usernameButton}
						data-flx="channel.direct-message.dm-welcome-section.username-button.open-full-profile"
					>
						<span className={styles.username} data-flx="channel.direct-message.dm-welcome-section.username">
							{fluxerTag}
						</span>
					</button>
				</FocusRing>
				<UserProfileBadges
					user={user}
					profile={profile}
					isModal={true}
					isMobile={mobileLayout.enabled}
					data-flx="channel.direct-message.dm-welcome-section.user-profile-badges"
				/>
			</div>
			<p className={styles.welcomeText} data-flx="channel.direct-message.dm-welcome-section.welcome-text">
				<Trans>
					Say hi to <strong data-flx="channel.direct-message.dm-welcome-section.strong">{ph({displayName})}</strong>.
					Your DM starts here.
				</Trans>
			</p>
			{(hasMutualGuilds || shouldShowActionButton) && (
				<div className={styles.actionSection} data-flx="channel.direct-message.dm-welcome-section.action-section">
					{renderMutualGuilds()}
					{shouldShowActionButton && renderActionButton()}
				</div>
			)}
		</div>
	);
});
