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
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as RelationshipActionCreators from '~/actions/RelationshipActionCreators';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import {RelationshipTypes} from '~/Constants';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import {UserProfileBadges} from '~/components/popouts/UserProfileBadges';
import {AvatarStack} from '~/components/uikit/avatars/AvatarStack';
import {Button} from '~/components/uikit/Button/Button';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import type {ProfileRecord} from '~/records/ProfileRecord';
import GuildMemberStore from '~/stores/GuildMemberStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import RelationshipStore from '~/stores/RelationshipStore';
import UserStore from '~/stores/UserStore';
import styles from './DMWelcomeSection.module.css';

interface DMWelcomeSectionProps {
	userId: string;
}

export const DMWelcomeSection: React.FC<DMWelcomeSectionProps> = observer(function DMWelcomeSection({userId}) {
	const {t} = useLingui();

	const user = UserStore.getUser(userId);
	const mutualGuilds = GuildMemberStore.getMutualGuilds(user?.id ?? '');
	const relationship = RelationshipStore.getRelationship(user?.id ?? '');
	const relationshipType = relationship?.type;
	const [profile, setProfile] = React.useState<ProfileRecord | null>(null);
	const mobileLayout = MobileLayoutStore;

	React.useEffect(() => {
		if (!user) return;
		UserProfileActionCreators.fetch(user.id)
			.then((fetchedProfile) => {
				setProfile(fetchedProfile);
			})
			.catch((error) => {
				console.error('Failed to fetch user profile:', error);
			});
	}, [user]);

	const openFullProfile = React.useCallback(() => {
		if (!user) return;

		UserProfileActionCreators.openUserProfile(user.id);
	}, [user]);

	if (!user) {
		return null;
	}

	const handleSendFriendRequest = () => {
		RelationshipActionCreators.sendFriendRequest(user.id);
	};

	const handleAcceptFriendRequest = () => {
		RelationshipActionCreators.acceptFriendRequest(user.id);
	};

	const handleRemoveFriend = () => {
		RelationshipActionCreators.removeRelationship(user.id);
	};

	const handleCancelFriendRequest = () => {
		RelationshipActionCreators.removeRelationship(user.id);
	};

	const handleIgnoreFriendRequest = () => {
		RelationshipActionCreators.removeRelationship(user.id);
	};

	const hasMutualGuilds = mutualGuilds.length > 0;
	const shouldShowActionButton =
		!user.bot &&
		(relationshipType === undefined ||
			relationshipType === RelationshipTypes.INCOMING_REQUEST ||
			relationshipType === RelationshipTypes.OUTGOING_REQUEST ||
			relationshipType === RelationshipTypes.FRIEND);
	const mutualGuildCount = mutualGuilds.length;

	const renderActionButton = () => {
		if (user.bot) return null;
		switch (relationshipType) {
			case undefined:
				return (
					<Button small={true} onClick={handleSendFriendRequest}>
						<Trans>Send Friend Request</Trans>
					</Button>
				);
			case RelationshipTypes.INCOMING_REQUEST:
				return (
					<div className={styles.actionButtonsContainer}>
						<Button small={true} onClick={handleAcceptFriendRequest}>
							<Trans>Accept</Trans>
						</Button>
						<Button variant="secondary" small={true} onClick={handleIgnoreFriendRequest}>
							<Trans>Ignore</Trans>
						</Button>
					</div>
				);
			case RelationshipTypes.OUTGOING_REQUEST:
				return (
					<Button variant="secondary" small={true} onClick={handleCancelFriendRequest}>
						<Trans>Cancel Friend Request</Trans>
					</Button>
				);
			case RelationshipTypes.FRIEND:
				return (
					<Button variant="secondary" small={true} onClick={handleRemoveFriend}>
						<Trans>Remove Friend</Trans>
					</Button>
				);
			default:
				return null;
		}
	};

	const renderMutualGuilds = () => {
		if (!hasMutualGuilds) return null;

		return (
			<div className={styles.mutualGuildsContainer}>
				<AvatarStack size={32} maxVisible={3}>
					{mutualGuilds.map((guild) => (
						<div key={guild.id} className={styles.guildIconWrapper}>
							<GuildIcon id={guild.id} name={guild.name} icon={guild.icon} className={styles.guildIcon} sizePx={32} />
						</div>
					))}
				</AvatarStack>

				<span className={styles.mutualGuildsText}>
					{mutualGuildCount === 1
						? t`${mutualGuildCount} mutual commmunity`
						: t`${mutualGuildCount} mutual communities`}
				</span>
			</div>
		);
	};

	return (
		<div className={styles.welcomeSection}>
			<div className={styles.profileSection}>
				<FocusRing offset={-2}>
					<button type="button" onClick={openFullProfile} className={styles.avatarButton}>
						<StatusAwareAvatar user={user} size={80} showOffline={true} />
					</button>
				</FocusRing>

				<FocusRing offset={-2}>
					<button type="button" onClick={openFullProfile} className={styles.usernameButton}>
						<span className={styles.username}>{user.username}</span>
						<span className={styles.discriminator}>#{user.discriminator}</span>
					</button>
				</FocusRing>

				<UserProfileBadges user={user} profile={profile} isModal={true} isMobile={mobileLayout.enabled} />
			</div>

			<p className={styles.welcomeText}>
				<Trans>
					This is the beginning of your direct message history with <strong>{user.username}</strong>.
				</Trans>
			</p>

			{(hasMutualGuilds || shouldShowActionButton) && (
				<div className={styles.actionSection}>
					{renderMutualGuilds()}
					{shouldShowActionButton && renderActionButton()}
				</div>
			)}
		</div>
	);
});
