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
import {CheckIcon, DotsThreeVerticalIcon, UserIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as RelationshipActionCreators from '~/actions/RelationshipActionCreators';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import {RelationshipTypes} from '~/Constants';
import {LongPressable} from '~/components/LongPressable';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import type {MenuGroupType} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {MenuBottomSheet} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import UserStore from '~/stores/UserStore';
import * as NicknameUtils from '~/utils/NicknameUtils';
import styles from './MobileFriendRequestItem.module.css';

interface MobileFriendRequestItemProps {
	userId: string;
	relationshipType: number;
}

export const MobileFriendRequestItem: React.FC<MobileFriendRequestItemProps> = observer(
	({userId, relationshipType}) => {
		const {t} = useLingui();

		const [menuOpen, setMenuOpen] = React.useState(false);
		const user = UserStore.getUser(userId);

		if (!user) return null;

		const handleViewProfile = () => {
			UserProfileActionCreators.openUserProfile(user.id);
			setMenuOpen(false);
		};

		const handleAccept = () => {
			RelationshipActionCreators.acceptFriendRequest(userId);
			setMenuOpen(false);
		};

		const handleIgnore = () => {
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Ignore Friend Request`}
						description={t`Are you sure you want to ignore the friend request from ${user.displayName}?`}
						primaryText={t`Ignore`}
						onPrimary={() => RelationshipActionCreators.removeRelationship(userId)}
					/>
				)),
			);
			setMenuOpen(false);
		};

		const handleCancel = () => {
			RelationshipActionCreators.removeRelationship(userId);
			setMenuOpen(false);
		};

		const menuGroups: Array<MenuGroupType> = [
			{
				items: [
					{
						icon: <UserIcon weight="fill" className={styles.iconSize} />,
						label: t`View Profile`,
						onClick: handleViewProfile,
					},
				],
			},
		];

		if (relationshipType === RelationshipTypes.INCOMING_REQUEST) {
			menuGroups.push({
				items: [
					{
						icon: <CheckIcon weight="bold" className={styles.iconSize} />,
						label: t`Accept`,
						onClick: handleAccept,
					},
					{
						icon: <XIcon weight="bold" className={styles.iconSize} />,
						label: t`Ignore`,
						onClick: handleIgnore,
						danger: true,
					},
				],
			});
		} else if (relationshipType === RelationshipTypes.OUTGOING_REQUEST) {
			menuGroups.push({
				items: [
					{
						icon: <XIcon weight="bold" className={styles.iconSize} />,
						label: t`Cancel Request`,
						onClick: handleCancel,
						danger: true,
					},
				],
			});
		}

		const statusText =
			relationshipType === RelationshipTypes.INCOMING_REQUEST ? (
				<Trans>Incoming friend request</Trans>
			) : (
				<Trans>Friend request sent</Trans>
			);

		return (
			<>
				<LongPressable className={styles.requestItem} onLongPress={() => setMenuOpen(true)}>
					<StatusAwareAvatar user={user} size={40} />
					<div className={styles.userInfo}>
						<span className={styles.userName}>{NicknameUtils.getNickname(user)}</span>
						<span className={styles.requestStatus}>{statusText}</span>
					</div>
					<FocusRing offset={-2}>
						<button type="button" onClick={() => setMenuOpen(true)} className={styles.actionButton}>
							<DotsThreeVerticalIcon weight="bold" className={styles.iconSize} />
						</button>
					</FocusRing>
				</LongPressable>

				<MenuBottomSheet isOpen={menuOpen} onClose={() => setMenuOpen(false)} groups={menuGroups} />
			</>
		);
	},
);
