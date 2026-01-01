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
import {CopyIcon, DotsThreeVerticalIcon, IdentificationCardIcon, ProhibitIcon, UserIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as RelationshipActionCreators from '~/actions/RelationshipActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import {RelationshipTypes} from '~/Constants';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {StatusSlate} from '~/components/modals/shared/StatusSlate';
import styles from '~/components/modals/tabs/BlockedUsersTab.module.css';
import {Button} from '~/components/uikit/Button/Button';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import {Scroller} from '~/components/uikit/Scroller';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import RelationshipStore from '~/stores/RelationshipStore';
import UserStore from '~/stores/UserStore';

const BlockedUsersTab: React.FC = observer(() => {
	const {t, i18n} = useLingui();
	const relationships = RelationshipStore.getRelationships();
	const blockedUsers = React.useMemo(() => {
		return relationships.filter((rel) => rel.type === RelationshipTypes.BLOCKED);
	}, [relationships]);

	const handleUnblockUser = (userId: string) => {
		const user = UserStore.getUser(userId);
		if (!user) return;

		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Unblock User`}
					description={t`Are you sure you want to unblock ${user.username}?`}
					primaryText={t`Unblock`}
					primaryVariant="primary"
					onPrimary={async () => {
						RelationshipActionCreators.removeRelationship(userId);
					}}
				/>
			)),
		);
	};

	const handleViewProfile = React.useCallback((userId: string) => {
		UserProfileActionCreators.openUserProfile(userId);
	}, []);

	const handleMoreOptionsClick = React.useCallback(
		(userId: string, event: React.MouseEvent<HTMLButtonElement>) => {
			const user = UserStore.getUser(userId);
			if (!user) return;

			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<>
					<MenuGroup>
						<MenuItem
							icon={<UserIcon size={16} />}
							onClick={() => {
								onClose();
								handleViewProfile(userId);
							}}
						>
							{t`View Profile`}
						</MenuItem>
					</MenuGroup>
					<MenuGroup>
						<MenuItem
							icon={<CopyIcon size={16} />}
							onClick={() => {
								onClose();
								TextCopyActionCreators.copy(i18n, user.tag, true);
							}}
						>
							{t`Copy FluxerTag`}
						</MenuItem>
						<MenuItem
							icon={<IdentificationCardIcon size={16} />}
							onClick={() => {
								onClose();
								TextCopyActionCreators.copy(i18n, user.id, true);
							}}
						>
							{t`Copy User ID`}
						</MenuItem>
					</MenuGroup>
				</>
			));
		},
		[handleViewProfile],
	);

	if (blockedUsers.length === 0) {
		return (
			<StatusSlate
				Icon={ProhibitIcon}
				title={<Trans>No Blocked Users</Trans>}
				description={<Trans>You haven't blocked anyone yet.</Trans>}
				fullHeight={true}
			/>
		);
	}

	return (
		<div className={styles.container}>
			<div className={styles.header}>
				<h2 className={styles.title}>
					<Trans>Blocked Users</Trans>
				</h2>
				<p className={styles.description}>
					<Trans>Blocked users can't send you friend requests or message you directly.</Trans>
				</p>
			</div>
			<div className={styles.scrollContainer}>
				<Scroller className={styles.scrollerPadding} key="blocked-users-scroller">
					<div className={styles.userList}>
						{blockedUsers.map((relationship) => {
							const user = UserStore.getUser(relationship.id);
							if (!user) return null;

							const moreOptionsButtonRef = React.createRef<HTMLButtonElement>();

							return (
								<div key={user.id} className={styles.userCard}>
									<div className={styles.userInfo}>
										<button
											type="button"
											className={styles.avatarButton}
											onClick={() => handleViewProfile(user.id)}
											aria-label={`View ${user.username}'s profile`}
										>
											<StatusAwareAvatar user={user} size={40} disablePresence={true} />
										</button>
										<button
											type="button"
											className={styles.usernameButton}
											onClick={() => handleViewProfile(user.id)}
											aria-label={`View ${user.username}'s profile`}
										>
											<div className={styles.usernameContainer}>
												<span className={styles.username}>{user.username}</span>
												<span className={styles.discriminator}>#{user.discriminator}</span>
											</div>
										</button>
									</div>
									<div className={styles.actions}>
										<Button variant="secondary" small={true} onClick={() => handleUnblockUser(user.id)}>
											<Trans>Unblock</Trans>
										</Button>
										<Button
											ref={moreOptionsButtonRef}
											variant="secondary"
											small={true}
											square={true}
											icon={<DotsThreeVerticalIcon weight="bold" className={styles.moreIcon} />}
											onClick={(event: React.MouseEvent<HTMLButtonElement>) => handleMoreOptionsClick(user.id, event)}
										/>
									</div>
								</div>
							);
						})}
					</div>
				</Scroller>
			</div>
		</div>
	);
});

export default BlockedUsersTab;
