// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer, SettingsTabContent} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {
	COPY_USER_ID_DESCRIPTOR,
	COPY_USERNAME_DESCRIPTOR,
	VIEW_PROFILE_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Relationships from '@app/features/relationship/state/Relationships';
import * as RelationshipActionUtils from '@app/features/relationship/utils/RelationshipActionUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {Button} from '@app/features/ui/button/Button';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import styles from '@app/features/user/components/modals/tabs/BlockedUsersTab.module.css';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CopyIcon, DotsThreeVerticalIcon, IdentificationCardIcon, ProhibitIcon, UserIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const VIEW_S_PROFILE_DESCRIPTOR = msg({
	message: "View {userName}'s profile",
	comment:
		'Button or menu action label in the blocked users tab. Keep it concise. Preserve {userName}; it is inserted by code. Keep the tone plain and specific.',
});
const MORE_ACTIONS_FOR_DESCRIPTOR = msg({
	message: 'More actions for {displayName}',
	comment:
		'Label in the blocked users tab. Preserve {displayName}; it is inserted by code. Keep the tone plain and specific.',
});

export const BlockedUsersContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const relationships = Relationships.getRelationships();
	const blockedUsers = useMemo(() => {
		return [...relationships]
			.filter((rel) => rel.type === RelationshipTypes.BLOCKED)
			.sort((a, b) => {
				const userA = Users.getUser(a.id);
				const userB = Users.getUser(b.id);
				if (!userA || !userB) return 0;
				return NicknameUtils.getNickname(userA, null).localeCompare(NicknameUtils.getNickname(userB, null));
			});
	}, [relationships]);
	const handleUnblockUser = (userId: string, event?: {shiftKey?: boolean}) => {
		const user = Users.getUser(userId);
		if (!user) return;
		RelationshipActionUtils.showUnblockUserConfirmation(i18n, user, {
			bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
			showShiftBypassConfirmationTip: true,
		});
	};
	const handleViewProfile = useCallback((userId: string) => {
		UserProfileCommands.openUserProfile(userId);
	}, []);
	const handleMoreOptionsClick = useCallback(
		(userId: string, event: React.MouseEvent<HTMLButtonElement>) => {
			const user = Users.getUser(userId);
			if (!user) return;
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<>
					<MenuGroup data-flx="user.blocked-users-tab.handle-more-options-click.menu-group">
						<MenuItem
							icon={
								<UserIcon size={remFromPx(16)} data-flx="user.blocked-users-tab.handle-more-options-click.user-icon" />
							}
							onClick={() => {
								onClose();
								handleViewProfile(userId);
							}}
							data-flx="user.blocked-users-tab.handle-more-options-click.menu-item.close"
						>
							{i18n._(VIEW_PROFILE_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
					<MenuGroup data-flx="user.blocked-users-tab.handle-more-options-click.menu-group--2">
						<MenuItem
							icon={
								<CopyIcon size={remFromPx(16)} data-flx="user.blocked-users-tab.handle-more-options-click.copy-icon" />
							}
							onClick={() => {
								onClose();
								TextCopyCommands.copy(i18n, user.tag, true);
							}}
							data-flx="user.blocked-users-tab.handle-more-options-click.menu-item.close--2"
						>
							{i18n._(COPY_USERNAME_DESCRIPTOR)}
						</MenuItem>
						<MenuItem
							icon={
								<IdentificationCardIcon
									size={remFromPx(16)}
									data-flx="user.blocked-users-tab.handle-more-options-click.identification-card-icon"
								/>
							}
							onClick={() => {
								onClose();
								TextCopyCommands.copy(i18n, user.id, true);
							}}
							data-flx="user.blocked-users-tab.handle-more-options-click.menu-item.close--3"
						>
							{i18n._(COPY_USER_ID_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
				</>
			));
		},
		[handleViewProfile],
	);
	return (
		<>
			{blockedUsers.length === 0 ? (
				<StatusSlate
					Icon={ProhibitIcon}
					title={<Trans>No blocked users</Trans>}
					description={<Trans>You haven't blocked anyone yet.</Trans>}
					data-flx="user.blocked-users-tab.status-slate"
				/>
			) : (
				<div className={styles.userList} data-flx="user.blocked-users-tab.user-list">
					{blockedUsers.map((relationship) => {
						const user = Users.getUser(relationship.id);
						if (!user) return null;
						const displayName = NicknameUtils.getNickname(user, null);
						return (
							<div key={user.id} className={styles.userCard} data-flx="user.blocked-users-tab.user-card">
								<div className={styles.userInfo} data-flx="user.blocked-users-tab.user-info">
									<button
										type="button"
										className={styles.avatarButton}
										onClick={() => handleViewProfile(user.id)}
										aria-label={i18n._(VIEW_S_PROFILE_DESCRIPTOR, {userName: displayName})}
										data-flx="user.blocked-users-tab.avatar-button.view-profile"
									>
										<StatusAwareAvatar
											user={user}
											size={32}
											disablePresence={true}
											data-flx="user.blocked-users-tab.status-aware-avatar"
										/>
									</button>
									<button
										type="button"
										className={styles.usernameButton}
										onClick={() => handleViewProfile(user.id)}
										aria-label={i18n._(VIEW_S_PROFILE_DESCRIPTOR, {userName: displayName})}
										data-flx="user.blocked-users-tab.username-button.view-profile"
									>
										<div className={styles.usernameContainer} data-flx="user.blocked-users-tab.username-container">
											<span className={styles.username} data-flx="user.blocked-users-tab.username">
												{displayName}
											</span>
											<span className={styles.discriminator} data-flx="user.blocked-users-tab.discriminator">
												{NicknameUtils.formatUserTagForStreamerMode(user)}
											</span>
										</div>
									</button>
								</div>
								<div className={styles.actions} data-flx="user.blocked-users-tab.actions">
									<Button
										variant="secondary"
										small={true}
										onClick={(event: React.MouseEvent<HTMLButtonElement>) => handleUnblockUser(user.id, event)}
										data-flx="user.blocked-users-tab.button.unblock-user"
									>
										<Trans>Unblock</Trans>
									</Button>
									<Button
										variant="secondary"
										small={true}
										square={true}
										icon={
											<DotsThreeVerticalIcon
												weight="bold"
												className={styles.moreIcon}
												data-flx="user.blocked-users-tab.more-icon"
											/>
										}
										aria-label={i18n._(MORE_ACTIONS_FOR_DESCRIPTOR, {displayName})}
										onClick={(event: React.MouseEvent<HTMLButtonElement>) => handleMoreOptionsClick(user.id, event)}
										data-flx="user.blocked-users-tab.button.more-options-click"
									/>
								</div>
							</div>
						);
					})}
				</div>
			)}
		</>
	);
});

const BlockedUsersTab: React.FC = observer(() => {
	return (
		<SettingsTabContainer data-flx="user.blocked-users-tab.settings-tab-container">
			<SettingsTabContent data-flx="user.blocked-users-tab.settings-tab-content">
				<SettingsSection
					id="blocked-users"
					title={<Trans>Blocked users</Trans>}
					description={<Trans>Blocked users can't send you friend requests or message you directly.</Trans>}
					data-flx="user.blocked-users-tab.blocked-users"
				>
					<BlockedUsersContent data-flx="user.blocked-users-tab.blocked-users-content" />
				</SettingsSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default BlockedUsersTab;
