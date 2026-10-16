// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildBan} from '@app/features/guild/commands/GuildCommands';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildMemberActionsSheet.module.css';
import {COPY_USER_ID_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {BanDetailsModal} from '@app/features/moderation/components/modals/BanDetailsModal';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {MenuBottomSheet, type MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {formatUserTag} from '@app/features/user/utils/UserTagUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {EyeIcon, IdentificationCardIcon, ProhibitIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const VIEW_DETAILS_DESCRIPTOR = msg({
	message: 'View details',
	comment:
		'Button or menu action label in the banned user actions sheet. Keep it concise. Keep the tone plain and specific.',
});
const REVOKE_BAN_DESCRIPTOR = msg({
	message: 'Revoke ban',
	comment: 'Short label in the banned user actions sheet. Keep it concise. Keep the tone plain and specific.',
});

interface BannedUserActionsSheetProps {
	isOpen: boolean;
	onClose: () => void;
	ban: GuildBan;
	onRevoke: () => void;
}

export const BannedUserActionsSheet: React.FC<BannedUserActionsSheetProps> = observer(
	({isOpen, onClose, ban, onRevoke}) => {
		const {i18n} = useLingui();
		const {user} = ban;
		const userDisplayName = NicknameUtils.getDisplayName(user);
		const userTag = NicknameUtils.formatTagForStreamerMode(
			user.tag ?? formatUserTag({...user, discriminator: (user.discriminator ?? '').padStart(4, '0')}),
		);
		const handleViewDetails = () => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<BanDetailsModal
						ban={ban}
						onRevoke={onRevoke}
						data-flx="guild.guild-tabs.banned-user-actions-sheet.handle-view-details.ban-details-modal"
					/>
				)),
			);
		};
		const handleRevokeBan = () => {
			onClose();
			onRevoke();
		};
		const handleCopyUserId = () => {
			TextCopyCommands.copy(i18n, user.id, true);
			onClose();
		};
		const menuGroups: Array<MenuGroupType> = [
			{
				items: [
					{
						icon: (
							<EyeIcon
								className={styles.icon}
								weight="bold"
								data-flx="guild.guild-tabs.banned-user-actions-sheet.icon"
							/>
						),
						label: i18n._(VIEW_DETAILS_DESCRIPTOR),
						onClick: handleViewDetails,
					},
				],
			},
			{
				items: [
					{
						icon: (
							<IdentificationCardIcon
								className={styles.icon}
								weight="bold"
								data-flx="guild.guild-tabs.banned-user-actions-sheet.icon--2"
							/>
						),
						label: i18n._(COPY_USER_ID_DESCRIPTOR),
						onClick: handleCopyUserId,
					},
				],
			},
			{
				items: [
					{
						icon: (
							<ProhibitIcon
								className={styles.icon}
								weight="bold"
								data-flx="guild.guild-tabs.banned-user-actions-sheet.icon--3"
							/>
						),
						label: i18n._(REVOKE_BAN_DESCRIPTOR),
						onClick: handleRevokeBan,
						danger: true,
					},
				],
			},
		];
		const avatarUrl = AvatarUtils.getUserAvatarURL(user, false);
		const headerContent = (
			<div className={styles.header} data-flx="guild.guild-tabs.banned-user-actions-sheet.header">
				<img
					src={avatarUrl}
					alt=""
					className={styles.headerAvatarImg}
					data-flx="guild.guild-tabs.banned-user-actions-sheet.header-avatar-img"
				/>
				<div className={styles.headerInfo} data-flx="guild.guild-tabs.banned-user-actions-sheet.header-info">
					<span className={styles.headerName} data-flx="guild.guild-tabs.banned-user-actions-sheet.header-name">
						{userDisplayName}
					</span>
					<span className={styles.headerTag} data-flx="guild.guild-tabs.banned-user-actions-sheet.header-tag">
						{userTag}
					</span>
				</div>
			</div>
		);
		return (
			<MenuBottomSheet
				isOpen={isOpen}
				onClose={onClose}
				groups={menuGroups}
				headerContent={headerContent}
				data-flx="guild.guild-tabs.banned-user-actions-sheet.menu-bottom-sheet"
			/>
		);
	},
);
