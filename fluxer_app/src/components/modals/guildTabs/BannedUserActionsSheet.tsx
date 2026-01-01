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

import {useLingui} from '@lingui/react/macro';
import {EyeIcon, IdentificationCardIcon, ProhibitIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type {GuildBan} from '~/actions/GuildActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import {BanDetailsModal} from '~/components/modals/BanDetailsModal';
import {MenuBottomSheet, type MenuGroupType} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import * as AvatarUtils from '~/utils/AvatarUtils';
import styles from './GuildMemberActionsSheet.module.css';

interface BannedUserActionsSheetProps {
	isOpen: boolean;
	onClose: () => void;
	ban: GuildBan;
	onRevoke: () => void;
}

export const BannedUserActionsSheet: React.FC<BannedUserActionsSheetProps> = observer(
	({isOpen, onClose, ban, onRevoke}) => {
		const {t, i18n} = useLingui();
		const {user} = ban;
		const userTag = user.tag ?? `${user.username}#${(user.discriminator ?? '').padStart(4, '0')}`;

		const handleViewDetails = () => {
			onClose();
			ModalActionCreators.push(modal(() => <BanDetailsModal ban={ban} onRevoke={onRevoke} />));
		};

		const handleRevokeBan = () => {
			onClose();
			onRevoke();
		};

		const handleCopyUserId = () => {
			TextCopyActionCreators.copy(i18n, user.id, true);
			onClose();
		};

		const menuGroups: Array<MenuGroupType> = [
			{
				items: [
					{
						icon: <EyeIcon className={styles.icon} weight="bold" />,
						label: t`View Details`,
						onClick: handleViewDetails,
					},
				],
			},
			{
				items: [
					{
						icon: <IdentificationCardIcon className={styles.icon} weight="bold" />,
						label: t`Copy User ID`,
						onClick: handleCopyUserId,
					},
				],
			},
			{
				items: [
					{
						icon: <ProhibitIcon className={styles.icon} weight="bold" />,
						label: t`Revoke Ban`,
						onClick: handleRevokeBan,
						danger: true,
					},
				],
			},
		];

		const avatarUrl = AvatarUtils.getUserAvatarURL(user, false);

		const headerContent = (
			<div className={styles.header}>
				<img src={avatarUrl} alt="" className={styles.headerAvatarImg} />
				<div className={styles.headerInfo}>
					<span className={styles.headerName}>{user.username}</span>
					<span className={styles.headerTag}>{userTag}</span>
				</div>
			</div>
		);

		return <MenuBottomSheet isOpen={isOpen} onClose={onClose} groups={menuGroups} headerContent={headerContent} />;
	},
);
