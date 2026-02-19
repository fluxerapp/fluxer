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

import * as GuildStickerActionCreators from '@app/actions/GuildStickerActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import {EditGuildStickerModal} from '@app/components/modals/EditGuildStickerModal';
import styles from '@app/components/stickers/StickerGridItem.module.css';
import {Checkbox} from '@app/components/uikit/checkbox/Checkbox';
import FocusRing from '@app/components/uikit/focus_ring/FocusRing';
import {Tooltip} from '@app/components/uikit/tooltip/Tooltip';
import {useStickerAnimation} from '@app/hooks/useStickerAnimation';
import GuildStore from '@app/stores/GuildStore';
import * as AvatarUtils from '@app/utils/AvatarUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {GuildStickerWithUser} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import {useLingui} from '@lingui/react/macro';
import {PencilIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

interface StickerGridItemProps {
	guildId: string;
	sticker: GuildStickerWithUser;
	canModify: boolean;
	onUpdate: () => void;
}

export const StickerGridItem = observer(function StickerGridItem({
	guildId,
	sticker,
	canModify,
	onUpdate,
}: StickerGridItemProps) {
	const {t} = useLingui();
	const {shouldAnimate} = useStickerAnimation();

	const stickerName = sticker.name;
	const guild = GuildStore.getGuild(guildId);
	const canExpressionPurge = guild?.features.has(GuildFeatures.EXPRESSION_PURGE_ALLOWED) ?? false;

	const handleEdit = () => {
		ModalActionCreators.push(
			ModalActionCreators.modal(() => (
				<EditGuildStickerModal guildId={guildId} sticker={sticker} onUpdate={onUpdate} />
			)),
		);
	};

	const handleDelete = () => {
		ModalActionCreators.push(
			ModalActionCreators.modal(() => (
				<ConfirmModal
					title={t`Delete Sticker`}
					description={t`Are you sure you want to delete "${stickerName}"? This action cannot be undone.`}
					primaryText={t`Delete`}
					primaryVariant="danger-primary"
					checkboxContent={
						canExpressionPurge ? <Checkbox>{t`Purge this sticker from storage and CDN`}</Checkbox> : undefined
					}
					onPrimary={async (checkboxChecked = false) => {
						await GuildStickerActionCreators.remove(
							guildId,
							sticker.id,
							Boolean(checkboxChecked) && canExpressionPurge,
						);
						onUpdate();
					}}
				/>
			)),
		);
	};

	const stickerUrl = AvatarUtils.getStickerURL({
		id: sticker.id,
		animated: shouldAnimate,
		size: 320,
	});
	const avatarUrl = sticker.user ? AvatarUtils.getUserAvatarURL(sticker.user, false) : null;

	return (
		<div className={styles.container}>
			<div className={styles.stickerWrapper}>
				<img src={stickerUrl} alt={stickerName} className={styles.stickerImage} loading="lazy" />
			</div>

			<div className={styles.content}>
				<div className={styles.header}>
					<span className={styles.stickerName}>{stickerName}</span>
				</div>

				{sticker.user && avatarUrl && (
					<div className={styles.authorInfo}>
						<img src={avatarUrl} alt="" className={styles.authorAvatar} loading="lazy" />
						<span className={styles.authorName}>{sticker.user.username}</span>
					</div>
				)}
			</div>

			{canModify && (
				<div className={styles.actions}>
					<Tooltip text={t`Edit`}>
						<FocusRing offset={-2}>
							<button type="button" onClick={handleEdit} className={styles.actionButton}>
								<PencilIcon className={styles.icon} weight="bold" />
							</button>
						</FocusRing>
					</Tooltip>

					<Tooltip text={t`Delete`}>
						<FocusRing offset={-2}>
							<button type="button" onClick={handleDelete} className={clsx(styles.actionButton, styles.deleteButton)}>
								<XIcon className={styles.icon} weight="bold" />
							</button>
						</FocusRing>
					</Tooltip>
				</div>
			)}
		</div>
	);
});
