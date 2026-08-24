// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import styles from '@app/features/emoji/components/stickers/StickerGridItem.module.css';
import {useStickerAnimation} from '@app/features/emoji/hooks/useStickerAnimation';
import * as GuildStickerCommands from '@app/features/expressions/commands/GuildStickerCommands';
import {EditGuildStickerModal} from '@app/features/expressions/components/modals/EditGuildStickerModal';
import Guilds from '@app/features/guild/state/Guilds';
import {useNearViewport} from '@app/features/messaging/hooks/useNearViewport';
import {StickerContextMenuItems} from '@app/features/ui/action_menu/items/StickerContextMenuItems';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {GuildStickerWithUser} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PencilIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const DELETE_STICKER_DESCRIPTOR = msg({
	message: 'Delete sticker',
	comment: 'Destructive action that deletes the selected sticker.',
});
const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR = msg({
	message: 'Delete "{stickerName}"? Can\'t be undone.',
	comment: 'Confirm dialog body before deleting a sticker.',
});
const DELETE_DESCRIPTOR = msg({
	message: 'Delete',
	comment: 'Destructive action label. Surface varies, see surrounding code for the specific object being deleted.',
});
const PURGE_THIS_STICKER_FROM_STORAGE_AND_CDN_DESCRIPTOR = msg({
	message: 'Purge this sticker from storage and CDN',
	comment: 'Staff-only checkbox in the delete-sticker confirmation.',
});
const EDIT_DESCRIPTOR = msg({
	message: 'Edit',
	comment: 'Action label for opening the edit flow for the selected item.',
});
const EDIT_STICKER_DESCRIPTOR = msg({
	message: 'Edit sticker {stickerName}',
	comment: 'Accessible label for the edit-sticker button on a sticker grid item.',
});
const DELETE_STICKER_2_DESCRIPTOR = msg({
	message: 'Delete sticker {stickerName}',
	comment: 'Accessible label for the delete-sticker button on a sticker grid item.',
});

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
	const {i18n} = useLingui();
	const {shouldAnimate, interactionHandlers} = useStickerAnimation({isAnimated: sticker.animated});
	const {ref: tileRef, isNearViewport} = useNearViewport<HTMLDivElement>({
		rememberKey: `guild-sticker-tile:${sticker.id}`,
	});
	const stickerName = sticker.name;
	const guild = Guilds.getGuild(guildId);
	const canExpressionPurge = guild?.features.has(GuildFeatures.EXPRESSION_PURGE_ALLOWED) ?? false;
	const handleEdit = () => {
		ModalCommands.push(
			ModalCommands.modal(() => (
				<EditGuildStickerModal
					guildId={guildId}
					sticker={sticker}
					onUpdate={onUpdate}
					data-flx="emoji.stickers.sticker-grid-item.handle-edit.edit-guild-sticker-modal"
				/>
			)),
		);
	};
	const handleDelete = () => {
		ModalCommands.push(
			ModalCommands.modal(() => (
				<ConfirmModal
					title={i18n._(DELETE_STICKER_DESCRIPTOR)}
					description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR, {stickerName})}
					primaryText={i18n._(DELETE_DESCRIPTOR)}
					primaryVariant="danger"
					checkboxContent={
						canExpressionPurge ? (
							<Checkbox data-flx="emoji.stickers.sticker-grid-item.handle-delete.checkbox">
								{i18n._(PURGE_THIS_STICKER_FROM_STORAGE_AND_CDN_DESCRIPTOR)}
							</Checkbox>
						) : undefined
					}
					onPrimary={async (checkboxChecked = false) => {
						await GuildStickerCommands.remove(guildId, sticker.id, Boolean(checkboxChecked) && canExpressionPurge);
						onUpdate();
					}}
					data-flx="emoji.stickers.sticker-grid-item.handle-delete.confirm-modal"
				/>
			)),
		);
	};
	const stickerUrl = AvatarUtils.getStickerURL({
		id: sticker.id,
		animated: shouldAnimate,
		isAnimatable: sticker.animated,
		size: 320,
	});
	const avatarUrl = sticker.user ? AvatarUtils.getUserAvatarURL(sticker.user, false) : null;
	const stickerForMenu = {
		id: sticker.id,
		guildId,
		name: sticker.name,
		description: sticker.description,
		tags: sticker.tags,
		url: stickerUrl,
		animated: sticker.animated,
		nsfw: sticker.nsfw,
		user: sticker.user,
	};
	const handleContextMenu = (event: React.MouseEvent) => {
		event.preventDefault();
		event.stopPropagation();
		ContextMenuCommands.openFromEvent(event, ({onClose}) => (
			<StickerContextMenuItems
				sticker={stickerForMenu}
				onClose={onClose}
				data-flx="emoji.stickers.sticker-grid-item.handle-context-menu.sticker-context-menu-items"
			/>
		));
	};
	return (
		<div
			ref={tileRef}
			role="group"
			className={styles.container}
			onContextMenu={handleContextMenu}
			onMouseEnter={interactionHandlers.onMouseEnter}
			onMouseLeave={interactionHandlers.onMouseLeave}
			onFocus={interactionHandlers.onFocus}
			onBlur={interactionHandlers.onBlur}
			data-flx="emoji.stickers.sticker-grid-item.container.context-menu"
		>
			<div className={styles.stickerWrapper} data-flx="emoji.stickers.sticker-grid-item.sticker-wrapper">
				{isNearViewport && (
					<img
						src={stickerUrl}
						alt={stickerName}
						className={styles.stickerImage}
						data-flx="emoji.stickers.sticker-grid-item.sticker-image"
					/>
				)}
			</div>
			<div className={styles.content} data-flx="emoji.stickers.sticker-grid-item.content">
				<div className={styles.header} data-flx="emoji.stickers.sticker-grid-item.header">
					<span className={styles.stickerName} data-flx="emoji.stickers.sticker-grid-item.sticker-name">
						{stickerName}
					</span>
				</div>
				{sticker.user && avatarUrl && (
					<div className={styles.authorInfo} data-flx="emoji.stickers.sticker-grid-item.author-info">
						{isNearViewport ? (
							<img
								src={avatarUrl}
								alt=""
								className={styles.authorAvatar}
								data-flx="emoji.stickers.sticker-grid-item.author-avatar"
							/>
						) : (
							<div
								className={styles.authorAvatar}
								data-flx="emoji.stickers.sticker-grid-item.author-avatar-placeholder"
							/>
						)}
						<span className={styles.authorName} data-flx="emoji.stickers.sticker-grid-item.author-name">
							{NicknameUtils.getDisplayName(sticker.user)}
						</span>
					</div>
				)}
			</div>
			{canModify && (
				<div className={styles.actions} data-flx="emoji.stickers.sticker-grid-item.actions">
					<Tooltip text={i18n._(EDIT_DESCRIPTOR)} data-flx="emoji.stickers.sticker-grid-item.tooltip">
						<FocusRing offset={-2} data-flx="emoji.stickers.sticker-grid-item.focus-ring">
							<button
								type="button"
								onClick={handleEdit}
								className={styles.actionButton}
								aria-label={i18n._(EDIT_STICKER_DESCRIPTOR, {stickerName})}
								data-flx="emoji.stickers.sticker-grid-item.action-button.edit"
							>
								<PencilIcon className={styles.icon} weight="bold" data-flx="emoji.stickers.sticker-grid-item.icon" />
							</button>
						</FocusRing>
					</Tooltip>
					<Tooltip text={i18n._(DELETE_DESCRIPTOR)} data-flx="emoji.stickers.sticker-grid-item.tooltip--2">
						<FocusRing offset={-2} data-flx="emoji.stickers.sticker-grid-item.focus-ring--2">
							<button
								type="button"
								onClick={handleDelete}
								className={clsx(styles.actionButton, styles.deleteButton)}
								aria-label={i18n._(DELETE_STICKER_2_DESCRIPTOR, {stickerName})}
								data-flx="emoji.stickers.sticker-grid-item.action-button.delete"
							>
								<XIcon className={styles.icon} weight="bold" data-flx="emoji.stickers.sticker-grid-item.icon--2" />
							</button>
						</FocusRing>
					</Tooltip>
				</div>
			)}
		</div>
	);
});
