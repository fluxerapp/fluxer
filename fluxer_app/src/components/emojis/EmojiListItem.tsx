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

import * as GuildEmojiActionCreators from '@app/actions/GuildEmojiActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import styles from '@app/components/emojis/EmojiListItem.module.css';
import {Input} from '@app/components/form/Input';
import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import {Button} from '@app/components/uikit/button/Button';
import {Checkbox} from '@app/components/uikit/checkbox/Checkbox';
import FocusRing from '@app/components/uikit/focus_ring/FocusRing';
import {InlineEdit} from '@app/components/uikit/InlineEdit';
import {Popout} from '@app/components/uikit/popout/Popout';
import {Tooltip} from '@app/components/uikit/tooltip/Tooltip';
import {useStickerAnimation} from '@app/hooks/useStickerAnimation';
import {Logger} from '@app/lib/Logger';
import GuildStore from '@app/stores/GuildStore';
import * as AvatarUtils from '@app/utils/AvatarUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {GuildEmojiWithUser} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import {Trans, useLingui} from '@lingui/react/macro';
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useRef, useState} from 'react';

const logger = new Logger('EmojiListItem');

interface EmojiRenamePopoutContentProps {
	initialName: string;
	onSave: (newName: string) => Promise<void>;
	onClose: () => void;
}

const EmojiRenamePopoutContent: React.FC<EmojiRenamePopoutContentProps> = ({initialName, onSave, onClose}) => {
	const {t} = useLingui();
	const [draft, setDraft] = useState(initialName);
	const [isSaving, setIsSaving] = useState(false);
	const inputRef = useRef<HTMLInputElement | null>(null);

	useEffect(() => {
		requestAnimationFrame(() => inputRef.current?.focus());
	}, []);

	const sanitizedDraft = draft.replace(/[^a-zA-Z0-9_]/g, '');
	const isDraftValid = sanitizedDraft.length >= 2 && sanitizedDraft.length <= 32;

	const handleSubmit = async () => {
		if (!isDraftValid || isSaving) return;
		setIsSaving(true);
		try {
			await onSave(sanitizedDraft);
			onClose();
		} finally {
			setIsSaving(false);
		}
	};

	const handleInputChange: React.ChangeEventHandler<HTMLInputElement> = (e) => {
		const {value, selectionStart, selectionEnd} = e.target;
		const next = value.replace(/[^a-zA-Z0-9_]/g, '');
		const removed = value.length - next.length;
		setDraft(next);

		if (inputRef.current && selectionStart !== null && selectionEnd !== null) {
			const newStart = Math.max(0, selectionStart - removed);
			const newEnd = Math.max(0, selectionEnd - removed);
			requestAnimationFrame(() => inputRef.current?.setSelectionRange(newStart, newEnd));
		}
	};

	return (
		<form
			className={styles.renamePopout}
			onSubmit={(e) => {
				e.preventDefault();
				void handleSubmit();
			}}
		>
			<div className={styles.renamePopoutHeader}>
				<span className={styles.renamePopoutTitle}>
					<Trans>Rename Emoji</Trans>
				</span>
				<span className={styles.renamePopoutHint}>
					<Trans>2-32 characters, letters, numbers, underscores.</Trans>
				</span>
			</div>
			<Input
				autoFocus
				ref={inputRef}
				value={draft}
				onChange={handleInputChange}
				maxLength={32}
				placeholder={t`Emoji name`}
			/>
			<div className={styles.renamePopoutActions}>
				<Button
					variant="secondary"
					type="button"
					small
					onClick={() => {
						setDraft(initialName);
						onClose();
					}}
				>
					<Trans>Cancel</Trans>
				</Button>
				<Button variant="primary" type="submit" small disabled={!isDraftValid || isSaving} submitting={isSaving}>
					<Trans>Save</Trans>
				</Button>
			</div>
		</form>
	);
};

export const EmojiListHeader: React.FC = observer(() => (
	<div className={styles.header}>
		<div className={styles.headerCell}>
			<Trans>Emoji</Trans>
		</div>
		<div className={styles.headerCell}>
			<Trans>Name</Trans>
		</div>
		<div className={styles.headerCell}>
			<Trans>Uploaded By</Trans>
		</div>
	</div>
));

export const EmojiListItem: React.FC<{
	guildId: string;
	emoji: GuildEmojiWithUser;
	layout: 'list' | 'grid';
	canModify: boolean;
	onRename: (emojiId: string, newName: string) => void;
	onRemove: (emojiId: string) => void;
}> = observer(({guildId, emoji, layout, canModify, onRename, onRemove}) => {
	const {t} = useLingui();
	const avatarUrl = emoji.user ? AvatarUtils.getUserAvatarURL(emoji.user, false) : null;
	const gridNameButtonRef = useRef<HTMLButtonElement | null>(null);

	const handleSave = async (newName: string) => {
		const sanitizedName = newName.replace(/[^a-zA-Z0-9_]/g, '');
		if (sanitizedName.length < 2) {
			ToastActionCreators.error(t`Emoji name must be at least 2 characters long`);
			throw new Error('Name too short');
		}
		if (sanitizedName.length > 32) {
			ToastActionCreators.error(t`Emoji name must be at most 32 characters long`);
			throw new Error('Name too long');
		}
		if (sanitizedName === emoji.name) return;

		const prevName = emoji.name;
		onRename(emoji.id, sanitizedName);

		try {
			await GuildEmojiActionCreators.update(guildId, emoji.id, {name: sanitizedName});
		} catch (err) {
			onRename(emoji.id, prevName);
			logger.error('Failed to update emoji name:', err);
			ToastActionCreators.error(t`Failed to update emoji name. Reverted to the previous name.`);
			throw err;
		}
	};

	const guild = GuildStore.getGuild(guildId);
	const canExpressionPurge = guild?.features.has(GuildFeatures.EXPRESSION_PURGE_ALLOWED) ?? false;

	const handleDelete = () => {
		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Delete Emoji`}
					description={t`Are you sure you want to delete :${emoji.name}:? This action cannot be undone.`}
					primaryText={t`Delete`}
					primaryVariant="danger-primary"
					checkboxContent={
						canExpressionPurge ? <Checkbox>{t`Purge this emoji from storage and CDN`}</Checkbox> : undefined
					}
					onPrimary={async (checkboxChecked = false) => {
						await GuildEmojiActionCreators.remove(guildId, emoji.id, checkboxChecked && canExpressionPurge);
						onRemove(emoji.id);
					}}
				/>
			)),
		);
	};

	const {shouldAnimate} = useStickerAnimation();
	const emojiUrl = AvatarUtils.getEmojiURL({id: emoji.id, animated: shouldAnimate});

	if (layout === 'grid') {
		return (
			<div className={clsx(styles.cardWrapper, styles.gridCardWrapper)}>
				<div className={clsx(styles.card, styles.gridCard)}>
					<div className={styles.gridEmojiWrapper}>
						<img src={emojiUrl} alt={emoji.name} className={styles.gridEmojiImage} loading="lazy" />
						{emoji.user && avatarUrl && (
							<Tooltip text={emoji.user.username}>
								<img src={avatarUrl} alt="" className={styles.gridAvatar} loading="lazy" />
							</Tooltip>
						)}
					</div>

					<div className={styles.gridName}>
						{canModify ? (
							<Popout
								position="bottom"
								offsetMainAxis={8}
								offsetCrossAxis={0}
								returnFocusRef={gridNameButtonRef}
								render={({onClose}) => (
									<EmojiRenamePopoutContent initialName={emoji.name} onSave={handleSave} onClose={onClose} />
								)}
							>
								<button
									type="button"
									ref={gridNameButtonRef}
									className={styles.gridNameButton}
									aria-label={t`Rename :${emoji.name}:`}
								>
									<span className={styles.gridNameText}>:{emoji.name}:</span>
								</button>
							</Popout>
						) : (
							<span className={styles.gridNameText}>:{emoji.name}:</span>
						)}
					</div>
				</div>

				{canModify && (
					<Tooltip text={t`Delete`}>
						<FocusRing offset={-2}>
							<button
								type="button"
								onClick={handleDelete}
								className={clsx(styles.deleteButton, styles.deleteButtonFloating)}
							>
								<XIcon className={styles.deleteIcon} weight="bold" />
							</button>
						</FocusRing>
					</Tooltip>
				)}
			</div>
		);
	}

	return (
		<div className={clsx(styles.cardWrapper, styles.listCardWrapper)}>
			<div className={clsx(styles.card, styles.listCard)}>
				<div className={styles.listEmoji}>
					<img src={emojiUrl} alt={emoji.name} className={styles.listEmojiImage} loading="lazy" />
				</div>

				<div className={styles.listName}>
					{canModify ? (
						<InlineEdit
							value={emoji.name}
							onSave={handleSave}
							prefix=":"
							suffix=":"
							maxLength={32}
							width="100%"
							className={styles.nameInlineEdit}
							inputClassName={styles.nameInlineEditInput}
							buttonClassName={styles.nameInlineEditButton}
						/>
					) : (
						<span className={styles.nameInlineEdit}>:{emoji.name}:</span>
					)}
				</div>

				<div className={styles.listUploader}>
					{emoji.user && avatarUrl ? (
						<>
							<img src={avatarUrl} alt="" className={styles.avatar} loading="lazy" />
							<span className={styles.username}>{emoji.user.username}</span>
						</>
					) : (
						<span className={styles.unknownUser}>
							<Trans>Unknown</Trans>
						</span>
					)}
				</div>
			</div>

			{canModify && (
				<Tooltip text={t`Delete`}>
					<FocusRing offset={-2}>
						<button type="button" onClick={handleDelete} className={styles.deleteButton}>
							<XIcon className={styles.deleteIcon} weight="bold" />
						</button>
					</FocusRing>
				</Tooltip>
			)}
		</div>
	);
});
