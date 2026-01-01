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
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as GuildEmojiActionCreators from '~/actions/GuildEmojiActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {GuildFeatures} from '~/Constants';
import {Input} from '~/components/form/Input';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {Button} from '~/components/uikit/Button/Button';
import {Checkbox} from '~/components/uikit/Checkbox/Checkbox';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {InlineEdit} from '~/components/uikit/InlineEdit';
import {Popout} from '~/components/uikit/Popout/Popout';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import type {GuildEmojiWithUser} from '~/records/GuildEmojiRecord';
import GuildStore from '~/stores/GuildStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import styles from './EmojiListItem.module.css';

interface EmojiRenamePopoutContentProps {
	initialName: string;
	onSave: (newName: string) => Promise<void>;
	onClose: () => void;
}

const EmojiRenamePopoutContent: React.FC<EmojiRenamePopoutContentProps> = ({initialName, onSave, onClose}) => {
	const {t} = useLingui();
	const [draft, setDraft] = React.useState(initialName);
	const [isSaving, setIsSaving] = React.useState(false);
	const inputRef = React.useRef<HTMLInputElement | null>(null);

	React.useEffect(() => {
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
	onRename: (emojiId: string, newName: string) => void;
	onRemove: (emojiId: string) => void;
}> = observer(({guildId, emoji, layout, onRename, onRemove}) => {
	const {t} = useLingui();
	const avatarUrl = emoji.user ? AvatarUtils.getUserAvatarURL(emoji.user, false) : null;
	const gridNameButtonRef = React.useRef<HTMLButtonElement | null>(null);

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
			console.error('Failed to update emoji name:', err);
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

	const emojiUrl = AvatarUtils.getEmojiURL({id: emoji.id, animated: emoji.animated});

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
					</div>
				</div>

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

			<Tooltip text={t`Delete`}>
				<FocusRing offset={-2}>
					<button type="button" onClick={handleDelete} className={styles.deleteButton}>
						<XIcon className={styles.deleteIcon} weight="bold" />
					</button>
				</FocusRing>
			</Tooltip>
		</div>
	);
});
