// SPDX-License-Identifier: AGPL-3.0-or-later

import i18nGlobal from '@app/app/I18n';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import styles from '@app/features/emoji/components/emojis/EmojiListItem.module.css';
import * as GuildEmojiCommands from '@app/features/expressions/commands/GuildEmojiCommands';
import Guilds from '@app/features/guild/state/Guilds';
import {getEmojiRenderUrl} from '@app/features/messaging/utils/markdown/EmojiDetector';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import {EmojiContextMenuItems} from '@app/features/ui/action_menu/items/EmojiContextMenuItems';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {InlineEdit} from '@app/features/ui/components/InlineEdit';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {GuildEmojiWithUser} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useRef, useState} from 'react';

const EMOJI_NAME_DESCRIPTOR = msg({
	message: 'Emoji name',
	comment: 'Placeholder in the custom emoji rename text field.',
});
const EMOJI_NAME_MUST_BE_AT_LEAST_2_CHARACTERS_DESCRIPTOR = msg({
	message: 'Emoji name must be at least 2 characters long',
	comment: 'Validation error for custom emoji names.',
});
const EMOJI_NAME_MUST_BE_AT_MOST_32_CHARACTERS_DESCRIPTOR = msg({
	message: 'Emoji name must be at most 32 characters long',
	comment: 'Validation error for custom emoji names.',
});
const INVALID_EMOJI_NAME_TITLE_DESCRIPTOR = msg({
	message: 'Invalid emoji name',
	comment: 'Title of the error modal shown when a custom emoji name fails validation.',
});
const RENAME_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't rename this emoji",
	comment: 'Title of the generic fallback error modal shown when renaming a custom emoji fails.',
});
const RENAME_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'The name was reverted to what it was before. Please try again in a moment.',
	comment: 'Body of the generic fallback error modal shown when renaming a custom emoji fails.',
});
const EMOJI_GONE_TITLE_DESCRIPTOR = msg({
	message: 'This emoji no longer exists',
	comment: 'Title of the error modal shown when a custom emoji was deleted before the rename was saved.',
});
const EMOJI_GONE_MESSAGE_DESCRIPTOR = msg({
	message: 'It may have been deleted. The name was reverted to what it was before.',
	comment: 'Body of the error modal shown when a custom emoji was deleted before the rename was saved.',
});
const NO_PERMISSION_TITLE_DESCRIPTOR = msg({
	message: "You can't rename this emoji",
	comment: 'Title of the error modal shown when the user lacks permission to rename a custom emoji.',
});
const NO_PERMISSION_MESSAGE_DESCRIPTOR = msg({
	message: "You don't have permission to rename this emoji. The name was reverted to what it was before.",
	comment: 'Body of the error modal shown when the user lacks permission to rename a custom emoji.',
});
const TOO_FAST_TITLE_DESCRIPTOR = msg({
	message: "You're going too fast",
	comment: 'Title of the error modal shown when renaming a custom emoji is rate limited.',
});
const TOO_FAST_MESSAGE_DESCRIPTOR = msg({
	message: 'Please wait a moment and try renaming again.',
	comment: 'Body of the error modal shown when renaming a custom emoji is rate limited.',
});

function resolveRenameEmojiErrorContent(code: string | undefined): {title: string; message: string} {
	switch (code) {
		case APIErrorCodes.UNKNOWN_EMOJI:
			return {
				title: i18nGlobal._(EMOJI_GONE_TITLE_DESCRIPTOR),
				message: i18nGlobal._(EMOJI_GONE_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.MISSING_PERMISSIONS:
		case APIErrorCodes.MISSING_ACCESS:
			return {
				title: i18nGlobal._(NO_PERMISSION_TITLE_DESCRIPTOR),
				message: i18nGlobal._(NO_PERMISSION_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.RATE_LIMITED:
			return {
				title: i18nGlobal._(TOO_FAST_TITLE_DESCRIPTOR),
				message: i18nGlobal._(TOO_FAST_MESSAGE_DESCRIPTOR),
			};
		default:
			return {
				title: i18nGlobal._(RENAME_FAILED_TITLE_DESCRIPTOR),
				message: i18nGlobal._(RENAME_FAILED_MESSAGE_DESCRIPTOR),
			};
	}
}

function showRenameEmojiErrorModal(error: unknown): void {
	const {title, message} = resolveRenameEmojiErrorContent(failureCode(error));
	ModalCommands.push(
		modal(() => (
			<GenericErrorModal
				title={title}
				message={message}
				data-flx="emoji.emojis.emoji-list-item.rename.generic-error-modal"
			/>
		)),
	);
}

const DELETE_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't delete this emoji",
	comment: 'Title of the generic fallback error modal shown when deleting a custom emoji fails.',
});
const DELETE_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong. Please try again in a moment.',
	comment: 'Body of the generic fallback error modal shown when deleting a custom emoji fails.',
});
const DELETE_GONE_TITLE_DESCRIPTOR = msg({
	message: 'This emoji no longer exists',
	comment: 'Title of the error modal shown when a custom emoji was already deleted.',
});
const DELETE_GONE_MESSAGE_DESCRIPTOR = msg({
	message: 'It looks like it was already deleted.',
	comment: 'Body of the error modal shown when a custom emoji was already deleted.',
});
const DELETE_NO_PERMISSION_TITLE_DESCRIPTOR = msg({
	message: "You can't delete this emoji",
	comment: 'Title of the error modal shown when the user lacks permission to delete a custom emoji.',
});
const DELETE_NO_PERMISSION_MESSAGE_DESCRIPTOR = msg({
	message: "You don't have permission to delete this emoji.",
	comment: 'Body of the error modal shown when the user lacks permission to delete a custom emoji.',
});
const DELETE_TOO_FAST_TITLE_DESCRIPTOR = msg({
	message: "You're going too fast",
	comment: 'Title of the error modal shown when deleting a custom emoji is rate limited.',
});
const DELETE_TOO_FAST_MESSAGE_DESCRIPTOR = msg({
	message: 'Please wait a moment and try deleting again.',
	comment: 'Body of the error modal shown when deleting a custom emoji is rate limited.',
});

function resolveDeleteEmojiErrorContent(code: string | undefined): {title: string; message: string} {
	switch (code) {
		case APIErrorCodes.UNKNOWN_EMOJI:
			return {
				title: i18nGlobal._(DELETE_GONE_TITLE_DESCRIPTOR),
				message: i18nGlobal._(DELETE_GONE_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.MISSING_PERMISSIONS:
		case APIErrorCodes.MISSING_ACCESS:
			return {
				title: i18nGlobal._(DELETE_NO_PERMISSION_TITLE_DESCRIPTOR),
				message: i18nGlobal._(DELETE_NO_PERMISSION_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.RATE_LIMITED:
			return {
				title: i18nGlobal._(DELETE_TOO_FAST_TITLE_DESCRIPTOR),
				message: i18nGlobal._(DELETE_TOO_FAST_MESSAGE_DESCRIPTOR),
			};
		default:
			return {
				title: i18nGlobal._(DELETE_FAILED_TITLE_DESCRIPTOR),
				message: i18nGlobal._(DELETE_FAILED_MESSAGE_DESCRIPTOR),
			};
	}
}

function showDeleteEmojiErrorModal(error: unknown): void {
	const {title, message} = resolveDeleteEmojiErrorContent(failureCode(error));
	ModalCommands.push(
		modal(() => (
			<GenericErrorModal
				title={title}
				message={message}
				data-flx="emoji.emojis.emoji-list-item.delete.generic-error-modal"
			/>
		)),
	);
}
const DELETE_EMOJI_DESCRIPTOR = msg({
	message: 'Delete emoji',
	comment: 'Confirmation modal title for deleting a custom emoji.',
});
const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR = msg({
	message: "Delete :{emojiName}:? Can't be undone.",
	comment: 'Confirmation modal body for deleting a custom emoji. emojiName is shown inside shortcode colons.',
});
const DELETE_DESCRIPTOR = msg({
	message: 'Delete',
	comment: 'Danger button label that confirms deleting a custom emoji.',
});
const PURGE_THIS_EMOJI_FROM_STORAGE_AND_CDN_DESCRIPTOR = msg({
	message: 'Purge this emoji from storage and CDN',
	comment: 'Optional checkbox in the delete emoji modal. CDN means content delivery network.',
});
const RENAME_DESCRIPTOR = msg({
	message: 'Rename :{emojiName}:',
	comment: 'Accessible label for the button that opens the custom emoji rename popout.',
});
const DELETE_2_DESCRIPTOR = msg({
	message: 'Delete',
	comment: 'Tooltip for the custom emoji delete button.',
});
const DELETE_EMOJI_2_DESCRIPTOR = msg({
	message: 'Delete emoji {emojiName}',
	comment: 'Accessible label for deleting a custom emoji from the grid view.',
});
const DELETE_EMOJI_3_DESCRIPTOR = msg({
	message: 'Delete emoji {emojiName}',
	comment: 'Accessible label for deleting a custom emoji from the list view.',
});
const logger = new Logger('EmojiListItem');

interface EmojiRenamePopoutContentProps {
	initialName: string;
	onSave: (newName: string) => Promise<void>;
	onClose: () => void;
}

const EmojiRenamePopoutContent: React.FC<EmojiRenamePopoutContentProps> = ({initialName, onSave, onClose}) => {
	const {i18n} = useLingui();
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
			data-flx="emoji.emojis.emoji-list-item.emoji-rename-popout-content.rename-popout.prevent-default"
		>
			<div
				className={styles.renamePopoutHeader}
				data-flx="emoji.emojis.emoji-list-item.emoji-rename-popout-content.rename-popout-header"
			>
				<span
					className={styles.renamePopoutTitle}
					data-flx="emoji.emojis.emoji-list-item.emoji-rename-popout-content.rename-popout-title"
				>
					<Trans comment="Title for the small popout used to rename a custom emoji.">Rename emoji</Trans>
				</span>
				<span
					className={styles.renamePopoutHint}
					data-flx="emoji.emojis.emoji-list-item.emoji-rename-popout-content.rename-popout-hint"
				>
					<Trans comment="Help text for custom emoji names. Emoji names may use letters, numbers, and underscores only.">
						2-32 characters, letters, numbers, underscores.
					</Trans>
				</span>
			</div>
			<Input
				autoFocus
				ref={inputRef}
				value={draft}
				onChange={handleInputChange}
				maxLength={32}
				placeholder={i18n._(EMOJI_NAME_DESCRIPTOR)}
				data-flx="emoji.emojis.emoji-list-item.emoji-rename-popout-content.input"
			/>
			<div
				className={styles.renamePopoutActions}
				data-flx="emoji.emojis.emoji-list-item.emoji-rename-popout-content.rename-popout-actions"
			>
				<Button
					variant="secondary"
					type="button"
					small
					onClick={() => {
						setDraft(initialName);
						onClose();
					}}
					data-flx="emoji.emojis.emoji-list-item.emoji-rename-popout-content.button.set-draft"
				>
					<Trans comment="Button label that closes the emoji rename popout without saving.">Cancel</Trans>
				</Button>
				<Button
					variant="primary"
					type="submit"
					small
					disabled={!isDraftValid || isSaving}
					submitting={isSaving}
					data-flx="emoji.emojis.emoji-list-item.emoji-rename-popout-content.button.submit"
				>
					<Trans comment="Button label that saves a custom emoji rename.">Save</Trans>
				</Button>
			</div>
		</form>
	);
};
export const EmojiListHeader: React.FC = observer(() => (
	<div className={styles.header} data-flx="emoji.emojis.emoji-list-item.emoji-list-header.header">
		<div className={styles.headerCell} data-flx="emoji.emojis.emoji-list-item.emoji-list-header.header-cell">
			<Trans comment="Column header for the custom emoji image or glyph.">Emoji</Trans>
		</div>
		<div className={styles.headerCell} data-flx="emoji.emojis.emoji-list-item.emoji-list-header.header-cell--2">
			<Trans comment="Column header for custom emoji names.">Name</Trans>
		</div>
		<div className={styles.headerCell} data-flx="emoji.emojis.emoji-list-item.emoji-list-header.header-cell--3">
			<Trans comment="Column header showing who uploaded a custom emoji.">Uploaded by</Trans>
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
	const {i18n} = useLingui();
	const avatarUrl = emoji.user ? AvatarUtils.getUserAvatarURL(emoji.user, false) : null;
	const gridNameButtonRef = useRef<HTMLButtonElement | null>(null);
	const showEmojiNameValidationError = (message: string) => {
		ModalCommands.push(
			modal(() => (
				<GenericErrorModal
					title={i18n._(INVALID_EMOJI_NAME_TITLE_DESCRIPTOR)}
					message={message}
					data-flx="emoji.emojis.emoji-list-item.name-validation-error-modal"
				/>
			)),
		);
	};
	const handleSave = async (newName: string) => {
		const sanitizedName = newName.replace(/[^a-zA-Z0-9_]/g, '');
		if (sanitizedName.length < 2) {
			showEmojiNameValidationError(i18n._(EMOJI_NAME_MUST_BE_AT_LEAST_2_CHARACTERS_DESCRIPTOR));
			throw new Error('Name too short');
		}
		if (sanitizedName.length > 32) {
			showEmojiNameValidationError(i18n._(EMOJI_NAME_MUST_BE_AT_MOST_32_CHARACTERS_DESCRIPTOR));
			throw new Error('Name too long');
		}
		if (sanitizedName === emoji.name) return;
		const prevName = emoji.name;
		onRename(emoji.id, sanitizedName);
		try {
			await GuildEmojiCommands.update(guildId, emoji.id, {name: sanitizedName});
		} catch (err) {
			onRename(emoji.id, prevName);
			logger.error('Failed to update emoji name:', err);
			showRenameEmojiErrorModal(err);
			throw err;
		}
	};
	const guild = Guilds.getGuild(guildId);
	const canExpressionPurge = guild?.features.has(GuildFeatures.EXPRESSION_PURGE_ALLOWED) ?? false;
	const handleDelete = () => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(DELETE_EMOJI_DESCRIPTOR)}
					description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR, {emojiName: emoji.name})}
					primaryText={i18n._(DELETE_DESCRIPTOR)}
					primaryVariant="danger"
					checkboxContent={
						canExpressionPurge ? (
							<Checkbox data-flx="emoji.emojis.emoji-list-item.handle-delete.checkbox">
								{i18n._(PURGE_THIS_EMOJI_FROM_STORAGE_AND_CDN_DESCRIPTOR)}
							</Checkbox>
						) : undefined
					}
					onPrimary={async (checkboxChecked = false) => {
						try {
							await GuildEmojiCommands.remove(guildId, emoji.id, checkboxChecked && canExpressionPurge);
						} catch (error) {
							logger.error('Failed to delete emoji:', error);
							showDeleteEmojiErrorModal(error);
							throw error;
						}
						onRemove(emoji.id);
					}}
					data-flx="emoji.emojis.emoji-list-item.handle-delete.confirm-modal"
				/>
			)),
		);
	};
	const shouldAnimate = useShouldAnimate({kind: 'emoji', isAnimated: emoji.animated});
	const emojiUrl =
		getEmojiRenderUrl({
			id: emoji.id,
			surrogateUrl: null,
			isAnimatable: emoji.animated,
			animated: shouldAnimate,
			jumbo: false,
		}) ?? '';
	const emojiForMenu = {
		id: emoji.id,
		guildId,
		name: emoji.name,
		uniqueName: emoji.name,
		allNamesString: `:${emoji.name}:`,
		url: emojiUrl,
		animated: emoji.animated,
		nsfw: emoji.nsfw,
		user: emoji.user,
	};
	const handleContextMenu = (event: React.MouseEvent) => {
		event.preventDefault();
		event.stopPropagation();
		ContextMenuCommands.openFromEvent(event, ({onClose}) => (
			<EmojiContextMenuItems
				emoji={emojiForMenu}
				onClose={onClose}
				data-flx="emoji.emojis.emoji-list-item.handle-context-menu.emoji-context-menu-items"
			/>
		));
	};
	if (layout === 'grid') {
		return (
			// biome-ignore lint/a11y/noStaticElementInteractions: context-menu affordance on emoji card.
			<div
				className={clsx(styles.cardWrapper, styles.gridCardWrapper)}
				onContextMenu={handleContextMenu}
				data-flx="emoji.emojis.emoji-list-item.card-wrapper.context-menu"
			>
				<div className={clsx(styles.card, styles.gridCard)} data-flx="emoji.emojis.emoji-list-item.card">
					<div className={styles.gridEmojiWrapper} data-flx="emoji.emojis.emoji-list-item.grid-emoji-wrapper">
						<img
							src={emojiUrl}
							alt={emoji.name}
							className={styles.gridEmojiImage}
							data-flx="emoji.emojis.emoji-list-item.grid-emoji-image"
						/>
						{emoji.user && avatarUrl && (
							<Tooltip text={NicknameUtils.getDisplayName(emoji.user)} data-flx="emoji.emojis.emoji-list-item.tooltip">
								<img
									src={avatarUrl}
									alt=""
									className={styles.gridAvatar}
									data-flx="emoji.emojis.emoji-list-item.grid-avatar"
								/>
							</Tooltip>
						)}
					</div>
					<div className={styles.gridName} data-flx="emoji.emojis.emoji-list-item.grid-name">
						{canModify ? (
							<Popout
								position="bottom"
								offsetMainAxis={8}
								offsetCrossAxis={0}
								returnFocusRef={gridNameButtonRef}
								render={({onClose}) => (
									<EmojiRenamePopoutContent
										initialName={emoji.name}
										onSave={handleSave}
										onClose={onClose}
										data-flx="emoji.emojis.emoji-list-item.emoji-rename-popout-content"
									/>
								)}
								data-flx="emoji.emojis.emoji-list-item.popout"
							>
								<button
									type="button"
									ref={gridNameButtonRef}
									className={styles.gridNameButton}
									aria-label={i18n._(RENAME_DESCRIPTOR, {emojiName: emoji.name})}
									data-flx="emoji.emojis.emoji-list-item.grid-name-button"
								>
									<span className={styles.gridNameText} data-flx="emoji.emojis.emoji-list-item.grid-name-text">
										:{emoji.name}:
									</span>
								</button>
							</Popout>
						) : (
							<span className={styles.gridNameText} data-flx="emoji.emojis.emoji-list-item.grid-name-text--2">
								:{emoji.name}:
							</span>
						)}
					</div>
				</div>
				{canModify && (
					<Tooltip text={i18n._(DELETE_2_DESCRIPTOR)} data-flx="emoji.emojis.emoji-list-item.tooltip--2">
						<FocusRing offset={-2} data-flx="emoji.emojis.emoji-list-item.focus-ring">
							<button
								type="button"
								onClick={handleDelete}
								className={clsx(styles.deleteButton, styles.deleteButtonFloating)}
								aria-label={i18n._(DELETE_EMOJI_2_DESCRIPTOR, {emojiName: emoji.name})}
								data-flx="emoji.emojis.emoji-list-item.delete-button"
							>
								<XIcon
									className={styles.deleteIcon}
									weight="bold"
									data-flx="emoji.emojis.emoji-list-item.delete-icon"
								/>
							</button>
						</FocusRing>
					</Tooltip>
				)}
			</div>
		);
	}
	return (
		// biome-ignore lint/a11y/noStaticElementInteractions: context-menu affordance on emoji card.
		<div
			className={clsx(styles.cardWrapper, styles.listCardWrapper)}
			onContextMenu={handleContextMenu}
			data-flx="emoji.emojis.emoji-list-item.card-wrapper.context-menu--2"
		>
			<div className={clsx(styles.card, styles.listCard)} data-flx="emoji.emojis.emoji-list-item.card--2">
				<div className={styles.listEmoji} data-flx="emoji.emojis.emoji-list-item.list-emoji">
					<img
						src={emojiUrl}
						alt={emoji.name}
						className={styles.listEmojiImage}
						data-flx="emoji.emojis.emoji-list-item.list-emoji-image"
					/>
				</div>
				<div className={styles.listName} data-flx="emoji.emojis.emoji-list-item.list-name">
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
							data-flx="emoji.emojis.emoji-list-item.name-inline-edit"
						/>
					) : (
						<span className={styles.nameInlineEdit} data-flx="emoji.emojis.emoji-list-item.name-inline-edit--2">
							:{emoji.name}:
						</span>
					)}
				</div>
				<div className={styles.listUploader} data-flx="emoji.emojis.emoji-list-item.list-uploader">
					{emoji.user && avatarUrl ? (
						<>
							<img src={avatarUrl} alt="" className={styles.avatar} data-flx="emoji.emojis.emoji-list-item.avatar" />
							<span className={styles.username} data-flx="emoji.emojis.emoji-list-item.username">
								{NicknameUtils.getDisplayName(emoji.user)}
							</span>
						</>
					) : (
						<span className={styles.unknownUser} data-flx="emoji.emojis.emoji-list-item.unknown-user">
							<Trans comment="Fallback uploader label when the user who uploaded an emoji is unavailable.">
								Unknown
							</Trans>
						</span>
					)}
				</div>
			</div>
			{canModify && (
				<Tooltip text={i18n._(DELETE_2_DESCRIPTOR)} data-flx="emoji.emojis.emoji-list-item.tooltip--3">
					<FocusRing offset={-2} data-flx="emoji.emojis.emoji-list-item.focus-ring--2">
						<button
							type="button"
							onClick={handleDelete}
							className={styles.deleteButton}
							aria-label={i18n._(DELETE_EMOJI_3_DESCRIPTOR, {emojiName: emoji.name})}
							data-flx="emoji.emojis.emoji-list-item.delete-button--2"
						>
							<XIcon
								className={styles.deleteIcon}
								weight="bold"
								data-flx="emoji.emojis.emoji-list-item.delete-icon--2"
							/>
						</button>
					</FocusRing>
				</Tooltip>
			)}
		</div>
	);
});
