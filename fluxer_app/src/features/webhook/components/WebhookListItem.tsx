// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {IMAGE_MAX_SIZE_BYTES} from '@app/features/app/config/I18nDisplayConstants';
import {getAcceptString} from '@app/features/expressions/utils/AssetFormatCopy';
import {isSvgFile, readImageFileAsUploadDataUrl} from '@app/features/expressions/utils/ImageUploadFileUtils';
import {
	INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR,
	SOMETHING_WENT_WRONG_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {Input} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import styles from '@app/features/webhook/components/WebhookListItem.module.css';
import type {Webhook} from '@app/features/webhook/models/Webhook';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CaretDownIcon, CopySimpleIcon, TrashSimpleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const UNKNOWN_USER_DESCRIPTOR = msg({
	message: 'Unknown user',
	comment: 'Fallback label when the user who created a webhook is unavailable.',
});
const FAILED_TO_DELETE_WEBHOOK_DESCRIPTOR = msg({
	message: "Couldn't delete this webhook",
	comment: 'Toast shown when deleting a webhook fails.',
});
const AVATAR_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR = msg({
	message: 'Avatar is too large — pick one under {imageMaxSizeLabel}.',
	comment: 'Webhook avatar upload error. IMAGE_MAX_SIZE_LABEL is a formatted file-size limit.',
});
const WEBHOOK_DESCRIPTOR = msg({
	message: 'Webhook {currentName}',
	comment:
		'Short label in the webhooks webhook list item. Keep it concise. Preserve {currentName}; it is inserted by code.',
});
const NAME_DESCRIPTOR = msg({
	message: 'Name',
	comment: 'Form label for the editable webhook display name.',
});
const WEBHOOK_NAME_DESCRIPTOR = msg({
	message: 'Webhook name',
	comment: 'Placeholder in the webhook name text field.',
});
const CHANNEL_DESCRIPTOR = msg({
	message: 'Channel',
	comment: 'Select label for the channel a webhook posts into.',
});
const WEBHOOK_URL_DESCRIPTOR = msg({
	message: 'Webhook URL',
	comment: 'Read-only field label for the generated webhook URL.',
});

interface ChannelOption {
	id: string;
	label: string;
}

interface WebhookListItemProps {
	webhook: Webhook;
	channelName?: string | null;
	onDelete: (webhook: Webhook) => Promise<void>;
	onUpdate: (webhookId: string, updates: {name?: string; avatar?: string | null; channelId?: string}) => void;
	availableChannels?: Array<ChannelOption>;
	defaultExpanded?: boolean;
	isExpanded?: boolean;
	onExpandedChange?: (expanded: boolean) => void;
	formVersion?: number;
}

const logger = new Logger('WebhookListItem');
export const WebhookListItem: React.FC<WebhookListItemProps> = observer(
	({
		webhook,
		channelName,
		onDelete,
		onUpdate,
		availableChannels,
		defaultExpanded = false,
		isExpanded,
		onExpandedChange,
		formVersion,
	}) => {
		const {i18n} = useLingui();
		const [localExpanded, setLocalExpanded] = useState(defaultExpanded);
		const expanded = isExpanded ?? localExpanded;
		const setExpanded = useCallback(
			(next: boolean) => {
				if (onExpandedChange) onExpandedChange(next);
				else setLocalExpanded(next);
			},
			[onExpandedChange],
		);
		const [isDeleting, setIsDeleting] = useState(false);
		const [isUpdatingAvatar, setIsUpdatingAvatar] = useState(false);
		const [selectedChannelId, setSelectedChannelId] = useState(webhook.channelId);
		const [currentName, setCurrentName] = useState(webhook.name);
		const [localAvatar, setLocalAvatar] = useState<string | null | undefined>(undefined);
		useEffect(() => {
			setLocalAvatar(undefined);
		}, []);
		useEffect(() => {
			if (formVersion == null) return;
			setLocalAvatar(undefined);
			setCurrentName(webhook.name);
			setSelectedChannelId(webhook.channelId);
		}, [formVersion, webhook.name, webhook.channelId]);
		const creator = webhook.creator;
		const creatorDisplayName = creator?.displayName ?? i18n._(UNKNOWN_USER_DESCRIPTOR);
		const createdAt = useMemo(() => DateUtils.getFormattedShortDate(webhook.createdAt), [webhook.createdAt]);
		const effectiveAvatar: string | null = localAvatar !== undefined ? localAvatar : (webhook.avatar ?? null);
		const avatarUrl = useMemo(() => {
			return AvatarUtils.getWebhookAvatarURL({id: webhook.id, avatar: effectiveAvatar}, false);
		}, [webhook.id, effectiveAvatar]);
		const webhookUrl = useMemo(() => webhook.webhookUrl, [webhook.webhookUrl]);
		const handleCopy = useCallback(async () => {
			try {
				await TextCopyCommands.copy(i18n, webhookUrl);
			} catch (error) {
				logger.error('Failed to copy webhook URL', error);
			}
		}, [i18n, webhookUrl]);
		const handleDelete = useCallback(async () => {
			if (!onDelete) return;
			setIsDeleting(true);
			try {
				await onDelete(webhook);
			} catch (error) {
				logger.error('Failed to delete webhook', error);
				showGenericErrorModal({
					title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
					message: () => i18n._(FAILED_TO_DELETE_WEBHOOK_DESCRIPTOR),
					dataFlx: 'webhook.webhook-list-item.delete-error-modal',
				});
			} finally {
				setIsDeleting(false);
			}
		}, [onDelete, webhook, i18n]);
		const handleChannelChange = useCallback(
			(newChannelId: string) => {
				if (newChannelId === webhook.channelId) return;
				setSelectedChannelId(newChannelId);
				onUpdate?.(webhook.id, {channelId: newChannelId});
			},
			[onUpdate, webhook.id, webhook.channelId],
		);
		const handleAvatarUpload = useCallback(async () => {
			const [file] = await openFilePicker({accept: getAcceptString('avatar')});
			if (!file) return;
			if (file.size > 10 * 1024 * 1024) {
				showGenericErrorModal({
					title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
					message: () =>
						i18n._(AVATAR_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR, {
							imageMaxSizeLabel: formatFileSize(i18n.locale, IMAGE_MAX_SIZE_BYTES),
						}),
					dataFlx: 'webhook.webhook-list-item.avatar-too-large-error-modal',
				});
				return;
			}
			try {
				setIsUpdatingAvatar(true);
				const base64 = isSvgFile(file)
					? await readImageFileAsUploadDataUrl(file)
					: await AvatarUtils.fileToBase64(file);
				setLocalAvatar(base64);
				onUpdate?.(webhook.id, {avatar: base64});
			} catch {
				showGenericErrorModal({
					title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
					message: () => i18n._(INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR),
					dataFlx: 'webhook.webhook-list-item.invalid-avatar-error-modal',
				});
			} finally {
				setIsUpdatingAvatar(false);
			}
		}, [i18n, webhook.id, onUpdate]);
		const handleClearAvatar = useCallback(() => {
			setLocalAvatar(null);
			onUpdate?.(webhook.id, {avatar: null});
		}, [webhook.id, onUpdate]);
		return (
			<div className={styles.container} data-flx="webhook.webhook-list-item.container">
				<FocusRing offset={-2} data-flx="webhook.webhook-list-item.focus-ring">
					<button
						type="button"
						className={styles.headerButton}
						onClick={() => setExpanded(!expanded)}
						aria-expanded={expanded}
						aria-label={i18n._(WEBHOOK_DESCRIPTOR, {currentName})}
						data-flx="webhook.webhook-list-item.header-button.set-expanded"
					>
						<div className={styles.left} data-flx="webhook.webhook-list-item.left">
							<div
								className={styles.avatarLarge}
								style={{backgroundImage: `url(${avatarUrl})`}}
								aria-hidden
								data-flx="webhook.webhook-list-item.avatar-large"
							/>
							<div className={styles.textBlock} data-flx="webhook.webhook-list-item.text-block">
								<div className={styles.titleRow} data-flx="webhook.webhook-list-item.title-row">
									<span className={styles.name} data-flx="webhook.webhook-list-item.name">
										{currentName}
									</span>
									{channelName && (
										<span className={styles.channelTag} data-flx="webhook.webhook-list-item.channel-tag">
											#{channelName}
										</span>
									)}
								</div>
								<div className={styles.metaRow} data-flx="webhook.webhook-list-item.meta-row">
									<span className={styles.truncate} data-flx="webhook.webhook-list-item.truncate">
										<Trans comment="Webhook metadata line. creatorDisplayName is the webhook creator, createdAt is a short localized date.">
											Created by {creatorDisplayName} on {createdAt}
										</Trans>
									</span>
									{channelName && (
										<span className={styles.channelTagMobile} data-flx="webhook.webhook-list-item.channel-tag-mobile">
											#{channelName}
										</span>
									)}
								</div>
							</div>
						</div>
						<CaretDownIcon
							className={clsx(styles.chevron, expanded && styles.chevronExpanded)}
							weight="bold"
							data-flx="webhook.webhook-list-item.chevron"
						/>
					</button>
				</FocusRing>
				{expanded && (
					<div className={styles.details} data-flx="webhook.webhook-list-item.details">
						<div className={styles.detailsRow} data-flx="webhook.webhook-list-item.details-row">
							<div className={styles.avatarColumn} data-flx="webhook.webhook-list-item.avatar-column">
								<label
									htmlFor={`webhook-avatar-${webhook.id}`}
									className={styles.label}
									data-flx="webhook.webhook-list-item.label"
								>
									<Trans comment="Form label for a webhook avatar image.">Avatar</Trans>
								</label>
								<div
									className={styles.avatarPreview}
									style={{backgroundImage: `url(${avatarUrl})`}}
									aria-hidden
									data-flx="webhook.webhook-list-item.avatar-preview"
								/>
								<div className={styles.avatarActions} data-flx="webhook.webhook-list-item.avatar-actions">
									<Button
										variant="secondary"
										small={true}
										onClick={handleAvatarUpload}
										submitting={isUpdatingAvatar}
										data-flx="webhook.webhook-list-item.button.avatar-upload"
									>
										<Trans comment="Button label for uploading a webhook avatar image.">Upload image</Trans>
									</Button>
									{effectiveAvatar !== null && (
										<Button
											variant="secondary"
											small
											onClick={handleClearAvatar}
											data-flx="webhook.webhook-list-item.button.clear-avatar"
										>
											<Trans comment="Button label for removing the current webhook avatar image.">Remove</Trans>
										</Button>
									)}
								</div>
							</div>
							<div className={styles.fields} data-flx="webhook.webhook-list-item.fields">
								<div className={styles.fieldsRow} data-flx="webhook.webhook-list-item.fields-row">
									<div className={styles.fieldGrow} data-flx="webhook.webhook-list-item.field-grow">
										<Input
											id={`webhook-name-${webhook.id}`}
											label={i18n._(NAME_DESCRIPTOR)}
											value={currentName}
											onChange={(event) => {
												const newName = event.target.value;
												setCurrentName(newName);
												onUpdate?.(webhook.id, {name: newName});
											}}
											onBlur={() => {
												if (currentName !== webhook.name) {
													onUpdate?.(webhook.id, {name: currentName});
												}
											}}
											placeholder={i18n._(WEBHOOK_NAME_DESCRIPTOR)}
											data-flx="webhook.webhook-list-item.input.set-current-name"
										/>
									</div>
									{availableChannels && availableChannels.length > 0 && (
										<div className={styles.fieldGrow} data-flx="webhook.webhook-list-item.field-grow--2">
											<Combobox
												label={i18n._(CHANNEL_DESCRIPTOR)}
												value={selectedChannelId}
												options={availableChannels.map((option) => ({
													value: option.id,
													label: option.label,
												}))}
												onChange={handleChannelChange}
												data-flx="webhook.webhook-list-item.select.channel-change"
											/>
										</div>
									)}
								</div>
								<div className={styles.urlWrapper} data-flx="webhook.webhook-list-item.url-wrapper">
									<Input
										id={`webhook-url-${webhook.id}`}
										label={i18n._(WEBHOOK_URL_DESCRIPTOR)}
										value={webhookUrl}
										readOnly
										onFocus={(event) => event.currentTarget.select()}
										className={styles.monoInput}
										data-flx="webhook.webhook-list-item.mono-input"
									/>
								</div>
							</div>
						</div>
						<div className={styles.actions} data-flx="webhook.webhook-list-item.actions">
							<Button
								variant="secondary"
								small
								onClick={handleCopy}
								leftIcon={
									<CopySimpleIcon
										className={styles.iconSmall}
										weight="fill"
										data-flx="webhook.webhook-list-item.icon-small"
									/>
								}
								data-flx="webhook.webhook-list-item.button.copy"
							>
								<Trans comment="Button label that copies the webhook URL to the clipboard.">Copy webhook URL</Trans>
							</Button>
							<Button
								variant="danger"
								onClick={handleDelete}
								submitting={isDeleting}
								leftIcon={
									<TrashSimpleIcon
										className={styles.iconSmall}
										weight="fill"
										data-flx="webhook.webhook-list-item.icon-small--2"
									/>
								}
								small={true}
								data-flx="webhook.webhook-list-item.button.delete"
							>
								<Trans comment="Danger button label that deletes a webhook.">Delete webhook</Trans>
							</Button>
						</div>
					</div>
				)}
			</div>
		);
	},
);
