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
import {CaretDownIcon, CopySimpleIcon, TrashSimpleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {Input} from '~/components/form/Input';
import {Select} from '~/components/form/Select';
import {Button} from '~/components/uikit/Button/Button';
import type {WebhookRecord} from '~/records/WebhookRecord';
import * as AvatarUtils from '~/utils/AvatarUtils';
import * as DateUtils from '~/utils/DateUtils';
import {openFilePicker} from '~/utils/FilePickerUtils';
import styles from './WebhookListItem.module.css';

interface ChannelOption {
	id: string;
	label: string;
}

interface WebhookListItemProps {
	webhook: WebhookRecord;
	channelName?: string | null;
	onDelete: (webhook: WebhookRecord) => Promise<void>;
	onUpdate: (webhookId: string, updates: {name?: string; avatar?: string | null; channelId?: string}) => void;
	availableChannels?: Array<ChannelOption>;
	defaultExpanded?: boolean;
	isExpanded?: boolean;
	onExpandedChange?: (expanded: boolean) => void;
	formVersion?: number;
}

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
		const {t, i18n} = useLingui();
		const [localExpanded, setLocalExpanded] = React.useState(defaultExpanded);
		const expanded = isExpanded ?? localExpanded;
		const setExpanded = React.useCallback(
			(next: boolean) => {
				if (onExpandedChange) onExpandedChange(next);
				else setLocalExpanded(next);
			},
			[onExpandedChange],
		);

		const [isDeleting, setIsDeleting] = React.useState(false);
		const [isUpdatingAvatar, setIsUpdatingAvatar] = React.useState(false);
		const [selectedChannelId, setSelectedChannelId] = React.useState(webhook.channelId);
		const [currentName, setCurrentName] = React.useState(webhook.name);

		const [localAvatar, setLocalAvatar] = React.useState<string | null | undefined>(undefined);

		React.useEffect(() => {
			setLocalAvatar(undefined);
		}, []);

		React.useEffect(() => {
			if (formVersion == null) return;
			setLocalAvatar(undefined);
			setCurrentName(webhook.name);
			setSelectedChannelId(webhook.channelId);
		}, [formVersion, webhook.name, webhook.channelId]);

		const creator = webhook.creator;
		const creatorDisplayName = creator?.displayName ?? t`Unknown user`;
		const createdAt = React.useMemo(() => DateUtils.getFormattedShortDate(webhook.createdAt), [webhook.createdAt]);

		const effectiveAvatar: string | null = localAvatar !== undefined ? localAvatar : (webhook.avatar ?? null);

		const avatarUrl = React.useMemo(() => {
			return AvatarUtils.getWebhookAvatarURL({id: webhook.id, avatar: effectiveAvatar}, false);
		}, [webhook.id, effectiveAvatar]);

		const webhookUrl = React.useMemo(() => webhook.webhookUrl, [webhook.webhookUrl]);

		const handleCopy = React.useCallback(async () => {
			try {
				await TextCopyActionCreators.copy(i18n, webhookUrl);
			} catch (error) {
				console.error('Failed to copy webhook URL', error);
			}
		}, [i18n, webhookUrl]);

		const handleDelete = React.useCallback(async () => {
			if (!onDelete) return;
			setIsDeleting(true);
			try {
				await onDelete(webhook);
			} catch (error) {
				console.error('Failed to delete webhook', error);
				ToastActionCreators.createToast({type: 'error', children: t`Failed to delete webhook`});
			} finally {
				setIsDeleting(false);
			}
		}, [onDelete, webhook]);

		const handleChannelChange = React.useCallback(
			(newChannelId: string) => {
				if (newChannelId === webhook.channelId) return;
				setSelectedChannelId(newChannelId);
				onUpdate?.(webhook.id, {channelId: newChannelId});
			},
			[onUpdate, webhook.id, webhook.channelId],
		);

		const handleAvatarUpload = React.useCallback(async () => {
			const [file] = await openFilePicker({accept: 'image/*'});
			if (!file) return;

			if (file.size > 10 * 1024 * 1024) {
				ToastActionCreators.createToast({
					type: 'error',
					children: t`Avatar file is too large. Please choose a file smaller than 10MB.`,
				});
				return;
			}

			try {
				setIsUpdatingAvatar(true);
				const base64 = await AvatarUtils.fileToBase64(file);
				setLocalAvatar(base64);
				onUpdate?.(webhook.id, {avatar: base64});
			} catch {
				ToastActionCreators.createToast({
					type: 'error',
					children: <Trans>That image is invalid. Please try another one.</Trans>,
				});
			} finally {
				setIsUpdatingAvatar(false);
			}
		}, [webhook.id, onUpdate]);

		const handleClearAvatar = React.useCallback(() => {
			setLocalAvatar(null);
			onUpdate?.(webhook.id, {avatar: null});
		}, [webhook.id, onUpdate]);

		return (
			<div className={styles.container}>
				<button
					type="button"
					className={styles.headerButton}
					onClick={() => setExpanded(!expanded)}
					aria-expanded={expanded}
				>
					<div className={styles.left}>
						<div className={styles.avatarLarge} style={{backgroundImage: `url(${avatarUrl})`}} aria-hidden />
						<div className={styles.textBlock}>
							<div className={styles.titleRow}>
								<span className={styles.name}>{currentName}</span>
								{channelName && <span className={styles.channelTag}>#{channelName}</span>}
							</div>
							<div className={styles.metaRow}>
								<span className={styles.truncate}>
									<Trans>
										Created by {creatorDisplayName} on {createdAt}
									</Trans>
								</span>
								{channelName && <span className={styles.channelTagMobile}>#{channelName}</span>}
							</div>
						</div>
					</div>
					<CaretDownIcon className={clsx(styles.chevron, expanded && styles.chevronExpanded)} weight="bold" />
				</button>

				{expanded && (
					<div className={styles.details}>
						<div className={styles.detailsRow}>
							<div className={styles.avatarColumn}>
								<label htmlFor={`webhook-avatar-${webhook.id}`} className={styles.label}>
									<Trans>Avatar</Trans>
								</label>
								<div className={styles.avatarPreview} style={{backgroundImage: `url(${avatarUrl})`}} aria-hidden />
								<div className={styles.avatarActions}>
									<Button variant="secondary" small={true} onClick={handleAvatarUpload} submitting={isUpdatingAvatar}>
										<Trans>Upload Image</Trans>
									</Button>
									{effectiveAvatar !== null && (
										<Button variant="secondary" small onClick={handleClearAvatar}>
											<Trans>Remove</Trans>
										</Button>
									)}
								</div>
							</div>

							<div className={styles.fields}>
								<div className={styles.fieldsRow}>
									<div className={styles.fieldGrow}>
										<Input
											id={`webhook-name-${webhook.id}`}
											label={t`Name`}
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
											placeholder={t`Webhook name`}
										/>
									</div>

									{availableChannels && availableChannels.length > 0 && (
										<div className={styles.fieldGrow}>
											<Select
												label={t`Channel`}
												value={selectedChannelId}
												options={availableChannels.map((option) => ({
													value: option.id,
													label: option.label,
												}))}
												onChange={handleChannelChange}
											/>
										</div>
									)}
								</div>

								<div className={styles.urlWrapper}>
									<Input
										id={`webhook-url-${webhook.id}`}
										label={t`Webhook URL`}
										value={webhookUrl}
										readOnly
										onFocus={(event) => event.currentTarget.select()}
										className={styles.monoInput}
									/>
								</div>
							</div>
						</div>

						<div className={styles.actions}>
							<Button
								variant="secondary"
								small
								onClick={handleCopy}
								leftIcon={<CopySimpleIcon className={styles.iconSmall} weight="fill" />}
							>
								<Trans>Copy Webhook URL</Trans>
							</Button>
							<Button
								variant="danger-primary"
								onClick={handleDelete}
								submitting={isDeleting}
								leftIcon={<TrashSimpleIcon className={styles.iconSmall} weight="fill" />}
								small={true}
							>
								<Trans>Delete Webhook</Trans>
							</Button>
						</div>
					</div>
				)}
			</div>
		);
	},
);
