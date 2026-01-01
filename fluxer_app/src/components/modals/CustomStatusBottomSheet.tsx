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
import {SmileyIcon, XIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as UserSettingsActionCreators from '~/actions/UserSettingsActionCreators';
import {Input} from '~/components/form/Input';
import {ExpressionPickerSheet} from '~/components/modals/ExpressionPickerSheet';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import {Button} from '~/components/uikit/Button/Button';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {type CustomStatus, normalizeCustomStatus} from '~/lib/customStatus';
import type {Emoji} from '~/stores/EmojiStore';
import EmojiStore from '~/stores/EmojiStore';
import PresenceStore from '~/stores/PresenceStore';
import UserStore from '~/stores/UserStore';
import {getEmojiURL, shouldUseNativeEmoji} from '~/utils/EmojiUtils';
import styles from './CustomStatusBottomSheet.module.css';

const CUSTOM_STATUS_SNAP_POINTS: Array<number> = [0, 1];

const EXPIRY_OPTIONS = [
	{id: 'never', label: <Trans>Don&apos;t clear</Trans>, minutes: null},
	{id: '30m', label: <Trans>30 minutes</Trans>, minutes: 30},
	{id: '1h', label: <Trans>1 hour</Trans>, minutes: 60},
	{id: '4h', label: <Trans>4 hours</Trans>, minutes: 4 * 60},
	{id: '24h', label: <Trans>24 hours</Trans>, minutes: 24 * 60},
];

interface CustomStatusBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
}

const buildDraftStatus = (params: {
	text: string;
	emojiId: string | null;
	emojiName: string | null;
	expiresAt: string | null;
}): CustomStatus | null => {
	return normalizeCustomStatus({
		text: params.text || null,
		emojiId: params.emojiId,
		emojiName: params.emojiName,
		expiresAt: params.expiresAt,
	});
};

export const CustomStatusBottomSheet = observer(({isOpen, onClose}: CustomStatusBottomSheetProps) => {
	const {t} = useLingui();
	const currentUser = UserStore.getCurrentUser();
	const currentUserId = currentUser?.id ?? null;
	const existingCustomStatus = currentUserId ? PresenceStore.getCustomStatus(currentUserId) : null;
	const normalizedExisting = normalizeCustomStatus(existingCustomStatus);

	const [statusText, setStatusText] = React.useState('');
	const [emojiId, setEmojiId] = React.useState<string | null>(null);
	const [emojiName, setEmojiName] = React.useState<string | null>(null);
	const [selectedExpiry, setSelectedExpiry] = React.useState<string>('never');
	const [isSaving, setIsSaving] = React.useState(false);
	const [emojiPickerOpen, setEmojiPickerOpen] = React.useState(false);

	const mountedAt = React.useMemo(() => new Date(), []);

	React.useEffect(() => {
		if (isOpen) {
			setStatusText(normalizedExisting?.text ?? '');
			setEmojiId(normalizedExisting?.emojiId ?? null);
			setEmojiName(normalizedExisting?.emojiName ?? null);
			setSelectedExpiry('never');
		}
	}, [isOpen, normalizedExisting?.text, normalizedExisting?.emojiId, normalizedExisting?.emojiName]);

	const getExpiresAt = React.useCallback(
		(expiryId: string): string | null => {
			const option = EXPIRY_OPTIONS.find((o) => o.id === expiryId);
			if (!option?.minutes) return null;
			return new Date(mountedAt.getTime() + option.minutes * 60 * 1000).toISOString();
		},
		[mountedAt],
	);

	const draftStatus = React.useMemo(
		() => buildDraftStatus({text: statusText.trim(), emojiId, emojiName, expiresAt: getExpiresAt(selectedExpiry)}),
		[statusText, emojiId, emojiName, selectedExpiry, getExpiresAt],
	);

	const handleEmojiSelect = React.useCallback((emoji: Emoji) => {
		if (emoji.id) {
			setEmojiId(emoji.id);
			setEmojiName(emoji.name);
		} else {
			setEmojiId(null);
			setEmojiName(emoji.surrogates ?? emoji.name);
		}
	}, []);

	const handleClearDraft = () => {
		setStatusText('');
		setEmojiId(null);
		setEmojiName(null);
	};

	const handleSave = async () => {
		if (isSaving) return;

		setIsSaving(true);
		try {
			await UserSettingsActionCreators.update({customStatus: draftStatus});
			onClose();
		} finally {
			setIsSaving(false);
		}
	};

	const renderEmojiPreview = (): React.ReactNode => {
		if (!draftStatus) return null;
		if (draftStatus.emojiId) {
			const emoji = EmojiStore.getEmojiById(draftStatus.emojiId);
			if (emoji?.url) {
				return <img src={emoji.url} alt={emoji.name} className={styles.emojiPreviewImage} />;
			}
		}
		if (draftStatus.emojiName) {
			if (!shouldUseNativeEmoji) {
				const twemojiUrl = getEmojiURL(draftStatus.emojiName);
				if (twemojiUrl) {
					return <img src={twemojiUrl} alt={draftStatus.emojiName} className={styles.emojiPreviewImage} />;
				}
			}
			return <span className={styles.emojiPreviewNative}>{draftStatus.emojiName}</span>;
		}
		return null;
	};

	const emojiPreview = renderEmojiPreview();

	return (
		<BottomSheet
			isOpen={isOpen}
			onClose={onClose}
			snapPoints={CUSTOM_STATUS_SNAP_POINTS}
			initialSnap={CUSTOM_STATUS_SNAP_POINTS.length - 1}
			title={t`Set Custom Status`}
			zIndex={10001}
		>
			<div className={styles.content}>
				<Input
					id="custom-status-text"
					value={statusText}
					onChange={(event) => setStatusText(event.target.value.slice(0, 128))}
					maxLength={128}
					placeholder={t`What's happening?`}
					leftElement={
						<FocusRing offset={-2} enabled={!isSaving}>
							<button
								type="button"
								className={clsx(styles.emojiTriggerButton, emojiPickerOpen && styles.emojiTriggerButtonActive)}
								aria-label={emojiPreview ? t`Change emoji` : t`Choose an emoji`}
								disabled={isSaving}
								onClick={() => setEmojiPickerOpen(true)}
							>
								{emojiPreview ?? <SmileyIcon size={22} weight="fill" aria-hidden="true" />}
							</button>
						</FocusRing>
					}
					rightElement={
						draftStatus ? (
							<FocusRing offset={-2} enabled={!isSaving}>
								<button
									type="button"
									className={styles.clearButtonIcon}
									onClick={handleClearDraft}
									disabled={isSaving}
									aria-label={t`Clear custom status`}
								>
									<XIcon size={16} weight="bold" />
								</button>
							</FocusRing>
						) : null
					}
				/>
				<ExpressionPickerSheet
					isOpen={emojiPickerOpen}
					onClose={() => setEmojiPickerOpen(false)}
					onEmojiSelect={(emoji) => {
						handleEmojiSelect(emoji);
						setEmojiPickerOpen(false);
					}}
					visibleTabs={['emojis']}
					zIndex={10002}
				/>
				<div className={styles.footer}>
					<div className={styles.expirySelector}>
						<span className={styles.expirySelectorLabel}>
							<Trans>Clear after</Trans>
						</span>
						<select
							className={styles.expirySelect}
							value={selectedExpiry}
							onChange={(e) => setSelectedExpiry(e.target.value)}
							disabled={isSaving}
						>
							{EXPIRY_OPTIONS.map((option) => (
								<option key={option.id} value={option.id}>
									{typeof option.label === 'string' ? option.label : option.id}
								</option>
							))}
						</select>
					</div>
					<Button variant="primary" onClick={handleSave} submitting={isSaving} className={styles.saveButton}>
						<Trans>Save</Trans>
					</Button>
				</div>
			</div>
		</BottomSheet>
	);
});

CustomStatusBottomSheet.displayName = 'CustomStatusBottomSheet';
