// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getTimeWindowKeyForExpiresAt,
	getTimeWindowPresets,
	minutesToMs,
	TIME_WINDOW_LABEL_MESSAGES,
	type TimeWindowKey,
	type TimeWindowPreset,
} from '@app/features/app/config/TimeWindowPresets';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import DeveloperMode from '@app/features/devtools/state/DeveloperMode';
import Emoji from '@app/features/emoji/state/Emoji';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {ExpressionPickerSheet} from '@app/features/expressions/components/modals/ExpressionPickerSheet';
import {buildCustomEmojiURL} from '@app/features/expressions/utils/CustomEmojiImageUrl';
import {getEmojiURL} from '@app/features/expressions/utils/EmojiUtils';
import {getSkinTonedSurrogate} from '@app/features/expressions/utils/SkinToneUtils';
import Presence from '@app/features/presence/state/Presence';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import styles from '@app/features/user/components/modals/CustomStatusBottomSheet.module.css';
import {type CustomStatus, normalizeCustomStatus} from '@app/features/user/state/CustomStatus';
import Users from '@app/features/user/state/Users';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {SmileyIcon, XIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const SET_CUSTOM_STATUS_DESCRIPTOR = msg({
	message: 'Set custom status',
	comment: 'Short label in the custom status bottom sheet. Keep it concise.',
});
const WHAT_S_HAPPENING_DESCRIPTOR = msg({
	message: "What's happening?",
	comment: 'Placeholder question in the custom status text field.',
});
const CHANGE_EMOJI_DESCRIPTOR = msg({
	message: 'Change emoji',
	comment: 'Short label in the custom status bottom sheet. Keep it concise.',
});
const CHOOSE_AN_EMOJI_DESCRIPTOR = msg({
	message: 'Choose an emoji',
	comment: 'Button or menu action label in the custom status bottom sheet. Keep it concise.',
});
const CLEAR_CUSTOM_STATUS_DESCRIPTOR = msg({
	message: 'Clear custom status',
	comment: 'Button or menu action label in the custom status bottom sheet. Keep it concise.',
});
const CUSTOM_STATUS_SNAP_POINTS: Array<number> = [0, 1];

interface ExpiryOption {
	id: TimeWindowKey;
	label: string;
	minutes: number | null;
}

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
	const {i18n} = useLingui();
	const currentUser = Users.getCurrentUser();
	const currentUserId = currentUser?.id ?? null;
	const existingCustomStatus = currentUserId ? Presence.getCustomStatus(currentUserId) : null;
	const normalizedExisting = normalizeCustomStatus(existingCustomStatus);
	const isDeveloper = DeveloperMode.isDeveloper;
	const shouldAnimateEmojiPreview = useShouldAnimate({kind: 'emoji'});
	const [statusText, setStatusText] = useState('');
	const [emojiId, setEmojiId] = useState<string | null>(null);
	const [emojiName, setEmojiName] = useState<string | null>(null);
	const [selectedExpiry, setSelectedExpiry] = useState<TimeWindowKey>('never');
	const [isSaving, setIsSaving] = useState(false);
	const [emojiPickerOpen, setEmojiPickerOpen] = useState(false);
	useEffect(() => {
		if (isOpen) {
			setStatusText(normalizedExisting?.text ?? '');
			setEmojiId(normalizedExisting?.emojiId ?? null);
			setEmojiName(normalizedExisting?.emojiName ?? null);
			setSelectedExpiry(
				normalizedExisting
					? getTimeWindowKeyForExpiresAt(normalizedExisting.expiresAt, {
							includeDeveloperOptions: isDeveloper,
							fallbackKey: 'never',
						})
					: 'never',
			);
		}
	}, [
		isOpen,
		normalizedExisting?.text,
		normalizedExisting?.emojiId,
		normalizedExisting?.emojiName,
		normalizedExisting?.expiresAt,
		isDeveloper,
	]);
	const expiryOptions = useMemo(
		() =>
			getTimeWindowPresets({includeDeveloperOptions: isDeveloper}).map((preset: TimeWindowPreset) => ({
				id: preset.key,
				label: i18n._(TIME_WINDOW_LABEL_MESSAGES[preset.key]),
				minutes: preset.minutes,
			})),
		[i18n.locale, isDeveloper],
	);
	const draftStatus = useMemo(
		() => buildDraftStatus({text: statusText.trim(), emojiId, emojiName, expiresAt: null}),
		[statusText, emojiId, emojiName],
	);
	const getExpiresAtForSave = useCallback((): string | null => {
		const option = expiryOptions.find((o: ExpiryOption) => o.id === selectedExpiry);
		if (!option?.minutes) return null;
		return new Date(Date.now() + minutesToMs(option.minutes)!).toISOString();
	}, [expiryOptions, selectedExpiry]);
	const handleEmojiSelect = useCallback((emoji: FlatEmoji) => {
		if (emoji.id) {
			setEmojiId(emoji.id);
			setEmojiName(emoji.name);
		} else {
			setEmojiId(null);
			setEmojiName(getSkinTonedSurrogate(emoji));
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
			const statusToSave = buildDraftStatus({
				text: statusText.trim(),
				emojiId,
				emojiName,
				expiresAt: getExpiresAtForSave(),
			});
			await UserSettingsCommands.update({customStatus: statusToSave});
			onClose();
		} finally {
			setIsSaving(false);
		}
	};
	const renderEmojiPreview = (): React.ReactNode => {
		if (!draftStatus) return null;
		if (draftStatus.emojiId) {
			const emoji = Emoji.getEmojiById(draftStatus.emojiId);
			if (emoji) {
				return (
					<img
						src={buildCustomEmojiURL({
							id: draftStatus.emojiId,
							animated: Boolean(emoji.animated) && shouldAnimateEmojiPreview,
						})}
						alt={emoji.name}
						className={styles.emojiPreviewImage}
						data-flx="user.custom-status-bottom-sheet.render-emoji-preview.emoji-preview-image"
					/>
				);
			}
		}
		if (draftStatus.emojiName) {
			const twemojiUrl = getEmojiURL(draftStatus.emojiName);
			if (!twemojiUrl) return null;
			return (
				<img
					src={twemojiUrl}
					alt={draftStatus.emojiName}
					className={styles.emojiPreviewImage}
					data-flx="user.custom-status-bottom-sheet.render-emoji-preview.emoji-preview-image--2"
				/>
			);
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
			title={i18n._(SET_CUSTOM_STATUS_DESCRIPTOR)}
			zIndex={10001}
			data-flx="user.custom-status-bottom-sheet.bottom-sheet"
		>
			<div className={styles.content} data-flx="user.custom-status-bottom-sheet.content">
				<Input
					id="custom-status-text"
					value={statusText}
					onChange={(event) => setStatusText(event.target.value.slice(0, 128))}
					maxLength={128}
					placeholder={i18n._(WHAT_S_HAPPENING_DESCRIPTOR)}
					leftElement={
						<FocusRing offset={-2} enabled={!isSaving} data-flx="user.custom-status-bottom-sheet.focus-ring">
							<button
								type="button"
								className={clsx(styles.emojiTriggerButton, emojiPickerOpen && styles.emojiTriggerButtonActive)}
								aria-label={emojiPreview ? i18n._(CHANGE_EMOJI_DESCRIPTOR) : i18n._(CHOOSE_AN_EMOJI_DESCRIPTOR)}
								aria-haspopup="dialog"
								aria-expanded={emojiPickerOpen}
								disabled={isSaving}
								onClick={() => setEmojiPickerOpen(true)}
								data-flx="user.custom-status-bottom-sheet.emoji-trigger-button.set-emoji-picker-open"
							>
								{emojiPreview ?? (
									<SmileyIcon
										size={22}
										weight="fill"
										aria-hidden="true"
										data-flx="user.custom-status-bottom-sheet.smiley-icon"
									/>
								)}
							</button>
						</FocusRing>
					}
					rightElement={
						draftStatus ? (
							<FocusRing offset={-2} enabled={!isSaving} data-flx="user.custom-status-bottom-sheet.focus-ring--2">
								<button
									type="button"
									className={styles.clearButtonIcon}
									onClick={handleClearDraft}
									disabled={isSaving}
									aria-label={i18n._(CLEAR_CUSTOM_STATUS_DESCRIPTOR)}
									data-flx="user.custom-status-bottom-sheet.clear-button-icon.clear-draft"
								>
									<XIcon size={16} weight="bold" data-flx="user.custom-status-bottom-sheet.x-icon" />
								</button>
							</FocusRing>
						) : null
					}
					data-flx="user.custom-status-bottom-sheet.custom-status-text.set-status-text"
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
					data-flx="user.custom-status-bottom-sheet.expression-picker-sheet"
				/>
				<div className={styles.footer} data-flx="user.custom-status-bottom-sheet.footer">
					<div className={styles.expirySelector} data-flx="user.custom-status-bottom-sheet.expiry-selector">
						<span
							className={styles.expirySelectorLabel}
							data-flx="user.custom-status-bottom-sheet.expiry-selector-label"
						>
							<Trans>Clear after</Trans>
						</span>
						<select
							className={styles.expirySelect}
							value={selectedExpiry}
							onChange={(e) => setSelectedExpiry(e.target.value as TimeWindowKey)}
							disabled={isSaving}
							data-flx="user.custom-status-bottom-sheet.expiry-select.set-selected-expiry"
						>
							{expiryOptions.map((option: ExpiryOption) => (
								<option key={option.id} value={option.id} data-flx="user.custom-status-bottom-sheet.option">
									{option.label}
								</option>
							))}
						</select>
					</div>
					<Button
						variant="primary"
						onClick={handleSave}
						submitting={isSaving}
						className={styles.saveButton}
						data-flx="user.custom-status-bottom-sheet.save-button"
					>
						<Trans>Save</Trans>
					</Button>
				</div>
			</div>
		</BottomSheet>
	);
});

CustomStatusBottomSheet.displayName = 'CustomStatusBottomSheet';
