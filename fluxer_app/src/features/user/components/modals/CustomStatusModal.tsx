// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {
	DEFAULT_TIME_WINDOW_KEY,
	getTimeWindowKeyForExpiresAt,
	getTimeWindowPresets,
	TIME_WINDOW_LABEL_MESSAGES,
	type TimeWindowKey,
	type TimeWindowPreset,
} from '@app/features/app/config/TimeWindowPresets';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import DeveloperMode from '@app/features/devtools/state/DeveloperMode';
import Emoji from '@app/features/emoji/state/Emoji';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {ExpressionPickerPopout} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import {buildCustomEmojiURL} from '@app/features/expressions/utils/CustomEmojiImageUrl';
import {getEmojiURL} from '@app/features/expressions/utils/EmojiUtils';
import {getSkinTonedSurrogate} from '@app/features/expressions/utils/SkinToneUtils';
import {Button} from '@app/features/ui/button/Button';
import {CharacterCountAnnouncer} from '@app/features/ui/character_counter/CharacterCountAnnouncer';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Input} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {getRelativeDayLabelLower} from '@app/features/ui/utils/RelativeDayLabels';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import styles from '@app/features/user/components/modals/CustomStatusModal.module.css';
import {ProfilePreview} from '@app/features/user/components/profile/ProfilePreview';
import {type CustomStatus, normalizeCustomStatus} from '@app/features/user/state/CustomStatus';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {getDaysBetween} from '@fluxer/date_utils/src/DateComparison';
import {getFormattedShortDate, getFormattedTime} from '@fluxer/date_utils/src/DateFormatting';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {SmileyIcon, XIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useRef, useState} from 'react';

const SET_YOUR_STATUS_DESCRIPTOR = msg({
	message: 'Set your status',
	comment: 'Short label in the custom status modal. Keep it concise.',
});
const WHAT_S_HAPPENING_DESCRIPTOR = msg({
	message: "What's happening?",
	comment: 'Placeholder question in the custom status text field.',
});
const CHANGE_EMOJI_DESCRIPTOR = msg({
	message: 'Change emoji',
	comment: 'Short label in the custom status modal. Keep it concise.',
});
const CHOOSE_AN_EMOJI_DESCRIPTOR = msg({
	message: 'Choose an emoji',
	comment: 'Button or menu action label in the custom status modal. Keep it concise.',
});
const CLEAR_CUSTOM_STATUS_DESCRIPTOR = msg({
	message: 'Clear custom status',
	comment: 'Button or menu action label in the custom status modal. Keep it concise.',
});
const MS_PER_MINUTE = 60 * 1000;
const EXPIRATION_MENU_MAX_HEIGHT = 600;
const EMPTY_PREVIEW_CUSTOM_STATUS: CustomStatus = {
	text: '\u200B',
	expiresAt: null,
	emojiId: null,
	emojiName: null,
};

interface TimeLabel {
	dayLabel: string;
	timeString: string;
}

interface ExpirationPreset {
	key: TimeWindowKey;
	label: string;
	minutes: number | null;
}

interface ExpirationOption {
	key: TimeWindowKey;
	minutes: number | null;
	expiresAt: string | null;
	relativeLabel: TimeLabel | null;
	label: string;
}

const DEFAULT_EXPIRATION_KEY: TimeWindowKey = DEFAULT_TIME_WINDOW_KEY;
const RELATIVE_DAY_TIME_DESCRIPTOR = msg({
	message: '{dayLabel} at {timeString}',
	comment:
		'Custom status clear-time label. dayLabel is a relative day such as "today" or a formatted date; timeString is the localized time.',
});
const getPopoutClose = (renderProps: unknown): (() => void) => {
	const props = renderProps as {
		close?: unknown;
		requestClose?: unknown;
		onClose?: unknown;
	};
	if (typeof props.close === 'function') return props.close as () => void;
	if (typeof props.requestClose === 'function') return props.requestClose as () => void;
	if (typeof props.onClose === 'function') return props.onClose as () => void;
	return () => {};
};
const formatLabelWithRelative = (i18n: I18n, label: string, relative: TimeLabel | null): React.ReactNode => {
	if (!relative) return label;
	return (
		<>
			{label} ({i18n._(RELATIVE_DAY_TIME_DESCRIPTOR, {dayLabel: relative.dayLabel, timeString: relative.timeString})})
		</>
	);
};
const getDayDifference = (reference: Date, target: Date): number => {
	return getDaysBetween(target, reference);
};
const formatTimeString = (date: Date): string => getFormattedTime(date, getCurrentLocale());
const formatRelativeDayTimeLabel = (reference: Date, target: Date): TimeLabel => {
	const dayOffset = getDayDifference(reference, target);
	const timeString = formatTimeString(target);
	if (dayOffset === 0) return {dayLabel: getRelativeDayLabelLower(getCurrentLocale(), 0), timeString};
	if (dayOffset === 1) return {dayLabel: getRelativeDayLabelLower(getCurrentLocale(), 1), timeString};
	const dayLabel = getFormattedShortDate(target, getCurrentLocale());
	return {dayLabel, timeString};
};
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
export const CustomStatusModal = observer(() => {
	const {i18n} = useLingui();
	const initialStatus = normalizeCustomStatus(UserSettings.customStatus);
	const currentUser = Users.getCurrentUser();
	const isDeveloper = DeveloperMode.isDeveloper;
	const shouldAnimateEmojiPreview = useShouldAnimate({kind: 'emoji'});
	const [statusText, setStatusText] = useState(initialStatus?.text ?? '');
	const [emojiId, setEmojiId] = useState<string | null>(initialStatus?.emojiId ?? null);
	const [emojiName, setEmojiName] = useState<string | null>(initialStatus?.emojiName ?? null);
	const mountedAt = useMemo(() => new Date(), []);
	const [emojiPickerOpen, setEmojiPickerOpen] = useState(false);
	const emojiButtonRef = useRef<HTMLButtonElement | null>(null);
	const expirationPresets = useMemo(
		() =>
			getTimeWindowPresets({includeDeveloperOptions: isDeveloper}).map((preset: TimeWindowPreset) => ({
				key: preset.key,
				label: i18n._(TIME_WINDOW_LABEL_MESSAGES[preset.key]),
				minutes: preset.minutes,
			})),
		[i18n.locale, isDeveloper],
	);
	const expirationOptions = useMemo<Array<ExpirationOption>>(
		() =>
			expirationPresets.map((preset: ExpirationPreset) => {
				if (preset.minutes == null) {
					return {...preset, expiresAt: null, relativeLabel: null};
				}
				const target = new Date(mountedAt.getTime() + preset.minutes * MS_PER_MINUTE);
				return {
					...preset,
					expiresAt: target.toISOString(),
					relativeLabel: formatRelativeDayTimeLabel(mountedAt, target),
				};
			}),
		[mountedAt, expirationPresets],
	);
	const expirationLabelMap = useMemo<Record<TimeWindowKey, TimeLabel | null>>(() => {
		return expirationOptions.reduce<Record<TimeWindowKey, TimeLabel | null>>(
			(acc, option) => {
				acc[option.key] = option.relativeLabel;
				return acc;
			},
			{} as Record<TimeWindowKey, TimeLabel | null>,
		);
	}, [expirationOptions]);
	const selectOptions = useMemo<Array<ComboboxOption<TimeWindowKey>>>(() => {
		return expirationOptions.map((option) => ({value: option.key, label: option.label}));
	}, [expirationOptions]);
	const [selectedExpiration, setSelectedExpiration] = useState<TimeWindowKey>(() =>
		initialStatus
			? getTimeWindowKeyForExpiresAt(initialStatus.expiresAt, {
					includeDeveloperOptions: isDeveloper,
					referenceTime: mountedAt,
					fallbackKey: DEFAULT_EXPIRATION_KEY,
				})
			: DEFAULT_EXPIRATION_KEY,
	);
	const [isSaving, setIsSaving] = useState(false);
	const draftStatus = useMemo(
		() => buildDraftStatus({text: statusText.trim(), emojiId, emojiName, expiresAt: null}),
		[statusText, emojiId, emojiName],
	);
	const previewCustomStatus = draftStatus ?? EMPTY_PREVIEW_CUSTOM_STATUS;
	const getExpiresAtForSave = useCallback((): string | null => {
		const option = expirationOptions.find((entry) => entry.key === selectedExpiration);
		if (!option?.minutes) return null;
		return new Date(Date.now() + option.minutes * MS_PER_MINUTE).toISOString();
	}, [expirationOptions, selectedExpiration]);
	const handleExpirationChange = (value: TimeWindowKey) => {
		setSelectedExpiration(value);
	};
	const handleEmojiSelect = useCallback((emoji: FlatEmoji) => {
		if (emoji.id) {
			setEmojiId(emoji.id);
			setEmojiName(emoji.name);
		} else {
			setEmojiId(null);
			setEmojiName(getSkinTonedSurrogate(emoji));
		}
	}, []);
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
			ModalCommands.pop();
		} finally {
			setIsSaving(false);
		}
	};
	const handleClearDraft = () => {
		setStatusText('');
		setEmojiId(null);
		setEmojiName(null);
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
						data-flx="user.custom-status-modal.render-emoji-preview.emoji-preview-image"
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
					data-flx="user.custom-status-modal.render-emoji-preview.emoji-preview-image--2"
				/>
			);
		}
		return null;
	};
	const emojiPreview = renderEmojiPreview();
	return (
		<Modal.Root
			onClose={() => ModalCommands.pop()}
			size="medium"
			className={styles.modalRoot}
			data-flx="user.custom-status-modal.modal-root"
		>
			<Modal.ScreenReaderLabel
				text={i18n._(SET_YOUR_STATUS_DESCRIPTOR)}
				data-flx="user.custom-status-modal.modal-screen-reader-label"
			/>
			<Modal.Header title={i18n._(SET_YOUR_STATUS_DESCRIPTOR)} data-flx="user.custom-status-modal.modal-header" />
			<Modal.Content data-flx="user.custom-status-modal.modal-content">
				<div className={styles.previewSection} data-flx="user.custom-status-modal.preview-section">
					{currentUser && (
						<ProfilePreview
							user={currentUser}
							showMembershipInfo={false}
							showMessageButton={false}
							showPreviewLabel={false}
							previewCustomStatus={previewCustomStatus}
							data-flx="user.custom-status-modal.profile-preview"
						/>
					)}
				</div>
				<div className={styles.statusInputWrapper} data-flx="user.custom-status-modal.status-input-wrapper">
					<div className={styles.characterCount} aria-hidden="true" data-flx="user.custom-status-modal.character-count">
						{statusText.length}/128
					</div>
					<Input
						id="custom-status-text"
						value={statusText}
						onChange={(event) => setStatusText(event.target.value.slice(0, 128))}
						maxLength={128}
						placeholder={i18n._(WHAT_S_HAPPENING_DESCRIPTOR)}
						leftElement={
							<Popout
								position="bottom-start"
								animationType="none"
								offsetMainAxis={8}
								offsetCrossAxis={0}
								onOpen={() => setEmojiPickerOpen(true)}
								onClose={() => setEmojiPickerOpen(false)}
								returnFocusRef={emojiButtonRef}
								render={(renderProps) => {
									const closePopout = getPopoutClose(renderProps);
									return (
										<ExpressionPickerPopout
											onEmojiSelect={(emoji) => {
												handleEmojiSelect(emoji);
												setEmojiPickerOpen(false);
												closePopout();
											}}
											onClose={() => {
												setEmojiPickerOpen(false);
												closePopout();
											}}
											visibleTabs={['emojis']}
											data-flx="user.custom-status-modal.expression-picker-popout"
										/>
									);
								}}
								data-flx="user.custom-status-modal.popout"
							>
								<FocusRing offset={-2} enabled={!isSaving} data-flx="user.custom-status-modal.focus-ring">
									<button
										ref={emojiButtonRef}
										type="button"
										className={clsx(styles.emojiTriggerButton, emojiPickerOpen && styles.emojiTriggerButtonActive)}
										aria-label={emojiPreview ? i18n._(CHANGE_EMOJI_DESCRIPTOR) : i18n._(CHOOSE_AN_EMOJI_DESCRIPTOR)}
										aria-haspopup="dialog"
										aria-expanded={emojiPickerOpen}
										disabled={isSaving}
										data-flx="user.custom-status-modal.emoji-trigger-button"
									>
										{emojiPreview ?? (
											<SmileyIcon
												size={22}
												weight="fill"
												aria-hidden="true"
												data-flx="user.custom-status-modal.smiley-icon"
											/>
										)}
									</button>
								</FocusRing>
							</Popout>
						}
						rightElement={
							draftStatus ? (
								<FocusRing offset={-2} enabled={!isSaving} data-flx="user.custom-status-modal.focus-ring--2">
									<button
										type="button"
										className={styles.clearButtonIcon}
										onClick={handleClearDraft}
										disabled={isSaving}
										aria-label={i18n._(CLEAR_CUSTOM_STATUS_DESCRIPTOR)}
										data-flx="user.custom-status-modal.clear-button-icon.clear-draft"
									>
										<XIcon size={16} weight="bold" data-flx="user.custom-status-modal.x-icon" />
									</button>
								</FocusRing>
							) : null
						}
						data-flx="user.custom-status-modal.custom-status-text.set-status-text"
					/>
					<CharacterCountAnnouncer
						currentLength={statusText.length}
						maxLength={128}
						data-flx="user.custom-status-modal.character-count-announcer"
					/>
				</div>
			</Modal.Content>
			<Modal.Footer className={styles.footer} stretchButtons data-flx="user.custom-status-modal.footer">
				<div className={styles.expirationSelectWrapper} data-flx="user.custom-status-modal.expiration-select-wrapper">
					<label
						className={styles.expirationLabel}
						htmlFor="custom-status-expiration"
						data-flx="user.custom-status-modal.expiration-label"
					>
						<Trans>Clear after</Trans>
					</label>
					<Combobox
						id="custom-status-expiration"
						className={styles.expirationSelect}
						options={selectOptions}
						value={selectedExpiration}
						onChange={handleExpirationChange}
						disabled={isSaving}
						maxMenuHeight={EXPIRATION_MENU_MAX_HEIGHT}
						renderOption={(option) => formatLabelWithRelative(i18n, option.label, expirationLabelMap[option.value])}
						renderValue={(option) =>
							option ? formatLabelWithRelative(i18n, option.label, expirationLabelMap[option.value]) : null
						}
						data-flx="user.custom-status-modal.custom-status-expiration.expiration-change"
					/>
				</div>
				<Button
					variant="primary"
					onClick={handleSave}
					submitting={isSaving}
					fitContainer
					data-flx="user.custom-status-modal.button.save"
				>
					<Trans>Update my status</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});

CustomStatusModal.displayName = 'CustomStatusModal';
