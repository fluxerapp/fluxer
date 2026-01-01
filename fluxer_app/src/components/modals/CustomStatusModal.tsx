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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {SmileyIcon, XIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as UserSettingsActionCreators from '~/actions/UserSettingsActionCreators';
import {Input} from '~/components/form/Input';
import {Select, type SelectOption} from '~/components/form/Select';
import * as Modal from '~/components/modals/Modal';
import {ExpressionPickerPopout} from '~/components/popouts/ExpressionPickerPopout';
import {ProfilePreview} from '~/components/profile/ProfilePreview';
import {Button} from '~/components/uikit/Button/Button';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Popout} from '~/components/uikit/Popout/Popout';
import {type CustomStatus, normalizeCustomStatus} from '~/lib/customStatus';
import type {Emoji} from '~/stores/EmojiStore';
import EmojiStore from '~/stores/EmojiStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import UserStore from '~/stores/UserStore';
import {getEmojiURL, shouldUseNativeEmoji} from '~/utils/EmojiUtils';
import styles from './CustomStatusModal.module.css';

const MS_PER_MINUTE = 60 * 1000;
const MS_PER_DAY = 24 * 60 * 60 * 1000;

interface TimeLabel {
	dayLabel: string;
	timeString: string;
}

type ExpirationKey = '24h' | '4h' | '1h' | '30m' | 'never';

interface ExpirationOption {
	key: ExpirationKey;
	minutes: number | null;
	expiresAt: string | null;
	relativeLabel: TimeLabel | null;
	label: string;
}

const DEFAULT_EXPIRATION_KEY: ExpirationKey = '24h';

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

const formatLabelWithRelative = (label: string, relative: TimeLabel | null): React.ReactNode => {
	if (!relative) return label;

	return (
		<>
			{label} (
			<Trans>
				{relative.dayLabel} at {relative.timeString}
			</Trans>
			)
		</>
	);
};

const getDayDifference = (reference: Date, target: Date): number => {
	const referenceDayStart = new Date(reference.getFullYear(), reference.getMonth(), reference.getDate());
	const targetDayStart = new Date(target.getFullYear(), target.getMonth(), target.getDate());
	return Math.round((targetDayStart.getTime() - referenceDayStart.getTime()) / MS_PER_DAY);
};

const formatTimeString = (date: Date): string =>
	date.toLocaleTimeString(undefined, {hour: '2-digit', minute: '2-digit', hourCycle: 'h23'});

const formatRelativeDayTimeLabel = (i18n: I18n, reference: Date, target: Date): TimeLabel => {
	const dayOffset = getDayDifference(reference, target);
	const timeString = formatTimeString(target);

	if (dayOffset === 0) return {dayLabel: i18n._(msg`today`), timeString};
	if (dayOffset === 1) return {dayLabel: i18n._(msg`tomorrow`), timeString};

	const dayLabel = target.toLocaleDateString(undefined, {
		weekday: 'short',
		month: 'short',
		day: 'numeric',
	});
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
	const initialStatus = normalizeCustomStatus(UserSettingsStore.customStatus);
	const currentUser = UserStore.getCurrentUser();

	const [statusText, setStatusText] = React.useState(initialStatus?.text ?? '');
	const [emojiId, setEmojiId] = React.useState<string | null>(initialStatus?.emojiId ?? null);
	const [emojiName, setEmojiName] = React.useState<string | null>(initialStatus?.emojiName ?? null);
	const mountedAt = React.useMemo(() => new Date(), []);
	const [emojiPickerOpen, setEmojiPickerOpen] = React.useState(false);
	const emojiButtonRef = React.useRef<HTMLButtonElement | null>(null);

	const expirationPresets = React.useMemo(
		() => [
			{key: '24h' as const, label: i18n._(msg`24 hours`), minutes: 24 * 60},
			{key: '4h' as const, label: i18n._(msg`4 hours`), minutes: 4 * 60},
			{key: '1h' as const, label: i18n._(msg`1 hour`), minutes: 60},
			{key: '30m' as const, label: i18n._(msg`30 minutes`), minutes: 30},
			{key: 'never' as const, label: i18n._(msg`Don't clear`), minutes: null},
		],
		[i18n],
	);

	const expirationOptions = React.useMemo<Array<ExpirationOption>>(
		() =>
			expirationPresets.map((preset) => {
				if (preset.minutes == null) {
					return {...preset, expiresAt: null, relativeLabel: null};
				}

				const target = new Date(mountedAt.getTime() + preset.minutes * MS_PER_MINUTE);

				return {
					...preset,
					expiresAt: target.toISOString(),
					relativeLabel: formatRelativeDayTimeLabel(i18n, mountedAt, target),
				};
			}),
		[mountedAt, i18n, expirationPresets],
	);

	const expirationLabelMap = React.useMemo<Record<ExpirationKey, TimeLabel | null>>(() => {
		return expirationOptions.reduce<Record<ExpirationKey, TimeLabel | null>>(
			(acc, option) => {
				acc[option.key] = option.relativeLabel;
				return acc;
			},
			{} as Record<ExpirationKey, TimeLabel | null>,
		);
	}, [expirationOptions]);

	const selectOptions = React.useMemo<Array<SelectOption<ExpirationKey>>>(() => {
		return expirationOptions.map((option) => ({value: option.key, label: option.label}));
	}, [expirationOptions]);

	const [selectedExpiration, setSelectedExpiration] = React.useState<ExpirationKey>(DEFAULT_EXPIRATION_KEY);
	const [expiresAt, setExpiresAt] = React.useState<string | null>(() => {
		return expirationOptions.find((option) => option.key === DEFAULT_EXPIRATION_KEY)?.expiresAt ?? null;
	});
	const [isSaving, setIsSaving] = React.useState(false);

	const draftStatus = React.useMemo(
		() => buildDraftStatus({text: statusText.trim(), emojiId, emojiName, expiresAt}),
		[statusText, emojiId, emojiName, expiresAt],
	);

	const handleExpirationChange = (value: ExpirationKey) => {
		const option = expirationOptions.find((entry) => entry.key === value);
		setSelectedExpiration(value);
		setExpiresAt(option?.expiresAt ?? null);
	};

	const handleEmojiSelect = React.useCallback((emoji: Emoji) => {
		if (emoji.id) {
			setEmojiId(emoji.id);
			setEmojiName(emoji.name);
		} else {
			setEmojiId(null);
			setEmojiName(emoji.surrogates ?? emoji.name);
		}
	}, []);

	const handleSave = async () => {
		if (isSaving) return;

		setIsSaving(true);
		try {
			await UserSettingsActionCreators.update({customStatus: draftStatus});
			ModalActionCreators.pop();
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
		<Modal.Root onClose={() => ModalActionCreators.pop()} size="medium" className={styles.modalRoot}>
			<Modal.ScreenReaderLabel text={i18n._(msg`Set your status`)} />
			<Modal.Header title={i18n._(msg`Set your status`)} />
			<Modal.Content>
				<div className={styles.previewSection}>
					{currentUser && (
						<ProfilePreview
							user={currentUser}
							showMembershipInfo={false}
							showMessageButton={false}
							showPreviewLabel={false}
							previewCustomStatus={draftStatus}
						/>
					)}
				</div>

				<div className={styles.statusInputWrapper}>
					<Input
						id="custom-status-text"
						value={statusText}
						onChange={(event) => setStatusText(event.target.value.slice(0, 128))}
						maxLength={128}
						placeholder={i18n._(msg`What's happening?`)}
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
										/>
									);
								}}
							>
								<FocusRing offset={-2} enabled={!isSaving}>
									<button
										ref={emojiButtonRef}
										type="button"
										className={clsx(styles.emojiTriggerButton, emojiPickerOpen && styles.emojiTriggerButtonActive)}
										aria-label={emojiPreview ? i18n._(msg`Change emoji`) : i18n._(msg`Choose an emoji`)}
										disabled={isSaving}
									>
										{emojiPreview ?? <SmileyIcon size={22} weight="fill" aria-hidden="true" />}
									</button>
								</FocusRing>
							</Popout>
						}
						rightElement={
							draftStatus ? (
								<FocusRing offset={-2} enabled={!isSaving}>
									<button
										type="button"
										className={styles.clearButtonIcon}
										onClick={handleClearDraft}
										disabled={isSaving}
										aria-label={i18n._(msg`Clear custom status`)}
									>
										<XIcon size={16} weight="bold" />
									</button>
								</FocusRing>
							) : null
						}
					/>
					<div className={styles.characterCount}>{statusText.length}/128</div>
				</div>
			</Modal.Content>

			<Modal.Footer className={styles.footer}>
				<div className={styles.expirationSelectWrapper}>
					<label className={styles.expirationLabel} htmlFor="custom-status-expiration">
						<Trans>Clear after</Trans>
					</label>

					<Select
						id="custom-status-expiration"
						className={styles.expirationSelect}
						options={selectOptions}
						value={selectedExpiration}
						onChange={handleExpirationChange}
						disabled={isSaving}
						renderOption={(option) => formatLabelWithRelative(option.label, expirationLabelMap[option.value])}
						renderValue={(option) =>
							option ? formatLabelWithRelative(option.label, expirationLabelMap[option.value]) : null
						}
					/>
				</div>

				<Button variant="primary" onClick={handleSave} submitting={isSaving}>
					<Trans>Save</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});

CustomStatusModal.displayName = 'CustomStatusModal';
