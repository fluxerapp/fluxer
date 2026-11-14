// SPDX-License-Identifier: AGPL-3.0-or-later

import {getTimeWindowPresets, TIME_WINDOW_LABEL_MESSAGES} from '@app/features/app/config/TimeWindowPresets';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import DeveloperMode from '@app/features/devtools/state/DeveloperMode';
import Emoji from '@app/features/emoji/state/Emoji';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {buildCustomEmojiURL} from '@app/features/expressions/utils/CustomEmojiImageUrl';
import {getEmojiURL} from '@app/features/expressions/utils/EmojiUtils';
import {getSkinTonedSurrogate} from '@app/features/expressions/utils/SkinToneUtils';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import {buildDraftStatus, type CustomStatus} from '@app/features/user/state/CustomStatus';
import {handleAccountLimitedError} from '@app/features/user/utils/AccountLimitUtils';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, useState} from 'react';

interface CustomStatusDraftOptions {
	initialStatus: CustomStatus | null;
	onSaved: () => void;
	getExpiresAt: () => string | null;
}
export function useCustomStatusDraft({initialStatus, onSaved, getExpiresAt}: CustomStatusDraftOptions) {
	const {i18n} = useLingui();
	const isDeveloper = DeveloperMode.isDeveloper;
	const [statusText, setStatusText] = useState(initialStatus?.text ?? '');
	const [emojiId, setEmojiId] = useState<string | null>(initialStatus?.emojiId ?? null);
	const [emojiName, setEmojiName] = useState<string | null>(initialStatus?.emojiName ?? null);
	const [isSaving, setIsSaving] = useState(false);
	const expirationPresets = useMemo(
		() =>
			getTimeWindowPresets({includeDeveloperOptions: isDeveloper}).map((preset) => ({
				key: preset.key,
				label: i18n._(TIME_WINDOW_LABEL_MESSAGES[preset.key]),
				minutes: preset.minutes,
			})),
		[i18n.locale, isDeveloper],
	);
	const draftStatus = useMemo(
		() => buildDraftStatus({text: statusText.trim(), emojiId, emojiName, expiresAt: null}),
		[statusText, emojiId, emojiName],
	);
	const resetDraft = useCallback((status: CustomStatus | null) => {
		setStatusText(status?.text ?? '');
		setEmojiId(status?.emojiId ?? null);
		setEmojiName(status?.emojiName ?? null);
	}, []);
	const handleEmojiSelect = useCallback((emoji: FlatEmoji) => {
		if (emoji.id) {
			setEmojiId(emoji.id);
			setEmojiName(emoji.name);
		} else {
			setEmojiId(null);
			setEmojiName(getSkinTonedSurrogate(emoji));
		}
	}, []);
	const handleClearDraft = () => resetDraft(null);
	const handleSave = async () => {
		if (isSaving) return;
		setIsSaving(true);
		try {
			const statusToSave = buildDraftStatus({text: statusText.trim(), emojiId, emojiName, expiresAt: getExpiresAt()});
			await UserSettingsCommands.update({customStatus: statusToSave});
			onSaved();
		} catch (error) {
			if (!handleAccountLimitedError(error)) throw error;
		} finally {
			setIsSaving(false);
		}
	};
	return {
		statusText,
		setStatusText,
		isSaving,
		draftStatus,
		handleEmojiSelect,
		handleClearDraft,
		handleSave,
		resetDraft,
		expirationPresets,
	};
}
export const CustomStatusEmojiPreview = observer(
	({status, className}: {status: CustomStatus | null; className: string}) => {
		const shouldAnimate = useShouldAnimate({kind: 'emoji'});
		if (!status) return null;
		if (status.emojiId) {
			const emoji = Emoji.getEmojiById(status.emojiId);
			if (emoji)
				return (
					<img
						src={buildCustomEmojiURL({id: status.emojiId, animated: Boolean(emoji.animated) && shouldAnimate})}
						alt={emoji.name}
						className={className}
						data-flx="user.custom-status-emoji-preview.custom"
					/>
				);
		}
		if (status.emojiName) {
			const url = getEmojiURL(status.emojiName);
			if (url)
				return (
					<img
						src={url}
						alt={status.emojiName}
						className={className}
						data-flx="user.custom-status-emoji-preview.unicode"
					/>
				);
		}
		return null;
	},
);

export function hasCustomStatusEmojiPreview(status: CustomStatus | null): boolean {
	return Boolean(
		status &&
			((status.emojiId && Emoji.getEmojiById(status.emojiId)) || (status.emojiName && getEmojiURL(status.emojiName))),
	);
}
