// SPDX-License-Identifier: AGPL-3.0-or-later

import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {TooltipWithKeybind} from '@app/features/ui/keybind_hint/KeybindHint';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/voice/components/CompactVoiceCallView.module.css';
import type {CompactVoiceCallHeightToggle} from '@app/features/voice/components/compact_voice_call_view/shared';
import voiceCallStyles from '@app/features/voice/components/VoiceCallView.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ChatTeardropIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useMemo} from 'react';

const SHOW_CHAT_DESCRIPTOR = msg({
	message: 'Show chat',
	comment:
		'Tooltip and aria label on the compact call-view chat pill button when the chat panel is currently hidden. Activating it shows chat alongside the call.',
});
const HIDE_CHAT_DESCRIPTOR = msg({
	message: 'Hide chat',
	comment:
		'Tooltip and aria label on the compact call-view chat pill button when the chat panel is currently visible. Activating it hides chat so the call view fills the space.',
});
const SHOW_CHAT_WITH_UNREAD_DESCRIPTOR = msg({
	message: 'Show chat with {count, plural, one {# unread message} other {# unread messages}}',
	comment:
		'Tooltip and aria label on the compact call-view chat pill button when chat is hidden and has unread messages. {count} is the unread message count.',
});

function useHeightToggleButtonConfig(heightToggle: CompactVoiceCallHeightToggle | undefined) {
	const {i18n} = useLingui();
	const heightToggleUnreadCount = Math.max(0, Math.floor(heightToggle?.unreadCount ?? 0));
	const isChatHidden = heightToggle?.isExpanded === true;
	const accessibleLabel = useMemo(() => {
		if (!heightToggle) return '';
		if (!isChatHidden) return i18n._(HIDE_CHAT_DESCRIPTOR);
		if (heightToggleUnreadCount > 0) {
			return i18n._(SHOW_CHAT_WITH_UNREAD_DESCRIPTOR, {count: heightToggleUnreadCount});
		}
		return i18n._(SHOW_CHAT_DESCRIPTOR);
	}, [heightToggle, heightToggleUnreadCount, i18n.locale, isChatHidden]);
	const unreadBadgeLabel = heightToggleUnreadCount > 9 ? `${i18n.number(9)}+` : i18n.number(heightToggleUnreadCount);
	return {heightToggleUnreadCount, unreadBadgeLabel, accessibleLabel, isChatHidden};
}

export const CompactCallHeightToggleButton: React.FC<{
	heightToggle?: CompactVoiceCallHeightToggle;
	callViewId: string;
}> = ({heightToggle, callViewId}) => {
	const {heightToggleUnreadCount, unreadBadgeLabel, accessibleLabel, isChatHidden} =
		useHeightToggleButtonConfig(heightToggle);
	if (!heightToggle) return null;
	return (
		<div
			className={clsx(styles.heightToggleButtonWrap, voiceCallStyles.voiceChrome)}
			data-flx="voice.compact-voice-call-view.compact-call-height-toggle-button.height-toggle-button-wrap"
		>
			<div
				className={styles.heightToggleButtonShell}
				data-flx="voice.compact-voice-call-view.compact-call-height-toggle-button.height-toggle-button-shell"
			>
				<Tooltip
					text={() => (
						<TooltipWithKeybind
							label={accessibleLabel}
							action="voice_toggle_compact_call_view"
							data-flx="voice.compact-voice-call-view.compact-call-height-toggle-button.tooltip-with-keybind"
						/>
					)}
					position="top"
					data-flx="voice.compact-voice-call-view.compact-call-height-toggle-button.tooltip"
				>
					<FocusRing offset={-2} data-flx="voice.compact-voice-call-view.compact-call-height-toggle-button.focus-ring">
						<button
							type="button"
							className={styles.heightTogglePill}
							aria-label={accessibleLabel}
							aria-expanded={!isChatHidden}
							aria-controls={callViewId}
							onClick={heightToggle.onToggle}
							data-flx="voice.compact-voice-call-view.compact-call-height-toggle-button.height-toggle-pill"
						>
							<ChatTeardropIcon
								weight="fill"
								className={styles.heightTogglePillIcon}
								aria-hidden="true"
								data-flx="voice.compact-voice-call-view.compact-call-height-toggle-button.chat-teardrop-icon"
							/>
						</button>
					</FocusRing>
				</Tooltip>
				{heightToggleUnreadCount > 0 && (
					<div
						className={styles.heightToggleUnreadBadge}
						aria-hidden="true"
						data-flx="voice.compact-voice-call-view.compact-call-height-toggle-button.height-toggle-unread-badge"
					>
						{unreadBadgeLabel}
					</div>
				)}
			</div>
		</div>
	);
};
