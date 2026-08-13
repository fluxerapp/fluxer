// SPDX-License-Identifier: AGPL-3.0-or-later

import {EVERYONE_MENTION, HERE_MENTION} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/channel/components/MentionEveryonePopout.module.css';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import type {I18n} from '@lingui/core';
import {msg, ph} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {WarningIcon} from '@phosphor-icons/react';
import {formatNumber} from '@pkgs/number_utils/src/NumberFormatting';
import {useCallback, useEffect} from 'react';

const THIS_ROLE_DESCRIPTOR = msg({
	message: 'this role',
	comment: 'Fallback role name in a mass-mention warning title.',
});
const MENTIONED_ROLE_DESCRIPTOR = msg({
	message: 'mentioned role',
	comment: 'Fallback role name in a mass-mention warning description.',
});
const ENTER_DESCRIPTOR = msg({
	message: 'Enter',
	comment: 'Keyboard key label for confirming a mass-mention warning.',
});
const ESC_DESCRIPTOR = msg({
	message: 'Esc',
	comment: 'Keyboard key label for canceling a mass-mention warning.',
});

interface MentionEveryonePopoutProps {
	mentionType: '@everyone' | '@here' | 'role';
	memberCount: number;
	onConfirm: () => void;
	onCancel: () => void;
	roleName?: string;
}

const isMac = () => /Mac|iPod|iPhone|iPad/.test(navigator.platform);
export const getMentionTitle = (
	mentionType: MentionEveryonePopoutProps['mentionType'],
	roleName?: string,
	i18n?: I18n,
) => {
	if (mentionType === 'role') {
		const roleLabel = roleName ?? i18n?._(THIS_ROLE_DESCRIPTOR) ?? 'this role';
		return (
			<Trans comment="Warning dialog title before sending a message that mentions every member with a role.">
				Mention {ph({roleLabel})}?
			</Trans>
		);
	}
	if (mentionType === '@everyone') {
		return (
			<Trans comment="Warning dialog title before sending an @everyone mention.">
				Mention {ph({mention: EVERYONE_MENTION})}?
			</Trans>
		);
	}
	return (
		<Trans comment="Warning dialog title before sending an @here mention.">
			Mention {ph({mention: HERE_MENTION})}?
		</Trans>
	);
};
export const getMentionDescription = (
	mentionType: MentionEveryonePopoutProps['mentionType'],
	memberCount: number,
	roleName?: string,
	i18n?: I18n,
) => {
	if (mentionType === 'role') {
		const memberCountLabel = formatNumber(memberCount, getCurrentLocale());
		const roleLabel = roleName ?? i18n?._(MENTIONED_ROLE_DESCRIPTOR) ?? 'mentioned role';
		return (
			<Trans comment="Warning text before notifying every member with a role in the current channel.">
				This will notify{' '}
				<strong data-flx="channel.mention-everyone-popout.get-mention-description.strong">
					{ph({memberCount: memberCountLabel})}
				</strong>{' '}
				members with the{' '}
				<span className={styles.roleName} data-flx="channel.mention-everyone-popout.get-mention-description.role-name">
					{ph({roleLabel})}
				</span>{' '}
				in this channel. Are you sure you want to do this?
			</Trans>
		);
	}
	if (mentionType === '@everyone') {
		const memberCountLabel = formatNumber(memberCount, getCurrentLocale());
		return (
			<Trans comment="Warning text before notifying every member in the current channel.">
				This will notify{' '}
				<strong data-flx="channel.mention-everyone-popout.get-mention-description.strong--2">
					{ph({memberCount: memberCountLabel})}
				</strong>{' '}
				members in this channel. Are you sure you want to do this?
			</Trans>
		);
	}
	const memberCountLabel = formatNumber(memberCount, getCurrentLocale());
	return (
		<Trans comment="Warning text before notifying online members in the current channel with @here.">
			This will notify up to{' '}
			<strong data-flx="channel.mention-everyone-popout.get-mention-description.strong--3">
				{ph({memberCount: memberCountLabel})}
			</strong>{' '}
			online members in this channel. Are you sure you want to do this?
		</Trans>
	);
};
export const MentionEveryonePopout = ({
	mentionType,
	memberCount,
	onConfirm,
	onCancel,
	roleName,
}: MentionEveryonePopoutProps) => {
	const {i18n} = useLingui();
	const handleKeyDown = useCallback(
		(event: KeyboardEvent) => {
			if (event.key === 'Escape') {
				event.preventDefault();
				event.stopImmediatePropagation();
				onCancel();
				return;
			}
			if (event.key === 'Enter') {
				if (isIMEComposing(event)) {
					return;
				}
				event.preventDefault();
				event.stopImmediatePropagation();
				onConfirm();
			}
		},
		[onCancel, onConfirm],
	);
	useEffect(() => {
		document.addEventListener('keydown', handleKeyDown, true);
		return () => document.removeEventListener('keydown', handleKeyDown, true);
	}, [handleKeyDown]);
	const enterKeySymbol = isMac() ? '↵' : i18n._(ENTER_DESCRIPTOR);
	const escapeKeyLabel = i18n._(ESC_DESCRIPTOR);
	return (
		<div
			className={styles.container}
			role="dialog"
			aria-modal="true"
			data-flx="channel.mention-everyone-popout.container"
		>
			<div className={styles.header} data-flx="channel.mention-everyone-popout.header">
				<WarningIcon
					size={remFromPx(20)}
					weight="fill"
					className={styles.warningIcon}
					data-flx="channel.mention-everyone-popout.warning-icon"
				/>
				<span className={styles.title} data-flx="channel.mention-everyone-popout.title">
					{getMentionTitle(mentionType, roleName, i18n)}
				</span>
			</div>
			<p className={styles.description} data-flx="channel.mention-everyone-popout.description">
				{getMentionDescription(mentionType, memberCount, roleName, i18n)}
			</p>
			<div className={styles.keybinds} data-flx="channel.mention-everyone-popout.keybinds">
				<div className={styles.keybind} data-flx="channel.mention-everyone-popout.keybind">
					<kbd className={styles.keybindHint} data-flx="channel.mention-everyone-popout.keybind-hint">
						{escapeKeyLabel}
					</kbd>
					<span data-flx="channel.mention-everyone-popout.span">
						<Trans comment="Action label for dismissing a mass-mention warning.">Cancel</Trans>
					</span>
				</div>
				<div className={styles.keybind} data-flx="channel.mention-everyone-popout.keybind--2">
					<kbd className={styles.keybindHint} data-flx="channel.mention-everyone-popout.keybind-hint--2">
						{enterKeySymbol}
					</kbd>
					<span data-flx="channel.mention-everyone-popout.span--2">
						<Trans comment="Action label for confirming a mass mention that may notify many people.">Confirm</Trans>
					</span>
				</div>
			</div>
		</div>
	);
};
