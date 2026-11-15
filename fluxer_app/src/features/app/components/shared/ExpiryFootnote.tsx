// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/shared/ExpiryFootnote.module.css';
import {ExpiryFootnoteContextMenu} from '@app/features/app/components/shared/ExpiryFootnoteContextMenu';
import {HelpCenterArticleSlug} from '@app/features/app/config/HelpCenterConstants';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import * as HelpCenterUtils from '@app/features/ui/utils/HelpCenterUtils';
import {getFormattedShortDate} from '@app/features/user/utils/DateFormatting';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import {type FC, type MouseEvent, useCallback} from 'react';

const EXPIRED_ON_DESCRIPTOR = msg({
	message: 'Expired on {date}',
	comment: 'Short label in the shared app expiry footnote. Preserve {date}; it is inserted by code.',
});
const EXPIRES_ON_DESCRIPTOR = msg({
	message: 'Expires on {date}',
	comment: 'Short label in the shared app expiry footnote. Preserve {date}; it is inserted by code.',
});

interface ExpiryFootnoteProps {
	expiresAt: Date | null;
	isExpired: boolean;
	label?: string;
	className?: string;
	inline?: boolean;
}

export const ExpiryFootnote: FC<ExpiryFootnoteProps> = ({expiresAt, isExpired, label, className, inline = false}) => {
	const {i18n} = useLingui();
	const helpUrl = HelpCenterUtils.getURL(HelpCenterArticleSlug.AttachmentExpiry);
	const handleContextMenu = useCallback((event: MouseEvent<HTMLAnchorElement>) => {
		ContextMenuCommands.openFromEvent(event, () => (
			<ExpiryFootnoteContextMenu data-flx="app.expiry-footnote.handle-context-menu.expiry-footnote-context-menu" />
		));
	}, []);
	let resolved = label;
	if (!resolved) {
		if (expiresAt) {
			const date = getFormattedShortDate(expiresAt);
			resolved = isExpired ? i18n._(EXPIRED_ON_DESCRIPTOR, {date}) : i18n._(EXPIRES_ON_DESCRIPTOR, {date});
		} else {
			return null;
		}
	}
	const footnoteClassName = clsx(inline ? styles.inlineFootnote : styles.footnote, className);
	if (!helpUrl) {
		return (
			<span
				className={footnoteClassName}
				data-message-copy-hidden="true"
				data-flx="app.expiry-footnote.inline-footnote.text"
			>
				{resolved}
			</span>
		);
	}
	return (
		<FocusRing data-flx="app.expiry-footnote.focus-ring">
			<a
				className={footnoteClassName}
				href={helpUrl}
				onContextMenu={handleContextMenu}
				target="_blank"
				rel="noreferrer"
				data-message-copy-hidden="true"
				data-flx="app.expiry-footnote.inline-footnote.context-menu"
			>
				{resolved}
			</a>
		</FocusRing>
	);
};
