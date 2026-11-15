// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/app/App.module.css';
import {useTabKeyFocusGuard} from '@app/features/app/hooks/useTabKeyFocusGuard';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';

const SKIP_TO_CONTENT_DESCRIPTOR = msg({
	message: 'Skip to content',
	comment: 'Accessible skip-link label for keyboard users.',
});

function focusMainContent(event: React.MouseEvent<HTMLButtonElement>): void {
	event.currentTarget.ownerDocument.getElementById('main-content')?.focus();
}

export function SkipToContentLink(): React.ReactElement {
	const {i18n} = useLingui();
	const handleSkipLinkFocus = useTabKeyFocusGuard();
	return (
		<button
			type="button"
			className={styles.skipLink}
			onFocus={handleSkipLinkFocus}
			onClick={focusMainContent}
			data-flx="app.app.app-wrapper.skip-link"
		>
			{i18n._(SKIP_TO_CONTENT_DESCRIPTOR)}
		</button>
	);
}
