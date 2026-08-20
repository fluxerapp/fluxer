// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/shared/SpoilerOverlay.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import type {FC, ReactNode} from 'react';

const REVEAL_SPOILER_DESCRIPTOR = msg({
	message: 'Reveal spoiler',
	comment: 'Short label in the shared app spoiler overlay.',
});
const SPOILER_DESCRIPTOR = msg({
	message: 'Spoiler',
	comment: 'Short label in the shared app spoiler overlay.',
});

interface SpoilerOverlayProps {
	hidden: boolean;
	onReveal: () => void;
	children: ReactNode;
	label?: string;
	inline?: boolean;
	className?: string;
}

export const SpoilerOverlay: FC<SpoilerOverlayProps> = ({hidden, onReveal, children, label, inline, className}) => {
	const {i18n} = useLingui();
	const ariaLabel = label ?? i18n._(REVEAL_SPOILER_DESCRIPTOR);
	return (
		<div
			className={clsx(styles.container, inline && styles.inline, hidden && styles.hidden, className)}
			data-flx="app.spoiler-overlay.container"
		>
			<div className={styles.content} aria-hidden={hidden} data-flx="app.spoiler-overlay.content">
				{children}
			</div>
			{hidden && (
				<FocusRing offset={-2} data-flx="app.spoiler-overlay.focus-ring">
					<button
						type="button"
						className={styles.overlayButton}
						onClick={onReveal}
						aria-label={ariaLabel}
						data-flx="app.spoiler-overlay.overlay-button.reveal"
					>
						<span className={styles.overlayLabel} data-flx="app.spoiler-overlay.overlay-label">
							{label ?? i18n._(SPOILER_DESCRIPTOR)}
						</span>
					</button>
				</FocusRing>
			)}
		</div>
	);
};
