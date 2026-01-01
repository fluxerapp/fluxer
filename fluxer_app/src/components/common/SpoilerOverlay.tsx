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

import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import type {FC, ReactNode} from 'react';
import styles from './SpoilerOverlay.module.css';

interface SpoilerOverlayProps {
	hidden: boolean;
	onReveal: () => void;
	children: ReactNode;
	label?: string;
	inline?: boolean;
	className?: string;
}

export const SpoilerOverlay: FC<SpoilerOverlayProps> = ({hidden, onReveal, children, label, inline, className}) => {
	const {t} = useLingui();
	const ariaLabel = label ?? t`Reveal spoiler`;

	return (
		<div className={clsx(styles.container, inline && styles.inline, hidden && styles.hidden, className)}>
			<div className={styles.content} aria-hidden={hidden}>
				{children}
			</div>
			{hidden && (
				<button type="button" className={styles.overlayButton} onClick={onReveal} aria-label={ariaLabel}>
					<span className={styles.overlayLabel}>{label ?? t`Spoiler`}</span>
				</button>
			)}
		</div>
	);
};
