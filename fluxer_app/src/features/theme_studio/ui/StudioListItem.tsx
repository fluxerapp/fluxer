// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/theme_studio/ui/StudioListItem.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {clsx} from 'clsx';
import type React from 'react';
import type {ReactNode} from 'react';

interface StudioListItemProps {
	leading?: ReactNode;
	label: ReactNode;
	description?: ReactNode;
	trailing?: ReactNode;
	active?: boolean;
	onClick?: () => void;
	className?: string;
	codeBody?: ReactNode;
}

export const StudioListItem: React.FC<StudioListItemProps> = ({
	leading,
	label,
	description,
	trailing,
	active,
	onClick,
	className,
	codeBody,
}) => {
	const interactive = onClick !== undefined;
	const body = (
		<>
			{leading ? (
				<span className={styles.leading} data-flx="theme-studio.ui.studio-list-item.leading">
					{leading}
				</span>
			) : null}
			<span className={styles.text} data-flx="theme-studio.ui.studio-list-item.text">
				<span className={styles.label} data-flx="theme-studio.ui.studio-list-item.label">
					{label}
				</span>
				{description ? (
					<span className={styles.description} data-flx="theme-studio.ui.studio-list-item.description">
						{description}
					</span>
				) : null}
				{codeBody ? (
					<code className={styles.code} data-flx="theme-studio.ui.studio-list-item.code">
						{codeBody}
					</code>
				) : null}
			</span>
			{trailing ? (
				<span className={styles.trailing} data-flx="theme-studio.ui.studio-list-item.trailing">
					{trailing}
				</span>
			) : null}
		</>
	);
	if (interactive) {
		return (
			<FocusRing offset={-2} data-flx="theme-studio.ui.studio-list-item.focus-ring">
				<button
					type="button"
					className={clsx(styles.item, active && styles.active, className)}
					onClick={onClick}
					data-flx="theme-studio.ui.studio-list-item.item.click.button"
				>
					{body}
				</button>
			</FocusRing>
		);
	}
	return (
		<div
			className={clsx(styles.item, styles.staticItem, active && styles.active, className)}
			data-flx="theme-studio.ui.studio-list-item.item"
		>
			{body}
		</div>
	);
};
export const StudioListDivider: React.FC = () => (
	<div className={styles.divider} data-flx="theme-studio.ui.studio-list-item.studio-list-divider.divider" />
);
