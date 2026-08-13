// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {CaretRightIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import type {ReactNode} from 'react';
import {useCallback, useId} from 'react';
import styles from './StudioSection.module.css';

interface StudioSectionProps {
	title: ReactNode;
	icon?: ReactNode;
	count?: number;
	open: boolean;
	onToggle: (next: boolean) => void;
	className?: string;
	children?: ReactNode;
}

export const StudioSection: React.FC<StudioSectionProps> = ({
	title,
	icon,
	count,
	open,
	onToggle,
	className,
	children,
}) => {
	const id = useId();
	const handleToggle = useCallback(() => onToggle(!open), [open, onToggle]);
	return (
		<div className={clsx(styles.section, className)} data-flx="theme-studio.ui.studio-section.section">
			<FocusRing offset={-2} data-flx="theme-studio.ui.studio-section.focus-ring">
				<button
					type="button"
					className={styles.trigger}
					aria-expanded={open}
					aria-controls={id}
					onClick={handleToggle}
					data-flx="theme-studio.ui.studio-section.trigger.toggle.button"
				>
					<span
						className={clsx(styles.chevron, open && styles.chevronOpen)}
						data-flx="theme-studio.ui.studio-section.chevron"
					>
						<CaretRightIcon
							size={remFromPx(12)}
							weight="bold"
							data-flx="theme-studio.ui.studio-section.caret-right-icon"
						/>
					</span>
					{icon ? (
						<span className={styles.icon} data-flx="theme-studio.ui.studio-section.icon">
							{icon}
						</span>
					) : null}
					<span className={styles.title} data-flx="theme-studio.ui.studio-section.title">
						{title}
					</span>
					{count !== undefined ? (
						<span className={styles.count} data-flx="theme-studio.ui.studio-section.count">
							{count}
						</span>
					) : null}
				</button>
			</FocusRing>
			{open ? (
				<div id={id} className={styles.body} data-flx="theme-studio.ui.studio-section.body">
					{children}
				</div>
			) : null}
		</div>
	);
};
