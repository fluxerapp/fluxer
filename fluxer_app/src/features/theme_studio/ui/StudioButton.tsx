// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/theme_studio/ui/StudioButton.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {clsx} from 'clsx';
import {type ButtonHTMLAttributes, forwardRef, type ReactNode} from 'react';

type StudioButtonVariant = 'primary' | 'secondary' | 'ghost' | 'danger' | 'dangerSolid' | 'successSolid';

interface StudioButtonProps extends ButtonHTMLAttributes<HTMLButtonElement> {
	variant?: StudioButtonVariant;
	compact?: boolean;
	fullWidth?: boolean;
	iconOnly?: boolean;
	leadingIcon?: ReactNode;
	trailingIcon?: ReactNode;
}

export const StudioButton = forwardRef<HTMLButtonElement, StudioButtonProps>(function StudioButton(
	{
		variant = 'ghost',
		compact = false,
		fullWidth = false,
		iconOnly = false,
		leadingIcon,
		trailingIcon,
		className,
		children,
		type = 'button',
		...rest
	},
	ref,
) {
	return (
		<FocusRing offset={-2} data-flx="theme-studio.ui.studio-button.focus-ring">
			<button
				ref={ref}
				type={type}
				className={clsx(
					styles.button,
					styles[variant],
					compact && styles.compact,
					fullWidth && styles.fullWidth,
					iconOnly && styles.iconOnly,
					className,
				)}
				data-flx="theme-studio.ui.studio-button.button"
				{...rest}
			>
				{leadingIcon ? (
					<span className={styles.leadingIcon} data-flx="theme-studio.ui.studio-button.leading-icon">
						{leadingIcon}
					</span>
				) : null}
				{children !== undefined && children !== null && children !== false ? (
					<span className={styles.label} data-flx="theme-studio.ui.studio-button.label">
						{children}
					</span>
				) : null}
				{trailingIcon ? (
					<span className={styles.trailingIcon} data-flx="theme-studio.ui.studio-button.trailing-icon">
						{trailingIcon}
					</span>
				) : null}
			</button>
		</FocusRing>
	);
});
