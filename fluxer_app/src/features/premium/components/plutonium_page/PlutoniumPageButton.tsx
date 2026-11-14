// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/premium/components/plutonium_page/PlutoniumPageButton.module.css';
import {PlutoniumPageIcon} from '@app/features/premium/components/plutonium_page/PlutoniumPageIcons';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {clsx} from 'clsx';
import type React from 'react';

type PlutoniumPageButtonVariant = 'primary' | 'secondary';

interface PlutoniumPageButtonProps {
	variant: PlutoniumPageButtonVariant;
	onClick: () => void;
	disabled?: boolean;
	loading?: boolean;
	directional?: boolean;
	children: React.ReactNode;
	'data-flx'?: string;
}

const VARIANT_CLASS: Record<PlutoniumPageButtonVariant, string | undefined> = {
	primary: styles.primary,
	secondary: undefined,
};

export function PlutoniumPageButton({
	variant,
	onClick,
	disabled,
	loading,
	directional,
	children,
	'data-flx': flx,
}: PlutoniumPageButtonProps) {
	return (
		<FocusRing offset={-4} data-flx="premium.plutonium-page.button.focus-ring">
			<button
				type="button"
				className={clsx(styles.button, VARIANT_CLASS[variant])}
				onClick={() => {
					if (!loading) onClick();
				}}
				disabled={disabled}
				aria-busy={loading || undefined}
				aria-disabled={loading || undefined}
				data-flx={flx}
			>
				<span className={styles.label} data-flx="premium.plutonium-page.button.label">
					{children}
				</span>
				{loading ? (
					<span className={styles.spinner} aria-hidden="true" data-flx="premium.plutonium-page.button.spinner" />
				) : (
					directional && (
						<PlutoniumPageIcon
							name="arrowRight"
							className={styles.icon}
							data-flx="premium.plutonium-page.button.icon"
						/>
					)
				)}
			</button>
		</FocusRing>
	);
}
