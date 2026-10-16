// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/flow/AuthFloatingBackButton.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {ArrowLeftIcon} from '@phosphor-icons/react';
import type React from 'react';

interface AuthFloatingBackButtonProps {
	readonly ariaLabel: string;
	readonly onBack: () => void;
}

export function AuthFloatingBackButton({ariaLabel, onBack}: AuthFloatingBackButtonProps): React.ReactElement {
	return (
		<FocusRing offset={-2} data-flx="auth.flow.auth-floating-back-button.focus-ring">
			<button
				type="button"
				className={styles.floatingBackButton}
				aria-label={ariaLabel}
				onClick={onBack}
				data-flx="auth.flow.auth-floating-back-button.button.back"
			>
				<ArrowLeftIcon
					size={remFromPx(18)}
					weight="bold"
					data-flx="auth.flow.auth-floating-back-button.arrow-left-icon"
				/>
			</button>
		</FocusRing>
	);
}
