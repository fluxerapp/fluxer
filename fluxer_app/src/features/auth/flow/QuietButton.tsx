// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import type {ReactNode} from 'react';

export function QuietButton({
	children,
	disabled,
	onClick,
	'data-flx': dataFlx,
}: {
	children: ReactNode;
	disabled?: boolean;
	onClick: () => void;
	'data-flx': string;
}) {
	return (
		<FocusRing offset={-2} data-flx={`${dataFlx}.focus-ring`}>
			<button type="button" className={styles.textButton} disabled={disabled} onClick={onClick} data-flx={dataFlx}>
				{children}
			</button>
		</FocusRing>
	);
}
