// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/lexical/composer/nodes/ComposerInline.module.css';
import type {JSX, ReactNode} from 'react';

interface ComposerAtomicPresentationProps {
	spoiler: boolean;
	children: ReactNode;
}

export function ComposerAtomicPresentation({spoiler, children}: ComposerAtomicPresentationProps): JSX.Element {
	if (!spoiler) {
		return <>{children}</>;
	}
	return <span className={styles.spoiler}>{children}</span>;
}
