// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/ui/components/Spinner.module.css';
import {Trans} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

export const SpinnerSize = Object.freeze({
	SMALL: 'small',
	MEDIUM: 'medium',
	LARGE: 'large',
} as const);

export type SpinnerSize = (typeof SpinnerSize)[keyof typeof SpinnerSize];

interface SpinnerProps {
	className?: string;
	size?: SpinnerSize;
}

export const Spinner = observer(function Spinner({className, size = 'medium'}: SpinnerProps) {
	return (
		<span className={clsx(styles.spinner, className)} data-flx="ui.spinner.spinner">
			<span className={styles.spinnerInner} data-flx="ui.spinner.spinner-inner">
				<span className={clsx(styles.spinnerItem, styles[size])} data-flx="ui.spinner.spinner-item" />
				<span className={clsx(styles.spinnerItem, styles[size], styles.delay1)} data-flx="ui.spinner.spinner-item--2" />
				<span className={clsx(styles.spinnerItem, styles[size], styles.delay2)} data-flx="ui.spinner.spinner-item--3" />
			</span>
			<span className={styles.srOnly} data-flx="ui.spinner.sr-only">
				<Trans>Loading…</Trans>
			</span>
		</span>
	);
});
