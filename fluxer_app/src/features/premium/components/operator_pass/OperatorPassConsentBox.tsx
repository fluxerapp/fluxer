// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/premium/components/operator_pass/OperatorPassConsentBox.module.css';
import {OPERATOR_PASS_REQUIRED_BOX_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import type {ReactNode} from 'react';

interface OperatorPassConsentBoxProps {
	id: string;
	checked: boolean;
	missing: boolean;
	disabled: boolean;
	status?: ReactNode;
	onChange: (value: boolean) => void;
	children: ReactNode;
	'data-flx': string;
}

export function OperatorPassConsentBox({
	id,
	checked,
	missing,
	disabled,
	status,
	onChange,
	children,
	'data-flx': flx,
}: OperatorPassConsentBoxProps) {
	const {i18n} = useLingui();
	return (
		<div id={id} className={clsx(styles.box, missing && styles.boxError)} data-flx={`${flx}.box`}>
			<Checkbox
				className={styles.checkbox}
				checked={checked}
				onChange={onChange}
				disabled={disabled}
				aria-describedby={missing ? `${id}-error` : undefined}
				aria-invalid={missing || undefined}
				aria-required
				data-flx={`${flx}.checkbox`}
			>
				<span className={styles.label} data-flx={`${flx}.label`}>
					{children}
				</span>
			</Checkbox>
			{missing && (
				<p id={`${id}-error`} className={styles.error} data-flx={`${flx}.error`}>
					{i18n._(OPERATOR_PASS_REQUIRED_BOX_DESCRIPTOR)}
				</p>
			)}
			{status ? (
				<p role="status" className={styles.status} data-flx={`${flx}.status`}>
					{status}
				</p>
			) : null}
		</div>
	);
}
