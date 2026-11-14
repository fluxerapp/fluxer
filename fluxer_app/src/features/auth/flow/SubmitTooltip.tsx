// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/flow/SubmitTooltip.module.css';
import {formatList} from '@app/features/i18n/utils/IntlCache';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type {ReactNode} from 'react';

const PLEASE_FILL_OUT_THE_FOLLOWING_FIELDS_DESCRIPTOR = msg({
	message: 'Fill out the following fields: {fieldList}',
	comment:
		'Short label in the authentication submit tooltip. Preserve {fieldList}; it is inserted by code. Keep the tone plain and specific.',
});

export interface MissingField {
	key: string;
	label: string;
}

export interface SubmitTooltipProps {
	children: ReactNode;
	missingFields?: Array<MissingField>;
}

const getMissingFieldsDescriptor = (fieldList: string): MessageDescriptor => ({
	...PLEASE_FILL_OUT_THE_FOLLOWING_FIELDS_DESCRIPTOR,
	values: {fieldList},
});

function getTooltipContentDescriptor(missingFields: Array<MissingField>, locale: string): MessageDescriptor | null {
	if (missingFields.length > 0) {
		const fieldList = formatList(
			missingFields.map((f) => f.label),
			{locale, style: 'long', type: 'conjunction'},
		);
		return getMissingFieldsDescriptor(fieldList);
	}
	return null;
}

export function shouldDisableSubmit(missingFields: Array<MissingField>): boolean {
	return missingFields.length > 0;
}

export function SubmitTooltip({children, missingFields = []}: SubmitTooltipProps) {
	const {i18n} = useLingui();
	const tooltipContentDescriptor = getTooltipContentDescriptor(missingFields, i18n.locale);
	const tooltipContent = tooltipContentDescriptor ? i18n._(tooltipContentDescriptor) : null;
	if (!tooltipContent) {
		return <>{children}</>;
	}
	return (
		<Tooltip text={tooltipContent} position="top" data-flx="auth.flow.submit-tooltip.tooltip">
			<div className={styles.buttonWrapper} data-flx="auth.flow.submit-tooltip.button-wrapper">
				{children}
			</div>
		</Tooltip>
	);
}
