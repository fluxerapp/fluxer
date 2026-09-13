// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/flow/SubmitTooltip.module.css';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {formatListWithConfig} from '@pkgs/list_utils/src/ListFormatting';
import type {ReactNode} from 'react';

const YOU_MUST_AGREE_TO_THE_TERMS_OF_SERVICE_DESCRIPTOR = msg({
	message: 'You must agree to the terms of service and privacy policy to create an account',
	comment: 'Registration submit-button tooltip explaining that ToS and privacy agreement is required to proceed.',
});
const YOU_MUST_AGREE_TO_THE_TERMS_DESCRIPTOR = msg({
	message: 'You must agree to the terms of service to create an account',
	comment: 'Registration submit-button tooltip explaining that terms agreement is required to proceed.',
});
const YOU_MUST_AGREE_TO_THE_PRIVACY_POLICY_DESCRIPTOR = msg({
	message: 'You must agree to the privacy policy to create an account',
	comment: 'Registration submit-button tooltip explaining that privacy policy agreement is required to proceed.',
});
const PLEASE_FILL_OUT_THE_FOLLOWING_FIELDS_DESCRIPTOR = msg({
	message: 'Fill out the following fields: {fieldList}',
	comment:
		'Short label in the authentication submit tooltip. Preserve {fieldList}; it is inserted by code. Keep the tone plain and specific.',
});

export interface MissingField {
	key: string;
	label: string;
}

export type LegalConsentRequirement = 'terms' | 'privacy' | 'terms_and_privacy';

export interface SubmitTooltipProps {
	children: ReactNode;
	consent: boolean;
	legalConsentRequirement?: LegalConsentRequirement;
	missingFields?: Array<MissingField>;
}

function getConsentRequiredDescriptor(requirement: LegalConsentRequirement): MessageDescriptor {
	if (requirement === 'terms') return YOU_MUST_AGREE_TO_THE_TERMS_DESCRIPTOR;
	if (requirement === 'privacy') return YOU_MUST_AGREE_TO_THE_PRIVACY_POLICY_DESCRIPTOR;
	return YOU_MUST_AGREE_TO_THE_TERMS_OF_SERVICE_DESCRIPTOR;
}
const getMissingFieldsDescriptor = (fieldList: string): MessageDescriptor => ({
	...PLEASE_FILL_OUT_THE_FOLLOWING_FIELDS_DESCRIPTOR,
	values: {fieldList},
});

function getTooltipContentDescriptor(
	consent: boolean,
	missingFields: Array<MissingField>,
	legalConsentRequirement: LegalConsentRequirement,
	locale: string,
): MessageDescriptor | null {
	if (!consent) {
		return getConsentRequiredDescriptor(legalConsentRequirement);
	}
	if (missingFields.length > 0) {
		const fieldList = formatListWithConfig(
			missingFields.map((f) => f.label),
			{locale, style: 'long', type: 'conjunction'},
		);
		return getMissingFieldsDescriptor(fieldList);
	}
	return null;
}

export function shouldDisableSubmit(consent: boolean, missingFields: Array<MissingField>): boolean {
	return !consent || missingFields.length > 0;
}

export function SubmitTooltip({
	children,
	consent,
	legalConsentRequirement = 'terms_and_privacy',
	missingFields = [],
}: SubmitTooltipProps) {
	const {i18n} = useLingui();
	const tooltipContentDescriptor = getTooltipContentDescriptor(
		consent,
		missingFields,
		legalConsentRequirement,
		i18n.locale,
	);
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
