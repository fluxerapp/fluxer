/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type {ReactNode} from 'react';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import styles from './SubmitTooltip.module.css';

export interface MissingField {
	key: string;
	label: string;
}

export interface SubmitTooltipProps {
	children: ReactNode;
	consent: boolean;
	missingFields?: Array<MissingField>;
}

const CONSENT_REQUIRED_DESCRIPTOR = msg`You must agree to the Terms of Service and Privacy Policy to create an account`;
const getMissingFieldsDescriptor = (fieldList: string): MessageDescriptor =>
	msg`Please fill out the following fields: ${fieldList}`;

function getTooltipContentDescriptor(consent: boolean, missingFields: Array<MissingField>): MessageDescriptor | null {
	if (!consent) {
		return CONSENT_REQUIRED_DESCRIPTOR;
	}

	if (missingFields.length > 0) {
		const fieldList = missingFields.map((f) => f.label).join(', ');
		return getMissingFieldsDescriptor(fieldList);
	}

	return null;
}

export function shouldDisableSubmit(consent: boolean, missingFields: Array<MissingField>): boolean {
	return !consent || missingFields.length > 0;
}

export function SubmitTooltip({children, consent, missingFields = []}: SubmitTooltipProps) {
	const {t} = useLingui();
	const tooltipContentDescriptor = getTooltipContentDescriptor(consent, missingFields);
	const tooltipContent = tooltipContentDescriptor ? t(tooltipContentDescriptor) : null;

	if (!tooltipContent) {
		return <>{children}</>;
	}

	return (
		<Tooltip text={tooltipContent} position="top">
			<div className={styles.buttonWrapper}>{children}</div>
		</Tooltip>
	);
}
