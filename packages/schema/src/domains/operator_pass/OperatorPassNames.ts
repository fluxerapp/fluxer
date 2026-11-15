// SPDX-License-Identifier: AGPL-3.0-or-later

import {segmentOperatorPassText} from '@fluxer/schema/src/domains/operator_pass/OperatorPassGraphemes';

export const OPERATOR_PASS_WALL_NAME_MAX_LENGTH = 32;

export const OPERATOR_PASS_CERTIFICATE_NAME_MAX_LENGTH = 48;

export const OPERATOR_PASS_CERTIFICATE_NAME_MAX_CODE_UNITS = OPERATOR_PASS_CERTIFICATE_NAME_MAX_LENGTH * 8;

export function countOperatorPassCertificateNameCharacters(name: string): number {
	let count = 0;
	for (const _ of segmentOperatorPassText(name)) {
		count++;
	}
	return count;
}

export function truncateOperatorPassCertificateName(name: string): string {
	let end = 0;
	let count = 0;
	for (const {segment} of segmentOperatorPassText(name)) {
		if (count === OPERATOR_PASS_CERTIFICATE_NAME_MAX_LENGTH) break;
		end += segment.length;
		count++;
	}
	return name.slice(0, end);
}
