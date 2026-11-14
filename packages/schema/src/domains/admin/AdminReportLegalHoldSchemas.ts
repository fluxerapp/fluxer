// SPDX-License-Identifier: AGPL-3.0-or-later

import {IsoTimestampStringType} from '@fluxer/schema/src/primitives/DateValidators';
import {createStringType, SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const ReportLegalHoldRequest = z.object({
	legal_hold_until: IsoTimestampStringType.nullable().describe(
		'When the hold ends, as an ISO 8601 timestamp in the future. Null clears the hold',
	),
	legal_hold_reason: createStringType(1, 512)
		.nullable()
		.optional()
		.describe('Why the report is kept past its retention period. Required to place a hold, ignored when clearing one'),
});

export type ReportLegalHoldRequest = z.infer<typeof ReportLegalHoldRequest>;

export const ReportLegalHoldResponse = z.object({
	report_id: SnowflakeStringType,
	legal_hold_until: z.string().nullable().describe('When the hold ends. Null when the report has no hold'),
	legal_hold_reason: z.string().nullable().describe('Why the report is held. Null when the report has no hold'),
});

export type ReportLegalHoldResponse = z.infer<typeof ReportLegalHoldResponse>;
