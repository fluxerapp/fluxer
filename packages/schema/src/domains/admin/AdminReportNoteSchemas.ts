// SPDX-License-Identifier: AGPL-3.0-or-later

import {createStringType, SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const ReportNoteRequest = z.object({
	body: createStringType(1, 2000).describe('The internal note. Staff only'),
});

export type ReportNoteRequest = z.infer<typeof ReportNoteRequest>;

export const ReportNoteResponse = z.object({
	note_id: SnowflakeStringType,
	author_id: SnowflakeStringType,
	body: z.string(),
	created_at: z.string(),
});

export type ReportNoteResponse = z.infer<typeof ReportNoteResponse>;

export const ReportNoteListResponse = z.object({
	notes: z.array(ReportNoteResponse).describe('The notes on the report, oldest first'),
});

export type ReportNoteListResponse = z.infer<typeof ReportNoteListResponse>;
