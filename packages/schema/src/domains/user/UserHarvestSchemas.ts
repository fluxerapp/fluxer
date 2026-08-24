// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createNamedStringLiteralUnion,
	createStringType,
	SnowflakeStringType,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const HarvestStatusEnum = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['pending', 'pending', 'The harvest job is waiting to be processed'],
			['processing', 'processing', 'The harvest job is currently being processed'],
			['completed', 'completed', 'The harvest job has finished successfully'],
			['failed', 'failed', 'The harvest job encountered an error and could not complete'],
		],
		'Current status of the harvest request',
	),
	'HarvestStatus',
);
export const HarvestCreationResponseSchema = z.object({
	harvest_id: SnowflakeStringType.describe('Unique identifier for the harvest request'),
	status: HarvestStatusEnum,
	created_at: z.string().describe('ISO 8601 timestamp when the harvest request was created'),
});
export const HarvestStatusResponseSchema = HarvestCreationResponseSchema.extend({
	started_at: z.string().nullable().describe('ISO 8601 timestamp when the harvest started, or null if pending'),
	completed_at: z.string().nullable().describe('ISO 8601 timestamp when the harvest completed, or null otherwise'),
	failed_at: z.string().nullable().describe('ISO 8601 timestamp when the harvest failed, or null otherwise'),
	file_size: z
		.string()
		.nullable()
		.describe('Final file size of the downloaded data, expressed as a string, or null if not available'),
	progress_percent: z.number().describe('Harvest progress as a percentage value between 0 and 100'),
	progress_step: z.string().nullable().describe('Textual description of the current harvest step, if available'),
	error_message: z.string().nullable().describe('Error message when the harvest fails, or null otherwise'),
	download_url_expires_at: z
		.string()
		.nullable()
		.describe('ISO 8601 timestamp when the download URL expires, or null if unavailable'),
	expires_at: z
		.string()
		.nullable()
		.describe('ISO 8601 timestamp when the harvest download expires, or null if unavailable'),
});
export type HarvestStatusResponse = z.infer<typeof HarvestStatusResponseSchema>;

export const HarvestStatusResponseSchemaNullable = HarvestStatusResponseSchema.nullable();
export const HarvestDownloadUrlResponse = z.object({
	download_url: createStringType(1, 2048).describe('The temporary URL to download the harvest archive'),
	expires_at: z.string().describe('ISO 8601 timestamp when the harvest download expires'),
});

export type HarvestDownloadUrlResponse = z.infer<typeof HarvestDownloadUrlResponse>;
