// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageAttachmentFlags, MessageAttachmentFlagsDescriptions} from '@fluxer/constants/src/ChannelConstants';
import {FilenameType} from '@fluxer/schema/src/primitives/FileValidators';
import {
	coerceNumberFromString,
	createBitflagInt32Type,
	createStringType,
	Int32Type,
	NonNegativeSafeIntegerType,
	SnowflakeType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const ClientAttachmentBase = z.object({
	title: createStringType(1, 1024).nullish().describe('A title for the attachment (1-1024 characters)'),
	description: createStringType(1, 4096)
		.nullish()
		.describe('An alt text description of the attachment (1-4096 characters)'),
	flags: coerceNumberFromString(
		createBitflagInt32Type(
			MessageAttachmentFlags,
			MessageAttachmentFlagsDescriptions,
			'Attachment flags',
			'MessageAttachmentFlags',
		),
	).default(0),
	duration: Int32Type.nullish().describe('The duration of the audio file in seconds'),
	waveform: createStringType(1, 4096).nullish().describe('Base64 encoded audio waveform data'),
});
export const ClientAttachmentRequest = ClientAttachmentBase.extend({
	id: coerceNumberFromString(Int32Type).describe('The client-side identifier for this attachment'),
	filename: FilenameType.describe('The name of the file being uploaded'),
	content_type: createStringType(1, 255).optional().describe('Optional MIME type for the uploaded file'),
});

export type ClientAttachmentRequest = z.infer<typeof ClientAttachmentRequest>;

export const ClientUploadedAttachmentRequest = ClientAttachmentBase.extend({
	id: coerceNumberFromString(Int32Type).describe('The client-side identifier for this attachment'),
	filename: FilenameType.describe('The name of the file being uploaded'),
	upload_filename: createStringType(1, 4096).describe(
		'Temporary upload key returned by the attachment upload endpoint',
	),
	file_size: coerceNumberFromString(NonNegativeSafeIntegerType).describe('Uploaded file size in bytes'),
	content_type: createStringType(1, 255).describe('MIME type of the uploaded file'),
});

export type ClientUploadedAttachmentRequest = z.infer<typeof ClientUploadedAttachmentRequest>;

export const ClientAttachmentReferenceRequest = ClientAttachmentBase.extend({
	id: z
		.union([coerceNumberFromString(Int32Type), SnowflakeType])
		.describe('The identifier of the attachment being referenced (snowflake ID or file index)'),
	filename: FilenameType.optional().describe('A new filename for the attachment'),
});

export type ClientAttachmentReferenceRequest = z.infer<typeof ClientAttachmentReferenceRequest>;
