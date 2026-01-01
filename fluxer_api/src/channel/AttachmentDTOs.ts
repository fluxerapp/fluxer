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

import {coerceNumberFromString, createStringType, FilenameType, Int32Type, z} from '~/Schema';

export const ClientAttachmentRequest = z.object({
	id: coerceNumberFromString(Int32Type),
	filename: FilenameType,
	title: createStringType(1, 1024).nullish(),
	description: createStringType(1, 1024).nullish(),
	flags: coerceNumberFromString(Int32Type).default(0),
});
export type ClientAttachmentRequest = z.infer<typeof ClientAttachmentRequest>;

export interface UploadedAttachment {
	id: number;
	filename: string;
	upload_filename: string;
	file_size: number;
	content_type: string;
}

export interface AttachmentToProcess {
	id: number;
	filename: string;
	upload_filename: string;
	title: string | null;
	description: string | null;
	flags: number;
	file_size: number;
	content_type: string;
}

export const ClientAttachmentReferenceRequest = z.object({
	id: coerceNumberFromString(Int32Type),
	filename: FilenameType.optional(),
	title: createStringType(1, 1024).nullish(),
	description: createStringType(1, 1024).nullish(),
	flags: coerceNumberFromString(Int32Type).default(0),
});
export type ClientAttachmentReferenceRequest = z.infer<typeof ClientAttachmentReferenceRequest>;

export function mergeUploadWithClientData(
	uploaded: UploadedAttachment,
	clientData?: ClientAttachmentRequest | ClientAttachmentReferenceRequest,
): AttachmentToProcess {
	return {
		id: uploaded.id,
		filename: uploaded.filename,
		upload_filename: uploaded.upload_filename,
		file_size: uploaded.file_size,
		content_type: uploaded.content_type,
		title: clientData?.title ?? null,
		description: clientData?.description ?? null,
		flags: 'flags' in (clientData ?? {}) ? (clientData as ClientAttachmentRequest).flags : 0,
	};
}

export type AttachmentRequestData = AttachmentToProcess | ClientAttachmentRequest | ClientAttachmentReferenceRequest;
