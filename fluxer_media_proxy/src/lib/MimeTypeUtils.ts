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

import {filetypeinfo} from 'magic-bytes.js';
import {Logger} from '~/Logger';
import {SUPPORTED_EXTENSIONS, SUPPORTED_MIME_TYPES, type SupportedExtension} from '~/lib/MediaTypes';

export const getMimeType = (buffer: Buffer, filename?: string): string | null => {
	if (filename) {
		const ext = filename.split('.').pop()?.toLowerCase();
		if (ext && ext in SUPPORTED_EXTENSIONS) {
			const mimeType = SUPPORTED_EXTENSIONS[ext as SupportedExtension];
			return mimeType;
		}
	}

	try {
		const fileInfo = filetypeinfo(buffer);
		if (fileInfo?.[0]?.mime && SUPPORTED_MIME_TYPES.has(fileInfo[0].mime)) {
			return fileInfo[0].mime;
		}
	} catch (error) {
		Logger.error({error}, 'Failed to detect file type using magic bytes');
	}

	return null;
};

export const generateFilename = (mimeType: string, originalFilename?: string): string => {
	const baseName = originalFilename ? originalFilename.split('.')[0] : 'file';
	const mimeToExt = Object.entries(SUPPORTED_EXTENSIONS).reduce(
		(acc, [ext, mime]) => {
			acc[mime] = ext;
			return acc;
		},
		{} as Record<string, string>,
	);

	const extension = mimeToExt[mimeType];
	if (!extension) throw new Error(`Unsupported MIME type: ${mimeType}`);
	return `${baseName}.${extension}`;
};

export const getMediaCategory = (mimeType: string): string | null => {
	const category = mimeType.split('/')[0];
	return ['image', 'video', 'audio'].includes(category) ? category : null;
};
