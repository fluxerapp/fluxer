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

import {MAX_ATTACHMENTS_PER_MESSAGE} from '~/Constants';
import {CloudUpload} from '~/lib/CloudUpload';

interface FileUploadResult {
	success: boolean;
	error?: 'too_many_attachments' | 'no_files';
}

export async function handleFileUpload(
	channelId: string,
	files: FileList | Array<File>,
	currentAttachmentCount: number,
): Promise<FileUploadResult> {
	const fileArray = Array.from(files);

	if (fileArray.length === 0) {
		return {success: false, error: 'no_files'};
	}

	if (currentAttachmentCount + fileArray.length > MAX_ATTACHMENTS_PER_MESSAGE) {
		return {success: false, error: 'too_many_attachments'};
	}

	await CloudUpload.addFiles(channelId, fileArray);
	return {success: true};
}
