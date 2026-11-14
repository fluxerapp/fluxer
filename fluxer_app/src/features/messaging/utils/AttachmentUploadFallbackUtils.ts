// SPDX-License-Identifier: AGPL-3.0-or-later

import {ATTACHMENT_MAX_SIZE_NON_PREMIUM} from '@fluxer/constants/src/LimitConstants';

interface FileSizeLike {
	size: number;
}

export const MULTIPART_ATTACHMENT_FALLBACK_MAX_REQUEST_SIZE = ATTACHMENT_MAX_SIZE_NON_PREMIUM;

function getMultipartFallbackRequestSize(files: ReadonlyArray<FileSizeLike>): number {
	return files.reduce((total, file) => total + file.size, 0);
}

export function exceedsMultipartFallbackRequestSize(
	files: ReadonlyArray<FileSizeLike>,
	maxRequestSize = MULTIPART_ATTACHMENT_FALLBACK_MAX_REQUEST_SIZE,
): boolean {
	return getMultipartFallbackRequestSize(files) > maxRequestSize;
}
