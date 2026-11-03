// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ATTACHMENT_MAX_SIZE_NON_PREMIUM} from '@fluxer/constants/src/LimitConstants';

const logger = new Logger('AttachmentUtils');

function getMaxAttachmentFileSize(): number {
	return LimitResolver.resolve({
		key: 'max_attachment_file_size',
		fallback: ATTACHMENT_MAX_SIZE_NON_PREMIUM,
	});
}

function countFilesAboveLimit(files: ReadonlyArray<File>): number {
	const maxFileSize = getMaxAttachmentFileSize();
	return files.filter((file) => file.size > maxFileSize).length;
}

export async function countOversizedAttachments(files: ReadonlyArray<File>): Promise<number> {
	if (countFilesAboveLimit(files) === 0) {
		return 0;
	}
	try {
		await RuntimeConfig.revalidateDiscovery();
	} catch (error) {
		logger.warn('Failed to refresh the attachment size limit:', error);
	}
	return countFilesAboveLimit(files);
}
