// SPDX-License-Identifier: AGPL-3.0-or-later

import path from 'node:path';
import type {FileFilter} from 'electron';

function fileExtension(filePath: string): string {
	return path.extname(filePath).slice(1);
}

export function saveDialogFilters(defaultPath: string): Array<FileFilter> {
	const extension = fileExtension(defaultPath);
	const allFiles: FileFilter = {name: 'All files', extensions: ['*']};
	if (!extension) {
		return [allFiles];
	}
	return [{name: `${extension.toUpperCase()} file`, extensions: [extension]}, allFiles];
}

export function withDefaultExtension(filePath: string, defaultPath: string): string {
	const extension = fileExtension(defaultPath);
	if (!extension || fileExtension(filePath)) {
		return filePath;
	}
	return `${filePath}.${extension}`;
}
