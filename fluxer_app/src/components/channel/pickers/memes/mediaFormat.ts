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

export const formatDuration = (seconds: number | null | undefined): string => {
	if (!seconds || seconds <= 0) return '0:00';

	const hours = Math.floor(seconds / 3600);
	const minutes = Math.floor((seconds % 3600) / 60);
	const secs = Math.floor(seconds % 60);

	if (hours > 0) {
		return `${hours}:${minutes.toString().padStart(2, '0')}:${secs.toString().padStart(2, '0')}`;
	}
	return `${minutes}:${secs.toString().padStart(2, '0')}`;
};

export const getFileExtension = (filename: string, contentType: string): string => {
	const extension = filename.split('.').pop()?.toUpperCase();
	if (extension && extension.length <= 4) return extension;

	const typeMatch = contentType.match(/\/([^;]+)/);
	return typeMatch?.[1]?.toUpperCase() || 'FILE';
};
