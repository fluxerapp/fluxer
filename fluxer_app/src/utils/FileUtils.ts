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

export const formatFileSize = (bytes: number): string => {
	if (bytes === 0) return '0 B';
	const k = 1000;
	const sizes = ['B', 'KB', 'MB', 'GB'];
	const i = Math.floor(Math.log(bytes) / Math.log(k));
	return `${Math.round((bytes / k ** i) * 10) / 10} ${sizes[i]}`;
};
