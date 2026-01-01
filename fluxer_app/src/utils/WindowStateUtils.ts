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

export enum StateFlags {
	SIZE = 1,
	POSITION = 2,
	MAXIMIZED = 4,
	FULLSCREEN = 8,
	DECORATIONS = 16,
	VISIBLE = 32,
	ALL = SIZE | POSITION | MAXIMIZED | FULLSCREEN | DECORATIONS | VISIBLE,
}

export async function saveCurrentWindowState(_flags: StateFlags = StateFlags.ALL): Promise<void> {}

export function buildStateFlags(_options: {
	rememberSizeAndPosition: boolean;
	rememberMaximized: boolean;
	rememberFullscreen: boolean;
}): StateFlags {
	return StateFlags.ALL;
}
