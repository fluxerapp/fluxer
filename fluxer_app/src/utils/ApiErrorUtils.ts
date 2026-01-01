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

import {HttpError} from '~/lib/HttpError';

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null;
}

export function getApiErrorCode(error: unknown): string | undefined {
	if (!(error instanceof HttpError)) return undefined;
	if (!isRecord(error.body)) return undefined;
	const code = error.body.code;
	return typeof code === 'string' ? code : undefined;
}
