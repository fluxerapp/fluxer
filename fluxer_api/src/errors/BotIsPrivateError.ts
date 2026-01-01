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

import type {FluxerErrorData} from './FluxerAPIError';
import {ForbiddenError} from './ForbiddenError';

export class BotIsPrivateError extends ForbiddenError {
	constructor({
		message = 'This bot is private and can only be invited by the owner',
		headers,
		data,
	}: {
		message?: string;
		data?: FluxerErrorData;
		headers?: Record<string, string>;
	} = {}) {
		super({code: 'BOT_IS_PRIVATE', message, data, headers});
	}
}
