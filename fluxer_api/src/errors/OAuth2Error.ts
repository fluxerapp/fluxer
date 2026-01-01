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

import {HTTPException} from 'hono/http-exception';

type ErrorStatusCode = 400 | 401 | 403;

export class OAuth2Error extends HTTPException {
	error: string;
	errorDescription: string;
	override status: ErrorStatusCode;

	constructor({
		error,
		errorDescription,
		status = 400,
	}: {
		error: string;
		errorDescription: string;
		status?: ErrorStatusCode;
	}) {
		super(status, {message: errorDescription});
		this.error = error;
		this.errorDescription = errorDescription;
		this.status = status;
		this.name = 'OAuth2Error';
	}

	override getResponse(): Response {
		return new Response(
			JSON.stringify({
				error: this.error,
				error_description: this.errorDescription,
			}),
			{
				status: this.status,
				headers: {
					'Content-Type': 'application/json',
				},
			},
		);
	}
}
