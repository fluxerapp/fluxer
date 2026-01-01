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

type ErrorStatusCode = 202 | 400 | 401 | 403 | 404 | 423 | 429 | 500;

export type FluxerErrorData = Record<string, unknown>;

export class FluxerAPIError extends HTTPException {
	code: string;
	override message: string;
	override status: ErrorStatusCode;
	data?: FluxerErrorData;
	headers?: Record<string, string>;

	constructor({
		code,
		message,
		status,
		data,
		headers,
	}: {
		code: string;
		message: string;
		status: ErrorStatusCode;
		data?: FluxerErrorData;
		headers?: Record<string, string>;
	}) {
		super(status, {message});
		this.code = code;
		this.message = message;
		this.status = status;
		this.data = data;
		this.headers = headers;
		this.name = 'FluxerAPIError';
	}

	override getResponse(): Response {
		return new Response(
			JSON.stringify({
				code: this.code,
				message: this.message,
				...this.data,
			}),
			{
				status: this.status,
				headers: {
					'Content-Type': 'application/json',
					...this.headers,
				},
			},
		);
	}
}
