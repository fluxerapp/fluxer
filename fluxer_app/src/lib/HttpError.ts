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

import type {HttpMethod} from '~/lib/HttpTypes';

export class HttpError extends Error {
	method: HttpMethod;
	url: string;
	status?: number;
	ok: boolean;
	body?: unknown;
	text?: string;
	headers?: Record<string, string>;

	constructor(params: {
		method: HttpMethod;
		url: string;
		ok: boolean;
		status: number;
		body?: unknown;
		text?: string;
		headers?: Record<string, string>;
	}) {
		const redactedUrl = params.url.replace(/\d+/g, 'xxx');
		super(`${params.method.toUpperCase()} ${redactedUrl} [${params.status}]`);

		this.name = 'HTTPResponseError';
		this.method = params.method;
		this.url = params.url;
		this.ok = params.ok;
		this.status = params.status;
		this.body = params.body;
		this.text = params.text;
		this.headers = params.headers;
	}
}
