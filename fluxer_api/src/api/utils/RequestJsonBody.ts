// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {HonoRequest} from 'hono';
import {parseJsonPreservingLargeIntegers} from './LosslessJsonParser';

interface RequestJsonBody {
	parsed: boolean;
	value: unknown;
}

const bodyCache = new WeakMap<Request, RequestJsonBody>();

export async function readRequestJsonBody(req: HonoRequest): Promise<RequestJsonBody> {
	const raw = req.raw;
	const cached = bodyCache.get(raw);
	if (cached) return cached;
	let body: RequestJsonBody;
	try {
		const text = await req.text();
		body = {parsed: true, value: text.trim().length === 0 ? {} : parseJsonPreservingLargeIntegers(text)};
	} catch {
		body = {parsed: false, value: {}};
	}
	bodyCache.set(raw, body);
	return body;
}

export async function requireRequestJsonBody(req: HonoRequest): Promise<unknown> {
	const body = await readRequestJsonBody(req);
	if (!body.parsed) {
		throw InputValidationError.fromCode('body', ValidationErrorCodes.INVALID_FORMAT);
	}
	return body.value;
}
