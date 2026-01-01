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

import assert from 'node:assert/strict';
import type {Context} from 'hono';
import {HTTPException} from 'hono/http-exception';
import {Config} from '~/Config';
import {toBodyData} from '~/lib/BinaryUtils';
import {setHeaders} from '~/lib/HttpUtils';
import type {HonoEnv} from '~/lib/MediaTypes';
import {readS3Object} from '~/lib/S3Utils';

export const handleStaticProxyRequest = async (ctx: Context<HonoEnv>): Promise<Response> => {
	const bucket = Config.AWS_S3_BUCKET_STATIC;
	const path = ctx.req.path;
	if (!bucket || path === '/') {
		return ctx.text('Not Found', 404);
	}
	const key = path.replace(/^\/+/, '');
	try {
		const {data, size, contentType, lastModified} = await readS3Object(bucket, key);
		assert(Buffer.isBuffer(data));
		setHeaders(ctx, size, contentType, null, lastModified);
		return ctx.body(toBodyData(data));
	} catch (error) {
		if (error instanceof HTTPException) {
			throw error;
		}
		return ctx.text('Not Found', 404);
	}
};
