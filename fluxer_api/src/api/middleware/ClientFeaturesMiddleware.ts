// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoEnv} from '@app/api/types/HonoEnv';
import {CLIENT_FEATURES_HEADER, parseClientFeaturesHeader} from '@app/api/utils/featureUtils';
import {createMiddleware} from 'hono/factory';

export const ClientFeaturesMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	ctx.set('clientFeatures', parseClientFeaturesHeader(ctx.req.header(CLIENT_FEATURES_HEADER)));
	return next();
});
