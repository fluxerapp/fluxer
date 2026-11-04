// SPDX-License-Identifier: AGPL-3.0-or-later

import {signInWithAdminSso} from '@app/api/admin/AdminSsoSignIn';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminSsoSignInRequest, AdminSsoSignInResponse} from '@fluxer/schema/src/domains/admin/AdminSsoSchemas';

export function AdminSsoAdminController(app: HonoApp) {
	app.post(
		'/admin/sso/sign-in',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_SSO_SIGN_IN),
		Validator('json', AdminSsoSignInRequest),
		OpenAPI({
			operationId: 'exchange_admin_sso_token',
			summary: 'Exchange admin SSO ID token',
			responseSchema: AdminSsoSignInResponse,
			statusCode: 200,
			security: [],
			tags: 'Admin',
			description:
				'Verifies an ID token from the admin sign-on provider and returns an admin panel access token for the Fluxer account in its fluxer_id claim. Only the admin panel calls it, with its OAuth2 client ID and secret as HTTP Basic auth. The account must already have admin:authenticate.',
		}),
		async (ctx) => {
			const body = ctx.req.valid('json');
			const result = await signInWithAdminSso(ctx, {
				authorizationHeader: ctx.req.header('authorization') ?? undefined,
				idToken: body.id_token,
				nonce: body.nonce,
			});
			return ctx.json(result);
		},
	);
}
