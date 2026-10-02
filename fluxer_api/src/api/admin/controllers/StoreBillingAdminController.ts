// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead, recordAdminWrite} from '@app/api/admin/AdminAuditRecorder';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {HostedOnlyRoute} from '@app/api/store_billing/StoreBillingController';
import {mapStorePurchaseToAdminResponse} from '@app/api/store_billing/StoreBillingMappers';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {AdminStorePurchaseListResponse} from '@fluxer/schema/src/domains/admin/AdminStoreBillingSchemas';
import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import type {Context} from 'hono';

async function requireUser(ctx: Context<HonoEnv>, userId: UserID): Promise<void> {
	if (!(await ctx.get('userRepository').findUnique(userId))) {
		throw new UnknownUserError();
	}
}

export function StoreBillingAdminController(app: HonoApp) {
	app.get(
		'/admin/users/:user_id/store-purchases',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'list_admin_user_store_purchases',
			summary: 'List user store purchases',
			responseSchema: AdminStorePurchaseListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Lists the App Store and Google Play purchases bound to a user, newest first, with their store state. Only available on hosted instances. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			await requireUser(ctx, userId);
			const rows = await ctx.get('storeEntitlementService').listStorePurchases(userId);
			await recordAdminRead(ctx, {
				targetType: 'user',
				targetId: userId,
				action: AdminAuditReadActions.LIST_USER_STORE_PURCHASES,
				metadata: {result_count: rows.length},
			});
			return ctx.json({purchases: rows.map(mapStorePurchaseToAdminResponse)});
		},
	);
	app.post(
		'/admin/users/:user_id/store-purchases/refresh',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'refresh_admin_user_store_purchases',
			summary: 'Refresh user store purchases',
			responseSchema: AdminStorePurchaseListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Reads every store purchase bound to a user again from the App Store or Google Play, applies the result to the account and returns the updated purchases. Creates audit log entry. Only available on hosted instances. Requires USER_UPDATE_FLAGS permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			await requireUser(ctx, userId);
			const storeEntitlementService = ctx.get('storeEntitlementService');
			const rows = await storeEntitlementService.listStorePurchases(userId);
			for (const row of rows) {
				await storeEntitlementService.refreshStorePurchase(row.store_key);
			}
			await storeEntitlementService.applyStoreEntitlementToUser(userId);
			const refreshed = await storeEntitlementService.listStorePurchases(userId);
			await recordAdminWrite(ctx, {
				targetType: 'user',
				targetId: userId,
				action: 'refresh_store_purchases',
				metadata: {purchase_count: rows.length},
			});
			return ctx.json({purchases: refreshed.map(mapStorePurchaseToAdminResponse)});
		},
	);
}
