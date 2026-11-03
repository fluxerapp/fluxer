// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead, recordAdminWrite} from '@app/api/admin/AdminAuditRecorder';
import {Config} from '@app/api/Config';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {HostedOnlyRoute} from '@app/api/middleware/BillingRouteAvailability';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {getOperatorPassRepository} from '@app/api/middleware/ServiceSingletons';
import type {OperatorPassFulfilmentService} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {
	addOperatorPassPreviewEntry,
	listOperatorPassPreviewEntries,
	removeOperatorPassPreviewEntry,
} from '@app/api/operator_pass/OperatorPassPreviewAccess';
import {planOperatorPassReset, resetOperatorPassPreviewData} from '@app/api/operator_pass/OperatorPassPreviewReset';
import {mapOperatorPassPurchaseToResponse} from '@app/api/operator_pass/OperatorPassPurchaseRules';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {
	getOperatorPassPhase,
	getOperatorPassSchedule,
	operatorPassNow,
} from '@app/api/operator_pass/OperatorPassSchedule';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnknownOperatorPassPurchaseError} from '@fluxer/errors/src/domains/operator_pass/UnknownOperatorPassPurchaseError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import {
	AdminOperatorPassPreviewIpCreateRequest,
	AdminOperatorPassPreviewIpEntry,
	AdminOperatorPassPreviewIpIdParam,
	AdminOperatorPassPreviewResponse,
	AdminOperatorPassPurchaseIdParam,
	AdminOperatorPassResetPlan,
	AdminOperatorPassResetRequest,
	AdminOperatorPassResetResponse,
} from '@fluxer/schema/src/domains/admin/AdminOperatorPassSchemas';
import {OperatorPassPurchaseResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import type {Context} from 'hono';

const TAG = 'Admin';

function requireFulfilment(ctx: Context<HonoEnv>): OperatorPassFulfilmentService {
	const fulfilment = ctx.get('stripeService').operatorPassFulfilment;
	if (!fulfilment) {
		throw new StripePaymentNotAvailableError();
	}
	return fulfilment;
}

async function listPreviewPurchases(repository: OperatorPassRepository) {
	const purchases = [];
	let pageState: string | null = null;
	do {
		const page = await repository.scanPurchases(500, pageState);
		purchases.push(...page.rows.filter((purchase) => purchase.preview_label != null));
		pageState = page.pageState;
	} while (pageState);
	purchases.sort((left, right) => right.created_at.getTime() - left.created_at.getTime());
	return Promise.all(
		purchases.map(async (purchase) => ({
			purchase_id: purchase.checkout_session_id,
			purchaser_id: purchase.purchaser_id.toString(),
			label: purchase.preview_label ?? '',
			status: purchase.status,
			quantity: Math.max(1, purchase.quantity ?? 1),
			currency: purchase.currency,
			created_at: purchase.created_at.toISOString(),
			delivered_at: purchase.delivered_at?.toISOString() ?? null,
			certificate_numbers: (await repository.listCertificatesByPurchase(purchase.checkout_session_id)).map(
				(certificate) => certificate.number,
			),
		})),
	);
}

export function OperatorPassPreviewAdminController(app: HonoApp) {
	app.get(
		'/admin/operator-pass/preview',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.OPERATOR_PASS_PREVIEW),
		OpenAPI({
			operationId: 'get_admin_operator_pass_preview',
			summary: 'Get Operator Pass preview entries',
			responseSchema: AdminOperatorPassPreviewResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Lists the IP addresses and CIDR ranges that see the Operator Pass sale open before it opens and buy passes at no cost, and every purchase made that way. Creates audit log entry. Only available on hosted instances. Requires OPERATOR_PASS_PREVIEW permission.',
		}),
		async (ctx) => {
			const entries = await listOperatorPassPreviewEntries(getKVClient());
			const purchases = await listPreviewPurchases(getOperatorPassRepository());
			await recordAdminRead(ctx, {
				targetType: 'operator_pass_preview',
				targetId: 0n,
				action: AdminAuditReadActions.GET_OPERATOR_PASS_PREVIEW,
				metadata: {entry_count: entries.length, purchase_count: purchases.length},
			});
			return ctx.json({
				entries,
				config_values: [...Config.operatorPass.previewIps],
				sale_open: getOperatorPassPhase(operatorPassNow(), getOperatorPassSchedule()) !== 'waitlist',
				purchases,
			});
		},
	);
	app.post(
		'/admin/operator-pass/preview/ips',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.OPERATOR_PASS_PREVIEW),
		Validator('json', AdminOperatorPassPreviewIpCreateRequest),
		OpenAPI({
			operationId: 'add_admin_operator_pass_preview_ip',
			summary: 'Add Operator Pass preview entry',
			responseSchema: AdminOperatorPassPreviewIpEntry,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Adds an IPv4 or IPv6 address or a CIDR range. Until the sale opens for everyone, requests from it see the sale open, and checkouts created from it cost nothing and are marked as preview purchases. Creates audit log entry. Only available on hosted instances. Requires OPERATOR_PASS_PREVIEW permission.',
		}),
		async (ctx) => {
			const body = ctx.req.valid('json');
			const now = new Date();
			const expiresAt = body.expires_at ? new Date(body.expires_at) : null;
			if (expiresAt && expiresAt.getTime() <= now.getTime()) {
				throw InputValidationError.create('expires_at', 'The expiry must be in the future');
			}
			const entry = await addOperatorPassPreviewEntry(getKVClient(), {
				value: body.value,
				label: body.label,
				expiresAt,
				createdBy: ctx.get('adminUserId').toString(),
				now,
			});
			if (!entry) {
				throw InputValidationError.create('value', 'Give an IPv4 or IPv6 address or a CIDR range');
			}
			await recordAdminWrite(ctx, {
				targetType: 'operator_pass_preview',
				targetId: 0n,
				action: 'add_operator_pass_preview_ip',
				metadata: {entry_id: entry.id, value: entry.value, label: entry.label, expires_at: entry.expires_at},
			});
			return ctx.json(entry);
		},
	);
	app.delete(
		'/admin/operator-pass/preview/ips/:entry_id',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.OPERATOR_PASS_PREVIEW),
		Validator('param', AdminOperatorPassPreviewIpIdParam),
		OpenAPI({
			operationId: 'remove_admin_operator_pass_preview_ip',
			summary: 'Remove Operator Pass preview entry',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Removes a preview entry. Requests from it see the real sale phase again at once. Purchases already made through it stay preview purchases. Creates audit log entry. Only available on hosted instances. Requires OPERATOR_PASS_PREVIEW permission.',
		}),
		async (ctx) => {
			const {entry_id: entryId} = ctx.req.valid('param');
			const removed = await removeOperatorPassPreviewEntry(getKVClient(), entryId);
			await recordAdminWrite(ctx, {
				targetType: 'operator_pass_preview',
				targetId: 0n,
				action: 'remove_operator_pass_preview_ip',
				metadata: {entry_id: entryId, removed},
			});
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/admin/operator-pass/preview/purchases/:purchase_id/cancel',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.OPERATOR_PASS_PREVIEW),
		Validator('param', AdminOperatorPassPurchaseIdParam),
		OpenAPI({
			operationId: 'cancel_admin_operator_pass_preview_purchase',
			summary: 'Cancel Operator Pass preview purchase',
			responseSchema: OperatorPassPurchaseResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Ends every pass of a preview purchase the way a full refund does: the passes are revoked, the certificate pages are cancelled and keep their numbers, and the purchase is marked refunded. Nothing is charged or refunded in Stripe. An open checkout is expired and its pending purchase deleted. Refuses purchases that are not preview purchases. Creates audit log entry. Only available on hosted instances. Requires OPERATOR_PASS_PREVIEW permission.',
		}),
		async (ctx) => {
			const {purchase_id: purchaseId} = ctx.req.valid('param');
			const fulfilment = requireFulfilment(ctx);
			const found = await fulfilment.repository.findPurchase(purchaseId);
			if (!found || found.preview_label == null) {
				throw new UnknownOperatorPassPurchaseError();
			}
			const purchase = await fulfilment.cancelPreviewPurchase(purchaseId);
			await recordAdminWrite(ctx, {
				targetType: 'user',
				targetId: purchase.purchaser_id,
				action: 'cancel_operator_pass_preview_purchase',
				metadata: {checkout_session_id: purchaseId, status: purchase.status},
			});
			return ctx.json(
				mapOperatorPassPurchaseToResponse(
					purchase,
					await fulfilment.repository.listUnitsByPurchase(purchaseId),
					operatorPassNow(),
					new Map(),
				),
			);
		},
	);
	app.get(
		'/admin/operator-pass/reset',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.OPERATOR_PASS_RESET),
		OpenAPI({
			operationId: 'get_admin_operator_pass_reset_plan',
			summary: 'Get Operator Pass reset plan',
			responseSchema: AdminOperatorPassResetPlan,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Counts what a reset of the Operator Pass preview data would remove, and says why it is refused when it is. Writes nothing. Creates audit log entry. Only available on hosted instances. Requires OPERATOR_PASS_RESET permission.',
		}),
		async (ctx) => {
			const plan = await planOperatorPassReset(getOperatorPassRepository());
			await recordAdminRead(ctx, {
				targetType: 'operator_pass_preview',
				targetId: 0n,
				action: AdminAuditReadActions.GET_OPERATOR_PASS_RESET_PLAN,
				metadata: {allowed: plan.allowed, preview_purchases: plan.preview_purchases},
			});
			return ctx.json(plan);
		},
	);
	app.post(
		'/admin/operator-pass/reset',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.OPERATOR_PASS_RESET),
		Validator('json', AdminOperatorPassResetRequest),
		OpenAPI({
			operationId: 'reset_admin_operator_pass_preview_data',
			summary: 'Reset Operator Pass preview data',
			responseSchema: AdminOperatorPassResetResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Removes every preview purchase with its passes, certificate pages and numbers, gift offers, wall entries and the Fluxer Labs rows they created, ends the Labs role they gave, and starts certificate numbering again at 1. Refused when any purchase was not made through a preview entry, when an account holds passes no preview purchase explains, when a Visionary holder turned on the Operator Pass, when a certificate number belongs to another pass, or when a preview purchase has a billing document. Keeps the waitlist. Creates audit log entry. Only available on hosted instances. Requires OPERATOR_PASS_RESET permission.',
		}),
		async (ctx) => {
			const result = await resetOperatorPassPreviewData({
				fulfilment: requireFulfilment(ctx),
				cacheService: ctx.get('cacheService'),
				kvClient: getKVClient(),
			});
			await recordAdminWrite(ctx, {
				targetType: 'operator_pass_preview',
				targetId: 0n,
				action: 'reset_operator_pass_preview_data',
				metadata: {
					preview_purchases: result.removed.preview_purchases,
					passes: result.removed.passes,
					certificate_numbers: result.removed.certificate_numbers,
					users: result.removed.users,
					remaining_purchases: result.remaining.preview_purchases,
				},
			});
			return ctx.json(result);
		},
	);
}
