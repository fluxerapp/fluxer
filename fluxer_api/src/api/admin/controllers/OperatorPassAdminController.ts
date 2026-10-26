// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead, recordAdminWrite} from '@app/api/admin/AdminAuditRecorder';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import type {OperatorPassUnitRow} from '@app/api/database/types/OperatorPassTypes';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {HostedOnlyRoute} from '@app/api/middleware/BillingRouteAvailability';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {
	getOperatorPassRepository,
	getOperatorPassWallInfrastructure,
	getOperatorPassWallService,
} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {
	mapOffer,
	mapPass,
	mapRelease,
	mapRemoval,
	mapRemovalHistory,
	mapUnit,
	mapWallDecision,
	mapWallEntry,
	mapWallEvent,
	wallAvatarRoute,
} from '@app/api/operator_pass/OperatorPassAdminMappers';
import {requireOperatorPassBusinessPurchase} from '@app/api/operator_pass/OperatorPassBusinessListing';
import {resolveOperatorPassHolder} from '@app/api/operator_pass/OperatorPassEntitlementService';
import type {OperatorPassFulfilmentService} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {mapOperatorPassPurchaseToResponse} from '@app/api/operator_pass/OperatorPassPurchaseRules';
import {calculateOperatorPassRecognition} from '@app/api/operator_pass/OperatorPassRecognition';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import type {OperatorPassWallDecisionResult} from '@app/api/operator_pass/OperatorPassWallModerationService';
import {hasOperatorPassWallChoice} from '@app/api/operator_pass/OperatorPassWallPresentation';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnknownOperatorPassPurchaseError} from '@fluxer/errors/src/domains/operator_pass/UnknownOperatorPassPurchaseError';
import {UnknownOperatorPassWallEntryError} from '@fluxer/errors/src/domains/operator_pass/UnknownOperatorPassWallEntryError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import {
	AdminOperatorPassNoticeRequest,
	AdminOperatorPassPurchaseIdParam,
	AdminOperatorPassReleaseEraseParam,
	AdminOperatorPassReleaseFileResponse,
	AdminOperatorPassReleasePreviewResponse,
	AdminOperatorPassReleasePublishRequest,
	AdminOperatorPassReleaseYearParam,
	AdminOperatorPassRemovalRequest,
	AdminOperatorPassRemovalResponse,
	AdminOperatorPassUserResponse,
	AdminOperatorPassWaitlistFollowupTextsResponse,
	AdminOperatorPassWaitlistStatsResponse,
	AdminOperatorPassWallAsOfQuery,
	AdminOperatorPassWallAsOfResponse,
	AdminOperatorPassWallAvatarParam,
	AdminOperatorPassWallBulkRequest,
	AdminOperatorPassWallBulkResponse,
	AdminOperatorPassWallBusinessRequest,
	AdminOperatorPassWallDecisionRequest,
	AdminOperatorPassWallEntryResponse,
	AdminOperatorPassWallFreezeRequest,
	AdminOperatorPassWallHideRequest,
	AdminOperatorPassWallLegalHoldRequest,
	AdminOperatorPassWallQueueResponse,
	AdminOperatorPassWallRevisionIdParam,
	AdminOperatorPassWallStateResponse,
	AdminOperatorPassWallTimelineResponse,
	AdminOperatorPassWithdrawRequest,
} from '@fluxer/schema/src/domains/admin/AdminOperatorPassSchemas';
import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
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

async function wallStateResponse() {
	const service = getOperatorPassWallService();
	const state = await service.publication.loadState();
	const snapshot = await service.publication.snapshot();
	return {
		version: Number(state.wall_version),
		frozen: state.frozen,
		frozen_at: state.frozen_at?.toISOString() ?? null,
		frozen_by: state.frozen_by?.toString() ?? null,
		published_at: state.published_at?.toISOString() ?? null,
		entry_count: snapshot.entries.length,
	};
}

async function sendWallStatement(
	ctx: Context<HonoEnv>,
	user: User,
	result: OperatorPassWallDecisionResult,
	live: {name: string | null; avatar: string | null},
): Promise<boolean> {
	if (!user.email || !result.decision.reason) {
		return false;
	}
	const revision = result.revision;
	const nameChanged = revision ? revision.name !== live.name : true;
	const avatarChanged = revision ? (revision.avatar ?? null) !== live.avatar : true;
	let target: 'both' | 'avatar' | 'name' = 'name';
	if (nameChanged && avatarChanged) {
		target = 'both';
	} else if (avatarChanged) {
		target = 'avatar';
	}
	let shown: 'previous' | 'operator' | 'username' = 'username';
	if (result.entry.live_revision_id != null) {
		shown = 'previous';
	} else if (result.entry.username_declined) {
		shown = 'operator';
	}
	return ctx
		.get('emailService')
		.send(
			'operator_pass_wall_name_replaced',
			user.email,
			{target, shown, reason: result.decision.reason, note: result.decision.note ?? ''},
			user.locale,
		);
}

async function sendHiddenStatement(
	ctx: Context<HonoEnv>,
	user: User,
	result: OperatorPassWallDecisionResult,
): Promise<boolean> {
	if (!user.email || !result.decision.reason) {
		return false;
	}
	return ctx
		.get('emailService')
		.send(
			'operator_pass_wall_hidden',
			user.email,
			{reason: result.decision.reason, note: result.decision.note ?? ''},
			user.locale,
		);
}

async function indexWallAudit(userId: UserID, logId: Promise<bigint>): Promise<void> {
	await getOperatorPassRepository().addWallAuditLog(userId, await logId);
}

export function OperatorPassAdminController(app: HonoApp) {
	app.get(
		'/admin/users/:user_id/operator-pass',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'get_admin_user_operator_pass',
			summary: 'Get user Operator Pass',
			responseSchema: AdminOperatorPassUserResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Returns the Operator Pass of a user with any section 8 removal and the removal history, and the purchases the user paid for. Only available on hosted instances. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			const user = await ctx.get('userRepository').findUniqueAssert(userId);
			const repository = getOperatorPassRepository();
			const [pass, removal, removalHistory, purchases, wallEntry, wallHistory, heldUnits, offers] = await Promise.all([
				repository.findOperatorPass(userId),
				repository.findRemoval(userId),
				repository.listRemovalHistory(userId),
				repository.listPurchasesByPurchaser(userId),
				repository.findWallEntry(userId),
				repository.listWallEvents(userId),
				repository.listUnitsByHolder(userId),
				repository.listOffersByUser(userId),
			]);
			const unitsByPurchase = new Map<string, Array<OperatorPassUnitRow>>();
			for (const purchase of purchases) {
				unitsByPurchase.set(
					purchase.checkout_session_id,
					await repository.listUnitsByPurchase(purchase.checkout_session_id),
				);
			}
			const units = new Map<string, OperatorPassUnitRow>();
			for (const unit of [...heldUnits, ...[...unitsByPurchase.values()].flat()]) {
				units.set(unit.pass_id, unit);
			}
			const sortedUnits = [...units.values()].sort(
				(left, right) => left.first_delivered_at.getTime() - right.first_delivered_at.getTime(),
			);
			const unitEvents = await Promise.all(sortedUnits.map((unit) => repository.listUnitEvents(unit.pass_id)));
			const now = operatorPassNow();
			const holder = resolveOperatorPassHolder({pass, removal}, now);
			await recordAdminRead(ctx, {
				targetType: 'user',
				targetId: userId,
				action: AdminAuditReadActions.GET_USER_OPERATOR_PASS,
				metadata: {purchase_count: purchases.length},
			});
			return ctx.json({
				holder: holder
					? {
							since: holder.since.toISOString(),
							source: holder.source,
							count: holder.count,
							recognition_count: calculateOperatorPassRecognition(userId, sortedUnits)?.count ?? holder.count,
							wall_chosen: hasOperatorPassWallChoice(await repository.findWallEntry(userId)),
						}
					: null,
				pass: pass ? mapPass(pass, purchases) : null,
				removal: removal ? mapRemoval(removal) : null,
				removal_history: removalHistory.map(mapRemovalHistory),
				purchases: purchases
					.sort((left, right) => right.created_at.getTime() - left.created_at.getTime())
					.map((purchase) =>
						mapOperatorPassPurchaseToResponse(purchase, unitsByPurchase.get(purchase.checkout_session_id) ?? [], now),
					),
				units: sortedUnits.map((unit, index) => mapUnit(unit, unitEvents[index] ?? [])),
				offers: offers.sort((left, right) => left.created_at.getTime() - right.created_at.getTime()).map(mapOffer),
				wall: wallEntry ? mapWallEntry(wallEntry, user) : null,
				wall_history: wallHistory.map(mapWallEvent),
			});
		},
	);
	app.post(
		'/admin/users/:user_id/operator-pass/removal-notice',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', UserIdParam),
		Validator('json', AdminOperatorPassNoticeRequest),
		OpenAPI({
			operationId: 'send_admin_operator_pass_removal_notice',
			summary: 'Send Operator Pass removal notice',
			responseSchema: AdminOperatorPassRemovalResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Emails the holder that the pass will be removed permanently under section 8 of the Operator Pass terms, with the rule, the facts and 7 days to reply. A permanent removal for a breach that is not serious needs this notice first. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			const user = await ctx.get('userRepository').findUniqueAssert(userId);
			const body = ctx.req.valid('json');
			const {removal, notified} = await requireFulfilment(ctx).removalService.sendNotice({
				user,
				adminUserId: ctx.get('adminUserId'),
				rule: body.rule,
				facts: body.facts,
			});
			await recordAdminWrite(ctx, {
				targetType: 'user',
				targetId: userId,
				action: 'send_operator_pass_removal_notice',
				metadata: {notified},
			});
			return ctx.json(mapRemoval(removal));
		},
	);
	app.post(
		'/admin/users/:user_id/operator-pass/removal',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', UserIdParam),
		Validator('json', AdminOperatorPassRemovalRequest),
		OpenAPI({
			operationId: 'remove_admin_operator_pass_perks',
			summary: 'Remove Operator Pass perks',
			responseSchema: AdminOperatorPassRemovalResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Removes the Operator Pass perks of a user under section 8 of the Operator Pass terms, for a number of days or permanently, and emails the statement of reasons with the appeal route. A limited removal ends on its own. A permanent removal for a breach that is not serious needs an earlier limited removal and a removal notice sent after it and at least 7 days earlier. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			const user = await ctx.get('userRepository').findUniqueAssert(userId);
			const body = ctx.req.valid('json');
			const {removal, notified} = await requireFulfilment(ctx).removalService.remove({
				user,
				adminUserId: ctx.get('adminUserId'),
				rule: body.rule,
				facts: body.facts,
				automated: body.automated,
				serious: body.serious,
				durationDays: body.duration_days,
				notifyUser: body.notify_user,
			});
			await recordAdminWrite(ctx, {
				targetType: 'user',
				targetId: userId,
				action: 'remove_operator_pass_perks',
				metadata: {
					permanent: body.duration_days === null,
					duration_days: body.duration_days ?? undefined,
					automated: body.automated,
					serious: body.serious,
					notified,
				},
			});
			return ctx.json(mapRemoval(removal));
		},
	);
	app.delete(
		'/admin/users/:user_id/operator-pass/removal',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'restore_admin_operator_pass_perks',
			summary: 'Restore Operator Pass perks',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Ends a removal or a pending notice, for example after an appeal under section 6.2 of the terms of service. The perks come back with the original date on the badge. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			await ctx.get('userRepository').findUniqueAssert(userId);
			const restored = await requireFulfilment(ctx).removalService.restore(userId);
			await recordAdminWrite(ctx, {
				targetType: 'user',
				targetId: userId,
				action: 'restore_operator_pass_perks',
				metadata: {restored},
			});
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/admin/operator-pass/purchases/:purchase_id/withdraw',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', AdminOperatorPassPurchaseIdParam),
		Validator('json', AdminOperatorPassWithdrawRequest),
		OpenAPI({
			operationId: 'withdraw_admin_operator_pass_purchase',
			summary: 'Record Operator Pass withdrawal',
			responseSchema: OperatorPassPurchaseResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Records a withdrawal under section 7.1 of the Operator Pass terms that the buyer sent by the form, by email or in another clear way. Uses the time Fluxer received it, refunds the passes in full unless the payment was already refunded, ends them and emails the timestamped receipt to the account email. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const {purchase_id: purchaseId} = ctx.req.valid('param');
			const body = ctx.req.valid('json');
			const fulfilment = requireFulfilment(ctx);
			const found = await fulfilment.repository.findPurchase(purchaseId);
			if (!found) {
				throw new UnknownOperatorPassPurchaseError();
			}
			const purchaser = await ctx.get('userRepository').findUniqueAssert(found.purchaser_id);
			const receivedAt = new Date(body.received_at);
			const purchase = await fulfilment.withdraw({
				purchaser,
				purchaseId,
				name: body.name,
				channel: body.channel,
				receivedAt,
			});
			await recordAdminWrite(ctx, {
				targetType: 'user',
				targetId: purchaser.id,
				action: 'withdraw_operator_pass_purchase',
				metadata: {
					checkout_session_id: purchaseId,
					channel: body.channel,
					received_at: receivedAt.toISOString(),
				},
			});
			return ctx.json(
				mapOperatorPassPurchaseToResponse(
					purchase,
					await fulfilment.repository.listUnitsByPurchase(purchase.checkout_session_id),
					operatorPassNow(),
				),
			);
		},
	);
	app.get(
		'/admin/operator-pass/waitlist/stats',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.OPERATOR_PASS_WAITLIST_STATS),
		OpenAPI({
			operationId: 'get_admin_operator_pass_waitlist_stats',
			summary: 'Get Operator Pass waitlist statistics',
			responseSchema: AdminOperatorPassWaitlistStatsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Counts Operator Pass waitlist signups by status and the price bracket answers per currency, for confirmed signups and for all signups. Returns counts only, never addresses. Only available on hosted instances. Requires OPERATOR_PASS_WAITLIST_STATS permission.',
		}),
		async (ctx) => {
			const waitlistService = ctx.get('operatorPassWaitlistService');
			if (!waitlistService) {
				throw new Error('Operator Pass waitlist service is not available');
			}
			const stats = await waitlistService.getStats();
			await recordAdminRead(ctx, {
				targetType: 'operator_pass_waitlist',
				targetId: 0n,
				action: AdminAuditReadActions.GET_OPERATOR_PASS_WAITLIST_STATS,
				metadata: {total: stats.total, currency_count: stats.price_feedback.filter((entry) => entry.all > 0).length},
			});
			return ctx.json(stats);
		},
	);
	app.get(
		'/admin/operator-pass/waitlist/followup-texts',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.OPERATOR_PASS_WAITLIST_STATS),
		OpenAPI({
			operationId: 'list_admin_operator_pass_waitlist_followup_texts',
			summary: 'List Operator Pass waitlist follow-up answers in their own words',
			responseSchema: AdminOperatorPassWaitlistFollowupTextsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Lists every answer visitors wrote under other in the waitlist follow-up question, newest first, with the options picked alongside and the signup status. The text is plain text as the visitor typed it and must be escaped wherever it is shown. Never returns addresses. Only available on hosted instances. Requires OPERATOR_PASS_WAITLIST_STATS permission.',
		}),
		async (ctx) => {
			const waitlistService = ctx.get('operatorPassWaitlistService');
			if (!waitlistService) {
				throw new Error('Operator Pass waitlist service is not available');
			}
			const entries = await waitlistService.listFollowupTexts();
			await recordAdminRead(ctx, {
				targetType: 'operator_pass_waitlist',
				targetId: 0n,
				action: AdminAuditReadActions.LIST_OPERATOR_PASS_WAITLIST_FOLLOWUP_TEXTS,
				metadata: {count: entries.length},
			});
			return ctx.json({entries});
		},
	);
	app.get(
		'/admin/operator-pass/wall/review-queue',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		OpenAPI({
			operationId: 'list_admin_operator_pass_wall_review_queue',
			summary: "List Operators' wall changes to review",
			responseSchema: AdminOperatorPassWallQueueResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Lists the pending wall revisions, oldest first, with the live and pending name and avatar side by side for the diff. Nothing pending shows on the wall until staff approve it. The approval target is 72 hours. Only available on hosted instances. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const queue = await getOperatorPassWallService().moderation.listPending();
			await recordAdminRead(ctx, {
				targetType: 'operator_pass_wall',
				targetId: 0n,
				action: AdminAuditReadActions.LIST_OPERATOR_PASS_WALL_REVIEW_QUEUE,
				metadata: {count: queue.length},
			});
			return ctx.json({entries: queue.map(({entry, user}) => mapWallEntry(entry, user))});
		},
	);
	app.post(
		'/admin/users/:user_id/operator-pass/wall/revisions/:revision_id/approve',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', AdminOperatorPassWallRevisionIdParam),
		OpenAPI({
			operationId: 'approve_admin_operator_pass_wall_revision',
			summary: "Approve Operators' wall revision",
			responseSchema: AdminOperatorPassWallEntryResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Approves a pending wall revision. Its name and avatar go live on the wall and the avatar is published to the public snapshot store. Records a decision. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const userId = createUserID(BigInt(ctx.req.valid('param').user_id));
			const user = await ctx.get('userRepository').findUniqueAssert(userId);
			const result = await getOperatorPassWallService().moderation.approve(
				userId,
				ctx.get('adminUserId'),
				BigInt(ctx.req.valid('param').revision_id),
			);
			const notified = false;
			await indexWallAudit(
				userId,
				recordAdminWrite(ctx, {
					targetType: 'user',
					targetId: userId,
					action: 'approve_operator_pass_wall_revision',
					metadata: {revision_id: ctx.req.valid('param').revision_id, notified},
				}),
			);
			return ctx.json(mapWallEntry(result.entry, user));
		},
	);
	app.post(
		'/admin/users/:user_id/operator-pass/wall/revisions/:revision_id/decline',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', AdminOperatorPassWallRevisionIdParam),
		Validator('json', AdminOperatorPassWallDecisionRequest),
		OpenAPI({
			operationId: 'decline_admin_operator_pass_wall_revision',
			summary: "Decline Operators' wall revision",
			responseSchema: AdminOperatorPassWallEntryResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Declines a pending wall revision because of its content, with a reason code and the facts. The wall keeps the last approved version or the fallback, and the listing keeps its tier, place and badge. Fluxer emails a statement of reasons with the appeal route under section 6.2 of the terms of service. Records a decision. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const userId = createUserID(BigInt(ctx.req.valid('param').user_id));
			const user = await ctx.get('userRepository').findUniqueAssert(userId);
			const before = await getOperatorPassWallService().getEntry(userId);
			const body = ctx.req.valid('json');
			if (before?.pending_revision_id?.toString() !== ctx.req.valid('param').revision_id) {
				throw new UnknownOperatorPassWallEntryError();
			}
			const result = await getOperatorPassWallService().moderation.decline(userId, ctx.get('adminUserId'), {
				reason: body.reason,
				note: body.note,
				usernameDeclined: body.username_declined === true,
			});
			const notified = await sendWallStatement(ctx, user, result, {
				name: before?.live_name ?? null,
				avatar: before?.live_avatar ?? null,
			});
			await indexWallAudit(
				userId,
				recordAdminWrite(ctx, {
					targetType: 'user',
					targetId: userId,
					action: 'decline_operator_pass_wall_revision',
					metadata: {revision_id: ctx.req.valid('param').revision_id, reason: body.reason, note: body.note, notified},
				}),
			);
			return ctx.json(mapWallEntry(result.entry, user));
		},
	);
	app.post(
		'/admin/users/:user_id/operator-pass/wall/revisions/:revision_id/revert',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', AdminOperatorPassWallRevisionIdParam),
		OpenAPI({
			operationId: 'revert_admin_operator_pass_wall_revision',
			summary: "Revert Operators' wall to revision",
			responseSchema: AdminOperatorPassWallEntryResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Puts an earlier approved revision back on the wall. A business revision cannot be put back this way, so the purchase is checked again whenever a business listing returns. Records a decision. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const userId = createUserID(BigInt(ctx.req.valid('param').user_id));
			const user = await ctx.get('userRepository').findUniqueAssert(userId);
			const result = await getOperatorPassWallService().moderation.revert(
				userId,
				ctx.get('adminUserId'),
				BigInt(ctx.req.valid('param').revision_id),
			);
			const notified = false;
			await indexWallAudit(
				userId,
				recordAdminWrite(ctx, {
					targetType: 'user',
					targetId: userId,
					action: 'revert_operator_pass_wall_revision',
					metadata: {revision_id: ctx.req.valid('param').revision_id, notified},
				}),
			);
			return ctx.json(mapWallEntry(result.entry, user));
		},
	);
	app.post(
		'/admin/users/:user_id/operator-pass/wall/force-fallback',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', UserIdParam),
		Validator('json', AdminOperatorPassWallDecisionRequest),
		OpenAPI({
			operationId: 'force_admin_operator_pass_wall_fallback',
			summary: "Force Operators' wall fallback",
			responseSchema: AdminOperatorPassWallEntryResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Takes the approved name and avatar off the wall because of their content and shows the fallback: the username, or Operator when the username also breaks the rules, with the default avatar. A business listing instead goes back to the last approved personal name and avatar when there is one. The listing keeps its tier, place and badge. Fluxer emails a statement of reasons. Records a decision. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			const user = await ctx.get('userRepository').findUniqueAssert(userId);
			const before = await getOperatorPassWallService().getEntry(userId);
			const body = ctx.req.valid('json');
			const result = await getOperatorPassWallService().moderation.forceFallback(userId, ctx.get('adminUserId'), {
				reason: body.reason,
				note: body.note,
				usernameDeclined: body.username_declined === true,
			});
			const notified = await sendWallStatement(ctx, user, result, {
				name: before?.live_name ?? null,
				avatar: before?.live_avatar ?? null,
			});
			await indexWallAudit(
				userId,
				recordAdminWrite(ctx, {
					targetType: 'user',
					targetId: userId,
					action: 'force_operator_pass_wall_fallback',
					metadata: {reason: body.reason, note: body.note, notified},
				}),
			);
			return ctx.json(mapWallEntry(result.entry, user));
		},
	);
	app.put(
		'/admin/users/:user_id/operator-pass/wall/business',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', UserIdParam),
		Validator('json', AdminOperatorPassWallBusinessRequest),
		OpenAPI({
			operationId: 'set_admin_operator_pass_wall_business_listing',
			summary: "Set Operators' wall business listing",
			responseSchema: AdminOperatorPassWallEntryResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Lists a business on the wall with its company name and logo under section 2.2 of the Operator Pass terms, or replaces its logo. The account must hold a pass and have a purchase with a tax ID, verified when it is an EU VAT number from outside Sweden, paid at least 7 days ago with no refund, open dispute or fraud warning. The logo must be a square PNG, JPEG or WebP image of at least 256 pixels, under 2 MB and not animated. The listing is approved at once, does not follow profile changes, and shows at one fixed size after the listings of people. An approved settings change, a revert or a forced fallback takes it back to a personal listing, and so does a refund, dispute or withdrawal that leaves no qualifying purchase, a section 8 removal, or losing the last pass. Records a decision. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			const user = await ctx.get('userRepository').findUniqueAssert(userId);
			const stripe = ctx.get('stripeService').getStripe();
			if (!stripe) {
				throw new StripePaymentNotAvailableError();
			}
			const purchase = await requireOperatorPassBusinessPurchase({
				stripe,
				repository: getOperatorPassRepository(),
				userId,
				now: operatorPassNow(),
			});
			const body = ctx.req.valid('json');
			const result = await getOperatorPassWallService().setBusinessListing(userId, ctx.get('adminUserId'), {
				name: body.name,
				logo: Buffer.from(body.logo, 'base64'),
			});
			await indexWallAudit(
				userId,
				recordAdminWrite(ctx, {
					targetType: 'user',
					targetId: userId,
					action: 'set_operator_pass_wall_business_listing',
					metadata: {sha256: result.revision?.avatar ?? null, checkout_session_id: purchase.checkout_session_id},
				}),
			);
			return ctx.json(mapWallEntry(result.entry, user));
		},
	);
	app.post(
		'/admin/users/:user_id/operator-pass/wall/hide',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', UserIdParam),
		Validator('json', AdminOperatorPassWallHideRequest),
		OpenAPI({
			operationId: 'hide_admin_operator_pass_wall_entry',
			summary: "Hide or show Operators' wall listing",
			responseSchema: AdminOperatorPassWallEntryResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Hides a listing from the wall for now, or shows it again. Hiding needs a reason and the facts, sends a statement of reasons, stops serving the avatar and purges it from the CDN. Removal from the wall for good is a section 8 removal. Records a decision. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			const user = await ctx.get('userRepository').findUniqueAssert(userId);
			const body = ctx.req.valid('json');
			if (body.hidden && (!body.reason || !body.note)) {
				throw new InputValidationError([
					{path: 'reason', message: 'A reason and the facts are required to hide a listing'},
				]);
			}
			const result = await getOperatorPassWallService().moderation.setHidden(
				userId,
				ctx.get('adminUserId'),
				body.hidden,
				{
					reason: body.reason ?? null,
					note: body.note ?? null,
				},
			);
			const notified = body.hidden ? await sendHiddenStatement(ctx, user, result) : false;
			await indexWallAudit(
				userId,
				recordAdminWrite(ctx, {
					targetType: 'user',
					targetId: userId,
					action: 'hide_operator_pass_wall_entry',
					metadata: {hidden: body.hidden, reason: body.reason ?? null, note: body.note ?? null, notified},
				}),
			);
			return ctx.json(mapWallEntry(result.entry, user));
		},
	);
	app.post(
		'/admin/operator-pass/wall/bulk',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('json', AdminOperatorPassWallBulkRequest),
		OpenAPI({
			operationId: 'bulk_admin_operator_pass_wall_revisions',
			summary: 'Approve or decline wall revisions in bulk',
			responseSchema: AdminOperatorPassWallBulkResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Approves or declines up to 100 pending revisions at once. Declining needs a reason and the facts and sends each holder a statement of reasons. Revisions that are no longer pending, or whose account no longer exists, are skipped. Creates one audit log entry per revision. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const body = ctx.req.valid('json');
			if (body.action === 'decline' && (!body.reason || !body.note)) {
				throw new InputValidationError([{path: 'reason', message: 'A reason and the facts are required to decline'}]);
			}
			const service = getOperatorPassWallService();
			const done: Array<string> = [];
			const skipped: Array<string> = [];
			for (const item of body.revisions) {
				const userId = createUserID(BigInt(item.user_id));
				const before = await service.getEntry(userId);
				if (before?.pending_revision_id?.toString() !== item.revision_id) {
					skipped.push(item.revision_id);
					continue;
				}
				const user = await ctx.get('userRepository').findUnique(userId);
				if (!user) {
					skipped.push(item.revision_id);
					continue;
				}
				let result: OperatorPassWallDecisionResult;
				try {
					result =
						body.action === 'approve'
							? await service.moderation.approve(userId, ctx.get('adminUserId'), BigInt(item.revision_id))
							: await service.moderation.decline(userId, ctx.get('adminUserId'), {
									reason: body.reason!,
									note: body.note!,
									usernameDeclined: false,
								});
				} catch (error) {
					if (error instanceof UnknownOperatorPassWallEntryError) {
						skipped.push(item.revision_id);
						continue;
					}
					throw error;
				}
				const notified =
					body.action === 'decline'
						? await sendWallStatement(ctx, user, result, {
								name: before.live_name ?? null,
								avatar: before.live_avatar ?? null,
							})
						: false;
				await indexWallAudit(
					userId,
					recordAdminWrite(ctx, {
						targetType: 'user',
						targetId: userId,
						action:
							body.action === 'approve' ? 'approve_operator_pass_wall_revision' : 'decline_operator_pass_wall_revision',
						metadata: {revision_id: item.revision_id, bulk: true, reason: body.reason ?? null, notified},
					}),
				);
				done.push(item.revision_id);
			}
			return ctx.json({done, skipped});
		},
	);
	app.get(
		'/admin/users/:user_id/operator-pass/wall/timeline',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'get_admin_operator_pass_wall_timeline',
			summary: "Get Operators' wall timeline",
			responseSchema: AdminOperatorPassWallTimelineResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Returns every wall revision of a holder with its status, every decision and the wall history, including pass count changes. Only available on hosted instances. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			const user = await ctx.get('userRepository').findUnique(userId);
			const timeline = await getOperatorPassWallService().history.timeline(userId);
			await indexWallAudit(
				userId,
				recordAdminRead(ctx, {
					targetType: 'user',
					targetId: userId,
					action: AdminAuditReadActions.GET_OPERATOR_PASS_WALL_TIMELINE,
					metadata: {revision_count: timeline.revisions.length},
				}),
			);
			return ctx.json({
				entry: timeline.entry ? mapWallEntry(timeline.entry, user) : null,
				revisions: timeline.revisions.map(({revision, status}) => ({
					id: revision.revision_id.toString(),
					name: revision.name,
					avatar_url: wallAvatarRoute(userId, revision.avatar),
					business: revision.business ?? false,
					source: revision.source,
					status,
					created_at: revision.created_at.toISOString(),
					created_by: revision.created_by?.toString() ?? null,
				})),
				decisions: timeline.decisions.map(mapWallDecision),
				events: timeline.events.map(mapWallEvent),
			});
		},
	);
	app.get(
		'/admin/users/:user_id/operator-pass/wall/avatars/:sha256',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('param', AdminOperatorPassWallAvatarParam),
		OpenAPI({
			operationId: 'get_admin_operator_pass_wall_avatar',
			summary: "Get private Operators' wall avatar",
			responseSchema: null,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Returns the private copy of a wall avatar snapshot of a holder, including pending and declined ones that are never public. Only available on hosted instances. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const {user_id: rawUserId, sha256} = ctx.req.valid('param');
			const userId = createUserID(BigInt(rawUserId));
			const revisions = await getOperatorPassRepository().listWallRevisions(userId);
			if (!revisions.some((revision) => revision.avatar === sha256)) {
				throw new UnknownOperatorPassWallEntryError();
			}
			const body = await getOperatorPassWallInfrastructure().avatarStore.readPrivate(sha256);
			if (!body) {
				throw new UnknownOperatorPassWallEntryError();
			}
			await indexWallAudit(
				userId,
				recordAdminRead(ctx, {
					targetType: 'user',
					targetId: userId,
					action: AdminAuditReadActions.GET_OPERATOR_PASS_WALL_AVATAR,
					metadata: {sha256},
				}),
			);
			return new Response(body, {
				status: 200,
				headers: {'content-type': 'image/webp', 'cache-control': 'private, no-store'},
			});
		},
	);
	app.get(
		'/admin/operator-pass/wall/as-of',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('query', AdminOperatorPassWallAsOfQuery),
		OpenAPI({
			operationId: 'get_admin_operator_pass_wall_as_of',
			summary: "Render Operators' wall at a time",
			responseSchema: AdminOperatorPassWallAsOfResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Rebuilds the wall as it was at a timestamp from the revisions, decisions and pass count history. A deleted holder shows as a neutral placeholder and never from deleted copies. Only available on hosted instances. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const at = new Date(ctx.req.valid('query').at);
			const entries = await getOperatorPassWallService().history.wallAsOf(at);
			await recordAdminRead(ctx, {
				targetType: 'operator_pass_wall',
				targetId: 0n,
				action: AdminAuditReadActions.GET_OPERATOR_PASS_WALL_AS_OF,
				metadata: {at: at.toISOString(), count: entries.length},
			});
			return ctx.json({
				at: at.toISOString(),
				entries: entries.map((entry) => ({
					user_id: entry.deleted ? null : entry.userId.toString(),
					name: entry.name,
					avatar_url: entry.deleted ? null : wallAvatarRoute(entry.userId, entry.avatar),
					count: entry.count,
					tier: entry.tier,
					first_pass_at: entry.firstPassAt.toISOString(),
					deleted: entry.deleted,
				})),
			});
		},
	);
	app.get(
		'/admin/operator-pass/wall/state',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		OpenAPI({
			operationId: 'get_admin_operator_pass_wall_state',
			summary: "Get Operators' wall publishing state",
			responseSchema: AdminOperatorPassWallStateResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Returns the published wall version and whether publishing is frozen. Only available on hosted instances. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			await recordAdminRead(ctx, {
				targetType: 'operator_pass_wall',
				targetId: 0n,
				action: AdminAuditReadActions.GET_OPERATOR_PASS_WALL_STATE,
				metadata: {},
			});
			return ctx.json(await wallStateResponse());
		},
	);
	app.post(
		'/admin/operator-pass/wall/freeze',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('json', AdminOperatorPassWallFreezeRequest),
		OpenAPI({
			operationId: 'freeze_admin_operator_pass_wall',
			summary: "Freeze or unfreeze Operators' wall",
			responseSchema: AdminOperatorPassWallStateResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Freezing stops publishing and keeps serving the current snapshot. Unfreezing publishes again with a resync. No manual reordering or resizing exists. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const {frozen} = ctx.req.valid('json');
			await getOperatorPassWallService().publication.setFrozen(frozen, ctx.get('adminUserId'));
			await recordAdminWrite(ctx, {
				targetType: 'operator_pass_wall',
				targetId: 0n,
				action: frozen ? 'freeze_operator_pass_wall' : 'unfreeze_operator_pass_wall',
				metadata: {frozen},
			});
			return ctx.json(await wallStateResponse());
		},
	);
	app.post(
		'/admin/operator-pass/wall/republish',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		OpenAPI({
			operationId: 'republish_admin_operator_pass_wall',
			summary: "Re-publish Operators' wall",
			responseSchema: AdminOperatorPassWallStateResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Publishes a new wall version with a resync so every consumer fetches the full snapshot again. Does nothing while frozen. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const version = await getOperatorPassWallService().publication.republish();
			await recordAdminWrite(ctx, {
				targetType: 'operator_pass_wall',
				targetId: 0n,
				action: 'republish_operator_pass_wall',
				metadata: {version},
			});
			return ctx.json(await wallStateResponse());
		},
	);
	app.post(
		'/admin/users/:user_id/operator-pass/wall/legal-hold',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', UserIdParam),
		Validator('json', AdminOperatorPassWallLegalHoldRequest),
		OpenAPI({
			operationId: 'set_admin_operator_pass_wall_legal_hold',
			summary: "Set Operators' wall legal hold",
			responseSchema: AdminOperatorPassWallEntryResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Keeps the name and avatar copies of a holder past the retention periods while an appeal or a legal hold is open, or releases them again. An open payment dispute holds them on its own. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			const user = await ctx.get('userRepository').findUniqueAssert(userId);
			const {held} = ctx.req.valid('json');
			const entry = await getOperatorPassWallService().retention.setLegalHold(userId, ctx.get('adminUserId'), held);
			await indexWallAudit(
				userId,
				recordAdminWrite(ctx, {
					targetType: 'user',
					targetId: userId,
					action: 'set_operator_pass_wall_legal_hold',
					metadata: {held},
				}),
			);
			return ctx.json(mapWallEntry(entry, user));
		},
	);
	app.get(
		'/admin/operator-pass/releases/:year',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('param', AdminOperatorPassReleaseYearParam),
		OpenAPI({
			operationId: 'get_admin_operator_pass_release',
			summary: 'Get The Operators of list',
			responseSchema: AdminOperatorPassReleasePreviewResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Previews the The Operators of list for a year: the cut-off, the publish deadline and the names that go in, or the published list. Only available on hosted instances. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const {year} = ctx.req.valid('param');
			const preview = await getOperatorPassWallService().releases.previewRelease(year);
			await recordAdminRead(ctx, {
				targetType: 'operator_pass_release',
				targetId: BigInt(year),
				action: AdminAuditReadActions.GET_OPERATOR_PASS_RELEASE,
				metadata: {candidate_count: preview.candidates.length},
			});
			return ctx.json({
				year,
				cutoff_at: preview.cutoffAt.toISOString(),
				publish_by: preview.publishBy.toISOString(),
				can_publish: !preview.published && operatorPassNow().getTime() >= preview.cutoffAt.getTime(),
				published: preview.published ? mapRelease(preview.published) : null,
				candidates: preview.candidates.map((candidate) => ({
					user_id: candidate.entry.user_id.toString(),
					name: candidate.name,
					delivered_at: candidate.entry.delivered_at?.toISOString() ?? null,
				})),
			});
		},
	);
	app.post(
		'/admin/operator-pass/releases/:year/publish',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', AdminOperatorPassReleaseYearParam),
		Validator('json', AdminOperatorPassReleasePublishRequest),
		OpenAPI({
			operationId: 'publish_admin_operator_pass_release',
			summary: 'Publish The Operators of list',
			responseSchema: AdminOperatorPassReleaseFileResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Marks the The Operators of list for a year as published in a release, stamps a release event on every name in it and returns the OPERATORS file to commit. Works only after the cut-off. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const {year} = ctx.req.valid('param');
			const body = ctx.req.valid('json');
			const result = await getOperatorPassWallService().releases.publishRelease({
				year,
				tag: body.tag,
				releasedAt: new Date(body.released_at),
				adminId: ctx.get('adminUserId'),
			});
			await recordAdminWrite(ctx, {
				targetType: 'operator_pass_release',
				targetId: BigInt(year),
				action: 'publish_operator_pass_release',
				metadata: {tag: body.tag, name_count: result.release.names.length},
			});
			return ctx.json({release: mapRelease(result.release), operators_file: result.file, tasks: result.tasks});
		},
	);
	app.post(
		'/admin/operator-pass/releases/:year/erase/:user_id',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', AdminOperatorPassReleaseEraseParam),
		OpenAPI({
			operationId: 'erase_admin_operator_pass_release_name',
			summary: 'Erase name from published list',
			responseSchema: AdminOperatorPassReleaseFileResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Erases the name of an account from a published The Operators of list. The homepage feature and future exports leave it out, a history event records it, and the response lists the release notes and OPERATORS file edits to make in the next commit. Creates audit log entry. Only available on hosted instances. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const {year, user_id: rawUserId} = ctx.req.valid('param');
			const userId = createUserID(BigInt(rawUserId));
			const result = await getOperatorPassWallService().releases.eraseFromRelease({
				year,
				userId,
				adminId: ctx.get('adminUserId'),
			});
			await indexWallAudit(
				userId,
				recordAdminWrite(ctx, {
					targetType: 'user',
					targetId: userId,
					action: 'erase_operator_pass_release_name',
					metadata: {year},
				}),
			);
			return ctx.json({release: mapRelease(result.release), operators_file: result.file, tasks: result.tasks});
		},
	);
}
