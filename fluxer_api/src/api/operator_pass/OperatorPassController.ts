// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {getContentMessage, resolveContentLocale} from '@app/api/content_i18n/ContentI18n';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {HostedOnlyRoute} from '@app/api/middleware/BillingRouteAvailability';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {
	getOperatorPassEntitlementService,
	getOperatorPassRepository,
	getOperatorPassWallService,
} from '@app/api/middleware/ServiceSingletons';
import type {OperatorPassCheckoutService} from '@app/api/operator_pass/OperatorPassCheckoutService';
import {
	isOperatorPassRemovalActive,
	resolveOperatorPassHolder,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import type {OperatorPassFulfilmentService} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {getLabsGuildId, getLabsOperatorsChannelId} from '@app/api/operator_pass/OperatorPassLabsRole';
import {mapOperatorPassPurchaseToResponse} from '@app/api/operator_pass/OperatorPassPurchaseRules';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import {
	deleteOperatorPassWaitlistEntry,
	verifyOperatorPassUnsubscribeToken,
} from '@app/api/operator_pass/OperatorPassWaitlistUnsubscribe';
import {
	mapOperatorPassListing,
	normalizeOperatorPassWallChoice,
	parseOperatorPassWallQuote,
} from '@app/api/operator_pass/OperatorPassWallPresentation';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {OperatorPassRateLimitConfigs} from '@app/api/rate_limit_configs/OperatorPassRateLimitConfig';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {lookupGeoip} from '@app/api/utils/IpUtils';
import {getRequestClientIp} from '@app/api/utils/RequestClientIp';
import {Validator} from '@app/api/Validator';
import {OperatorPassNotHeldError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassNotHeldError';
import {OperatorPassWaitlistTokenInvalidError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWaitlistTokenInvalidError';
import {UnknownOperatorPassUnitError} from '@fluxer/errors/src/domains/operator_pass/UnknownOperatorPassUnitError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import {
	OperatorPassCertificateFileResponse,
	OperatorPassCertificateLinkResponse,
	OperatorPassCertificateResponse,
	OperatorPassCertificateTokenParam,
	OperatorPassCheckoutRequest,
	OperatorPassCheckoutResponse,
	OperatorPassLabsGreetingRequest,
	OperatorPassLabsJoinResponse,
	OperatorPassLabsOperatorsRequest,
	OperatorPassLatestReleaseResponse,
	OperatorPassListingResponse,
	OperatorPassListingUpdateRequest,
	OperatorPassOfferAcceptRequest,
	OperatorPassOfferCreateRequest,
	OperatorPassOfferIdParam,
	OperatorPassOfferResponse,
	OperatorPassPassesResponse,
	OperatorPassPassIdParam,
	OperatorPassPurchaseIdParam,
	OperatorPassPurchaseListQuery,
	OperatorPassPurchaseListResponse,
	OperatorPassPurchaseResponse,
	OperatorPassStateQuery,
	OperatorPassStateResponse,
	OperatorPassVisionaryTurnOnRequest,
	OperatorPassWaitlistUnsubscribeQuery,
	OperatorPassWaitlistUnsubscribeResponse,
	OperatorPassWallResponse,
	OperatorPassWithdrawRequest,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {getOperatorPassCertificateFilename} from '@pkgs/email/src/operator_pass_certificate/OperatorPassCertificatePdf';
import type {Context} from 'hono';
import {createMiddleware} from 'hono/factory';

const TAG = 'Operator Pass';

function requireOperatorPassCheckoutService(ctx: Context<HonoEnv>): OperatorPassCheckoutService {
	const service = ctx.get('operatorPassCheckoutService');
	if (!service) {
		throw new StripePaymentNotAvailableError();
	}
	return service;
}

function requireFulfilmentService(ctx: Context<HonoEnv>): OperatorPassFulfilmentService {
	const service = ctx.get('stripeService').operatorPassFulfilment;
	if (!service) {
		throw new StripePaymentNotAvailableError();
	}
	return service;
}

const PrivateCertificateHeaders = createMiddleware<HonoEnv>(async (ctx, next) => {
	await next();
	ctx.res.headers.set('Cache-Control', 'private, no-store');
	ctx.res.headers.set('X-Robots-Tag', 'noindex, nofollow');
});

function certificateFileResponse(file: {filename: string; bytes: Uint8Array}): Response {
	const headers = new Headers();
	headers.set('Content-Type', 'application/pdf');
	headers.set('Content-Disposition', `inline; filename="${encodeURIComponent(file.filename)}"`);
	headers.set('Content-Length', String(file.bytes.byteLength));
	return new Response(file.bytes, {status: 200, headers});
}

export function OperatorPassController(app: HonoApp) {
	app.get(
		'/operator-pass/state',
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_STATE),
		OpenAPI({
			operationId: 'get_operator_pass_state',
			summary: 'Get Operator Pass state',
			description:
				'Returns whether the early bird or the standard price applies, the price of one pass in the buyer currency, the most passes one checkout can buy, the withdrawal rule of the buyer country and a signed quote. Signed-in callers also get their holder status with the number of passes they hold, and why they cannot buy passes.',
			responseSchema: OperatorPassStateResponse,
			statusCode: 200,
			security: [],
			tags: TAG,
		}),
		Validator('query', OperatorPassStateQuery),
		async (ctx) => {
			const {country_code: countryCode} = ctx.req.valid('query');
			const sessionUser = ctx.get('authTokenType') === 'session' ? (ctx.get('user') ?? null) : null;
			const user = sessionUser && !sessionUser.isBot ? sessionUser : null;
			const geoipCountryCode = user || !countryCode ? ((await lookupGeoip(ctx.req.raw)).countryCode ?? null) : null;
			const state = await requireOperatorPassCheckoutService(ctx).getState({
				user,
				countryCode: user ? (geoipCountryCode ?? countryCode ?? null) : (countryCode ?? geoipCountryCode),
			});
			ctx.header('Cache-Control', 'private, no-store');
			return ctx.json(state);
		},
	);
	app.post(
		'/operator-pass/checkout',
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_CHECKOUT),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'create_operator_pass_checkout',
			summary: 'Create Operator Pass checkout session',
			description:
				'Creates a Stripe checkout session in payment mode for one or more Operator Passes and nothing else. Every pass is delivered to the current account as soon as the payment settles. Buying more passes while holding some adds to the count.',
			responseSchema: OperatorPassCheckoutResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('json', OperatorPassCheckoutRequest),
		async (ctx) => {
			const {url, sessionId, expiresAt} = await requireOperatorPassCheckoutService(ctx).createCheckoutSession({
				user: ctx.get('user'),
				request: ctx.req.valid('json'),
				geoipCountryCode: (await lookupGeoip(ctx.req.raw)).countryCode ?? null,
				clientIp: getRequestClientIp(ctx),
				userAgent: ctx.req.header('user-agent') ?? null,
			});
			return ctx.json({url, session_id: sessionId, expires_at: expiresAt.toISOString()});
		},
	);
	app.get(
		'/operator-pass/purchases',
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_PURCHASES),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_operator_pass_purchases',
			summary: 'List Operator Pass purchases',
			description:
				'Lists the Operator Pass purchases the current account paid for, with the quantity, the delivery time and whether the withdraw button for the whole purchase is available.',
			responseSchema: OperatorPassPurchaseListResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('query', OperatorPassPurchaseListQuery),
		async (ctx) => {
			const service = requireFulfilmentService(ctx);
			const userId = ctx.get('user').id;
			const {session_id: sessionId} = ctx.req.valid('query');
			if (sessionId) {
				await service.settleReturnedCheckout(userId, sessionId);
			}
			const purchases = await service.listPurchases(userId);
			const now = operatorPassNow();
			return ctx.json(
				await Promise.all(
					purchases.map(async (purchase) =>
						mapOperatorPassPurchaseToResponse(
							purchase,
							await service.repository.listUnitsByPurchase(purchase.checkout_session_id),
							now,
							await service.certificates.listPurchaseCertificateUrls(purchase),
						),
					),
				),
			);
		},
	);
	app.get(
		'/operator-pass/certificates/:token',
		PrivateCertificateHeaders,
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_CERTIFICATE_LINK),
		OpenAPI({
			operationId: 'get_operator_pass_certificate',
			summary: 'Get an Operator certificate page by its link',
			description:
				'Returns the public details of one Operator certificate page by the token in its link, as the link page on the marketing site shows them. A cancelled page has no name. An unknown token, a token replaced by a new link, or a page whose file was deleted returns 404.',
			responseSchema: OperatorPassCertificateResponse,
			statusCode: 200,
			security: [],
			tags: TAG,
		}),
		Validator('param', OperatorPassCertificateTokenParam),
		async (ctx) => {
			const linked = await requireFulfilmentService(ctx).certificates.loadLinkedPage(ctx.req.valid('param').token);
			if (!linked) {
				throw new UnknownOperatorPassUnitError();
			}
			const {page, cancelled} = linked;
			return ctx.json({
				number: page.number,
				status: cancelled ? 'cancelled' : 'valid',
				name: cancelled ? null : page.holder_name,
				source: page.source,
				paid_at: page.paid_at.toISOString(),
				issued_at: cancelled ? null : page.issued_at.toISOString(),
			} satisfies OperatorPassCertificateResponse);
		},
	);
	app.get(
		'/operator-pass/certificates/:token/pdf',
		PrivateCertificateHeaders,
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_CERTIFICATE_LINK),
		OpenAPI({
			operationId: 'get_operator_pass_certificate_pdf',
			summary: 'Get the PDF of an Operator certificate page by its link',
			description:
				'Returns the newest stored copy of one Operator certificate page as a PDF to show in the browser, by the token in its link. A cancelled page, an unknown token, a token replaced by a new link, or a page whose file was deleted returns 404.',
			responseSchema: OperatorPassCertificateFileResponse,
			responseContentType: 'application/pdf',
			statusCode: 200,
			security: [],
			tags: TAG,
		}),
		Validator('param', OperatorPassCertificateTokenParam),
		async (ctx) => {
			const linked = await requireFulfilmentService(ctx).certificates.loadLinkedPage(ctx.req.valid('param').token);
			if (!linked || linked.cancelled) {
				throw new UnknownOperatorPassUnitError();
			}
			const {page} = linked;
			return certificateFileResponse({
				filename: getOperatorPassCertificateFilename(page.number),
				bytes: new Uint8Array(page.pdf),
			});
		},
	);
	app.post(
		'/operator-pass/passes/:pass_id/certificate-link/reset',
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_CERTIFICATE_LINK_RESET),
		LoginRequired,
		DefaultUserOnly,
		HostedOnlyRoute,
		OpenAPI({
			operationId: 'reset_operator_pass_certificate_link',
			summary: 'Replace the link of an Operator certificate page',
			description:
				'Replaces the private link of a certificate page the current account paid for, or of its Visionary page. Issues a new copy of the page with the new link printed on it, with the same number, payment date and name, and the old link stops working at once. A page of another account, including a pass passed on to the current account, returns 404, and a cancelled page returns 400.',
			responseSchema: OperatorPassCertificateLinkResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('param', OperatorPassPassIdParam),
		async (ctx) => {
			const certificateUrl = await requireFulfilmentService(ctx).resetCertificateLink(
				ctx.get('user'),
				ctx.req.valid('param').pass_id,
			);
			ctx.header('Cache-Control', 'private, no-store');
			return ctx.json({certificate_url: certificateUrl} satisfies OperatorPassCertificateLinkResponse);
		},
	);
	app.post(
		'/operator-pass/purchases/:purchase_id/withdraw',
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_WITHDRAW),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'withdraw_operator_pass_purchase',
			summary: 'Withdraw from an Operator Pass purchase',
			description:
				'Withdraws from a paid Operator Pass purchase while the right of withdrawal lasts: before the certificate is delivered, or after delivery while a country rule keeps the right. Refunds every pass still on the account of the buyer in full to the original payment method, marks their certificate pages cancelled and emails a timestamped receipt to the account email. Single passes are withdrawn with the pass withdraw route.',
			responseSchema: OperatorPassPurchaseResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('param', OperatorPassPurchaseIdParam),
		Validator('json', OperatorPassWithdrawRequest),
		async (ctx) => {
			const {name} = ctx.req.valid('json');
			const service = requireFulfilmentService(ctx);
			const purchase = await service.withdraw({
				purchaser: ctx.get('user'),
				purchaseId: ctx.req.valid('param').purchase_id,
				name,
				channel: 'button',
				receivedAt: operatorPassNow(),
			});
			return ctx.json(
				mapOperatorPassPurchaseToResponse(
					purchase,
					await service.repository.listUnitsByPurchase(purchase.checkout_session_id),
					operatorPassNow(),
					await service.certificates.listPurchaseCertificateUrls(purchase),
				),
			);
		},
	);
	app.get(
		'/operator-pass/waitlist/unsubscribe',
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_WAITLIST_UNSUBSCRIBE),
		OpenAPI({
			operationId: 'open_operator_pass_waitlist_unsubscribe',
			summary: 'Open Operator Pass waitlist unsubscribe page',
			description:
				'Redirects a mail client that opens the unsubscribe link of the Operator Pass launch email to the unsubscribe page. Nothing changes until a mail client sends the one-click POST.',
			responseSchema: null,
			statusCode: 302,
			security: [],
			tags: TAG,
		}),
		(ctx) => ctx.redirect(`${Config.endpoints.marketing}/pass/unsubscribe`, 302),
	);
	app.post(
		'/operator-pass/waitlist/unsubscribe',
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_WAITLIST_UNSUBSCRIBE),
		OpenAPI({
			operationId: 'unsubscribe_operator_pass_waitlist',
			summary: 'Leave Operator Pass waitlist',
			description:
				'Deletes the waitlist entry that the token from the Operator Pass launch email belongs to. Supports one-click unsubscribe from mail clients.',
			responseSchema: OperatorPassWaitlistUnsubscribeResponse,
			statusCode: 200,
			security: [],
			tags: TAG,
		}),
		Validator('query', OperatorPassWaitlistUnsubscribeQuery),
		async (ctx) => {
			const {token} = ctx.req.valid('query');
			const entryId = token ? verifyOperatorPassUnsubscribeToken(token) : null;
			if (!entryId) {
				throw new OperatorPassWaitlistTokenInvalidError();
			}
			await deleteOperatorPassWaitlistEntry(getOperatorPassRepository(), entryId);
			return ctx.json({status: 'unsubscribed' as const}, 200);
		},
	);
	app.get(
		'/operator-pass/listing',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_LISTING),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'get_operator_pass_listing',
			summary: "Get Operators' wall listing",
			description:
				"Returns what the Operators' wall shows for the current account right now, the name it chose and how many passes the account holds.",
			responseSchema: OperatorPassListingResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const entitlement = getOperatorPassEntitlementService();
			const [entry, state] = await Promise.all([
				getOperatorPassWallService().getEntry(user.id),
				entitlement.loadState(user.id),
			]);
			const holder = resolveOperatorPassHolder(state);
			return ctx.json(
				mapOperatorPassListing(entry, holder?.count ?? 0, {removed: isOperatorPassRemovalActive(state.removal)}),
			);
		},
	);
	app.put(
		'/operator-pass/listing',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_LISTING),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'update_operator_pass_listing',
			summary: "Update Operators' wall listing",
			description:
				"Submits a new name for the Operators' wall, takes the listing off the wall, or sets keep_as_is, release_list_opt_in or wall_quote. A new name or quote goes on the wall at once, the name together with the current profile avatar. An already listed holder can leave out wall_name and send only keep_as_is, release_list_opt_in or wall_quote.",
			responseSchema: OperatorPassListingResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('json', OperatorPassListingUpdateRequest),
		async (ctx) => {
			const user = ctx.get('user');
			const holder = await getOperatorPassEntitlementService().getHolder(user);
			if (!holder) {
				throw new OperatorPassNotHeldError();
			}
			const body = ctx.req.valid('json');
			const wallService = getOperatorPassWallService();
			const current = await wallService.getEntry(user.id);
			const choice =
				!body.wall_opt_out && !body.wall_name && current && !current.opted_out
					? {name: null, optedOut: false}
					: normalizeOperatorPassWallChoice({name: body.wall_name, optOut: body.wall_opt_out});
			const quote =
				body.wall_quote === undefined || choice.optedOut ? undefined : parseOperatorPassWallQuote(body.wall_quote);
			await wallService.setChoice(user.id, choice, user.id, {
				keepAsIs: body.keep_as_is,
				releaseListOptIn: body.release_list_opt_in,
				quote,
			});
			return ctx.json(mapOperatorPassListing(await wallService.getEntry(user.id), holder.count));
		},
	);
	app.get(
		'/operator-pass/passes',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_PASSES),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_operator_passes',
			summary: 'List Operator Passes',
			description:
				'Returns how many passes the current account holds, each pass with whether it can be passed on or withdrawn, and the open Pass it on offers to and from the account. The count is returned even when the badge is hidden or the account chose not to be listed.',
			responseSchema: OperatorPassPassesResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		async (ctx) => {
			return ctx.json(await requireFulfilmentService(ctx).holdingService.queries.describePasses(ctx.get('user')));
		},
	);
	app.put(
		'/operator-pass/labs-greeting',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_LISTING),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'update_operator_pass_labs_greeting',
			summary: 'Update the #operators greeting',
			description:
				'Turns the welcome in #operators on or off. When a pass is delivered, the system user welcomes everyone who has it on by name in #operators in Fluxer Labs, soon after and only once. Turning it on after delivery queues the welcome when the account is in Fluxer Labs and has not been welcomed yet. Turning it off before the welcome is sent means no welcome.',
			responseSchema: OperatorPassPassesResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('json', OperatorPassLabsGreetingRequest),
		async (ctx) => {
			const user = ctx.get('user');
			const holding = requireFulfilmentService(ctx).holdingService;
			await holding.updateLabsGreeting(user, ctx.req.valid('json').greeting);
			return ctx.json(await holding.queries.describePasses(user));
		},
	);
	app.put(
		'/operator-pass/labs-operators',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_LISTING),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'update_operator_pass_labs_operators',
			summary: 'Show or hide #operators',
			description:
				'Turns the Fluxer Labs roles that open #operators on or off for the current account. These are the Operator role, the Visionary role and the access role. Turning it off removes them at once, and no later role update adds them back. Turning it on gives back the roles the account is entitled to at once. It does not change passes, badges, the Operators wall, Fluxer Labs membership or other roles.',
			responseSchema: OperatorPassPassesResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('json', OperatorPassLabsOperatorsRequest),
		async (ctx) => {
			const holding = requireFulfilmentService(ctx).holdingService;
			const user = await holding.updateLabsOperatorsChannel(ctx.get('user'), ctx.req.valid('json').show_operators);
			return ctx.json(await holding.queries.describePasses(user));
		},
	);
	app.post(
		'/operator-pass/labs/join',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_LABS_JOIN),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'join_operator_pass_labs',
			summary: 'Join Fluxer Labs with the Operator Pass',
			description:
				'Adds the current account to Fluxer Labs with the Operator role, the same way a delivered pass does, and returns the IDs of the community and #operators. Needs at least one pass. An account that is already in Fluxer Labs gets the Operator role if it is missing, unless it turned off #operators. An account that left Fluxer Labs or was removed from it is never added again and gets status left, so it can join with the invite. A banned account gets status banned.',
			responseSchema: OperatorPassLabsJoinResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		async (ctx) => {
			const status = await requireFulfilmentService(ctx).holdingService.joinLabs(ctx.get('user'));
			if (status === 'not_held') {
				throw new OperatorPassNotHeldError();
			}
			return ctx.json({
				status,
				guild_id: getLabsGuildId()?.toString() ?? null,
				operators_channel_id: getLabsOperatorsChannelId()?.toString() ?? null,
			});
		},
	);
	app.post(
		'/operator-pass/visionary/turn-on',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_CHECKOUT),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'turn_on_operator_pass_visionary',
			summary: 'Turn on the Operator Pass that comes with Visionary',
			description:
				"Turns on the Operator Pass that comes with Visionary for the current account under section 13 of the Operator Pass terms. Needs Visionary, the terms box and the age box, and opens when Operator Pass sales open. Adds one pass that cannot be passed on, dated the day the account got Visionary, and issues its certificate page. The Operator badge stays hidden unless show_badge is true, and the account is listed on the Operators' wall only when it asks to be. The account is added to Fluxer Labs with the Operator role unless it left or was removed from Labs before, and is welcomed in #operators when labs_greeting allows. Nothing is charged. Calling it again returns the same pass and certificate.",
			responseSchema: OperatorPassPassesResponse,
			statusCode: 200,
			security: ['sessionToken'],
			tags: TAG,
		}),
		Validator('json', OperatorPassVisionaryTurnOnRequest),
		async (ctx) => {
			const user = ctx.get('user');
			const request = ctx.req.valid('json');
			const fulfilment = requireFulfilmentService(ctx);
			await fulfilment.visionary.turnOn({
				user,
				request,
				boxText: getContentMessage(
					'billing.operator_pass_visionary_terms_box',
					resolveContentLocale(request.consent_locale ?? user.locale),
				),
				clientIp: getRequestClientIp(ctx),
				userAgent: ctx.req.header('user-agent') ?? null,
			});
			return ctx.json(await fulfilment.holdingService.queries.describePasses(user));
		},
	);
	app.post(
		'/operator-pass/passes/:pass_id/offers',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_OFFER),
		RateLimitMiddleware(RateLimitConfigs.USER_FRIEND_REQUEST_SEND),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'create_operator_pass_offer',
			summary: 'Pass an Operator Pass on',
			description:
				'Offers a pass the current account bought to another user. The offer stays pending until the recipient accepts or declines, and the sender can withdraw it until then. The pass moves as soon as the recipient accepts, and only while the payment has no early fraud warning, open dispute or refund. Passing on the last pass needs confirm_last_pass, because it removes the badge, the wall listing and #operators from the account.',
			responseSchema: OperatorPassOfferResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('param', OperatorPassPassIdParam),
		Validator('json', OperatorPassOfferCreateRequest),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			const fulfilment = requireFulfilmentService(ctx);
			const offer = await fulfilment.createOffer({
				sender: user,
				passId: ctx.req.valid('param').pass_id,
				recipient:
					body.recipient_id != null
						? {id: createUserID(body.recipient_id)}
						: {username: body.username ?? '', discriminator: body.discriminator ?? -1},
				confirmLastPass: body.confirm_last_pass === true,
			});
			return ctx.json(await fulfilment.holdingService.queries.describeOffer(user, offer.offer_id));
		},
	);
	app.post(
		'/operator-pass/passes/:pass_id/withdraw',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_WITHDRAW),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'withdraw_operator_pass',
			summary: 'Withdraw from an Operator Pass',
			description:
				'Withdraws one pass the account bought while a country rule keeps the right of withdrawal after delivery. Refunds the price of that pass in full, ends the pass, marks its certificate page cancelled and emails a timestamped receipt to the account email.',
			responseSchema: OperatorPassPassesResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('param', OperatorPassPassIdParam),
		Validator('json', OperatorPassWithdrawRequest),
		async (ctx) => {
			const user = ctx.get('user');
			const service = requireFulfilmentService(ctx);
			await service.withdrawUnit({
				holder: user,
				passId: ctx.req.valid('param').pass_id,
				name: ctx.req.valid('json').name,
				receivedAt: operatorPassNow(),
			});
			return ctx.json(await service.holdingService.queries.describePasses(user));
		},
	);
	app.get(
		'/operator-pass/offers/:offer_id',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_PASSES),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'get_operator_pass_offer',
			summary: 'Get Pass it on offer',
			description: 'Returns a Pass it on offer the current account made or received.',
			responseSchema: OperatorPassOfferResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('param', OperatorPassOfferIdParam),
		async (ctx) => {
			return ctx.json(
				await requireFulfilmentService(ctx).holdingService.queries.describeOffer(
					ctx.get('user'),
					ctx.req.valid('param').offer_id,
				),
			);
		},
	);
	app.delete(
		'/operator-pass/offers/:offer_id',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_OFFER),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'withdraw_operator_pass_offer',
			summary: 'Withdraw Pass it on offer',
			description:
				'Withdraws an offer the current account made, while the pass has not moved yet. The pass stays with the sender and the recipient gets an email.',
			responseSchema: OperatorPassOfferResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('param', OperatorPassOfferIdParam),
		async (ctx) => {
			const user = ctx.get('user');
			const holding = requireFulfilmentService(ctx).holdingService;
			const offer = await holding.withdrawOffer(user, ctx.req.valid('param').offer_id);
			return ctx.json(await holding.queries.describeOffer(user, offer.offer_id));
		},
	);
	app.post(
		'/operator-pass/offers/:offer_id/accept',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_OFFER),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'accept_operator_pass_offer',
			summary: 'Accept Pass it on offer',
			description:
				"Accepts an offer to the current account with the Operator Pass terms and a choice for the Operators' wall. The pass moves at once. Once it moves, the account is added to Fluxer Labs unless it left or was removed from Labs before, and is welcomed in #operators when labs_greeting allows. A received pass cannot be passed on again, and it ends if the payment for it is reversed.",
			responseSchema: OperatorPassOfferResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('param', OperatorPassOfferIdParam),
		Validator('json', OperatorPassOfferAcceptRequest),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			const holding = requireFulfilmentService(ctx).holdingService;
			const choice =
				body.wall_opt_out == null && !body.wall_name
					? null
					: normalizeOperatorPassWallChoice({name: body.wall_name, optOut: body.wall_opt_out});
			const offer = await holding.acceptOffer({
				recipient: user,
				offerId: ctx.req.valid('param').offer_id,
				acceptedTerms: body.accept_terms,
				choice,
				labsGreeting: body.labs_greeting ?? null,
			});
			return ctx.json(await holding.queries.describeOffer(user, offer.offer_id));
		},
	);
	app.post(
		'/operator-pass/offers/:offer_id/decline',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_OFFER),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'decline_operator_pass_offer',
			summary: 'Decline Pass it on offer',
			description:
				'Declines a pending offer to the current account. The pass stays with the sender, who gets an email.',
			responseSchema: OperatorPassOfferResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('param', OperatorPassOfferIdParam),
		async (ctx) => {
			const user = ctx.get('user');
			const holding = requireFulfilmentService(ctx).holdingService;
			const offer = await holding.declineOffer(user, ctx.req.valid('param').offer_id);
			return ctx.json(await holding.queries.describeOffer(user, offer.offer_id));
		},
	);
	app.get(
		'/operator-pass/wall',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_WALL),
		OpenAPI({
			operationId: 'get_operator_pass_wall',
			summary: "Get Operators' wall",
			description:
				"Lists every Operator on the Operators' wall with the name and avatar the wall shows, the time the listing went up, the time of the first pass, how many passes the Operator holds and the size tier, with the highest tier first and the earliest first pass first within a tier. A listing whose name or avatar staff took off shows the holder's earlier name and avatar, or Operator with the default avatar. Holders who chose not to be listed and paused or removed passes are left out. The response is cached for up to a minute.",
			responseSchema: OperatorPassWallResponse,
			statusCode: 200,
			security: [],
			tags: TAG,
		}),
		async (ctx) => {
			ctx.header('Cache-Control', 'public, max-age=60');
			return ctx.json(await getOperatorPassWallService().publication.listPublicWall());
		},
	);
	app.get(
		'/operator-pass/releases/latest',
		HostedOnlyRoute,
		RateLimitMiddleware(OperatorPassRateLimitConfigs.OPERATOR_PASS_WALL),
		OpenAPI({
			operationId: 'get_operator_pass_latest_release',
			summary: 'Get latest Operators list',
			description:
				'Returns the latest published The Operators of list, with the release tag, the release date and the sorted names, for the homepage feature.',
			responseSchema: OperatorPassLatestReleaseResponse,
			statusCode: 200,
			security: [],
			tags: TAG,
		}),
		async (ctx) => {
			ctx.header('Cache-Control', 'public, max-age=300');
			return ctx.json({release: await getOperatorPassWallService().releases.latestRelease()});
		},
	);
}
