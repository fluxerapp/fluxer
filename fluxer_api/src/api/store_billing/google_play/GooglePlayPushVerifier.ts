// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {StoreNotificationUnauthorizedError} from '@fluxer/errors/src/domains/payment/StoreNotificationUnauthorizedError';
import {createRemoteJWKSet, type JWTPayload, jwtVerify} from 'jose';
import {z} from 'zod';

export const GOOGLE_OIDC_JWKS_URL = 'https://www.googleapis.com/oauth2/v3/certs';
const GOOGLE_OIDC_ISSUERS: Array<string> = ['https://accounts.google.com', 'accounts.google.com'];
const CLOCK_TOLERANCE_SECONDS = 30;

type RemoteJwks = ReturnType<typeof createRemoteJWKSet>;

export interface GooglePlayPushIdentity {
	email: string;
	subject: string | null;
}

interface GooglePlayPushVerifierOptions {
	jwksUrl?: string;
	now?: () => Date;
}

export class GooglePlayPushVerifier {
	private readonly jwksUrl: string;
	private readonly now: () => Date;
	private jwks: RemoteJwks | null = null;

	constructor(options: GooglePlayPushVerifierOptions = {}) {
		this.jwksUrl = options.jwksUrl ?? GOOGLE_OIDC_JWKS_URL;
		this.now = options.now ?? (() => new Date());
	}

	async verifyAuthorizationHeader(header: string | null | undefined): Promise<GooglePlayPushIdentity> {
		const match = /^Bearer\s+(\S+)$/iu.exec(header?.trim() ?? '');
		if (!match) {
			throw this.reject('missing_bearer');
		}
		const cfg = Config.googlePlay;
		const audience = cfg.pushAudience?.trim();
		const expectedEmail = cfg.pushServiceAccountEmail?.trim().toLowerCase();
		if (!audience || !expectedEmail) {
			throw this.reject('not_configured');
		}
		let payload: JWTPayload;
		try {
			const result = await jwtVerify(match[1], this.getJwks(), {
				algorithms: ['RS256'],
				issuer: GOOGLE_OIDC_ISSUERS,
				audience,
				requiredClaims: ['exp', 'iat'],
				clockTolerance: CLOCK_TOLERANCE_SECONDS,
				currentDate: this.now(),
			});
			payload = result.payload;
		} catch (error) {
			throw this.reject('token_rejected', error);
		}
		const email = payload['email'];
		if (typeof email !== 'string' || email.toLowerCase() !== expectedEmail) {
			throw this.reject('email_mismatch');
		}
		if (payload['email_verified'] !== true) {
			throw this.reject('email_unverified');
		}
		return {email, subject: typeof payload.sub === 'string' ? payload.sub : null};
	}

	private getJwks(): RemoteJwks {
		if (!this.jwks) {
			this.jwks = createRemoteJWKSet(new URL(this.jwksUrl));
		}
		return this.jwks;
	}

	private reject(reason: string, error?: unknown): StoreNotificationUnauthorizedError {
		Logger.warn(
			{reason, error: error instanceof Error ? error.message : undefined},
			'Rejected Google Play push notification',
		);
		return new StoreNotificationUnauthorizedError();
	}
}

const PushEnvelopeSchema = z.object({
	message: z.object({
		data: z.string().optional(),
		messageId: z.string().optional(),
		message_id: z.string().optional(),
		publishTime: z.string().optional(),
	}),
	subscription: z.string().optional(),
});

export interface GooglePlayPushMessage {
	messageId: string;
	data: string;
	subscription: string | null;
	publishTime: string | null;
}

export function parseGooglePlayPushEnvelope(body: unknown): GooglePlayPushMessage | null {
	const parsed = PushEnvelopeSchema.safeParse(body);
	if (!parsed.success) {
		return null;
	}
	const {message, subscription} = parsed.data;
	const messageId = message.messageId ?? message.message_id;
	if (!messageId || !message.data) {
		return null;
	}
	return {
		messageId,
		data: message.data,
		subscription: subscription ?? null,
		publishTime: message.publishTime ?? null,
	};
}

const IntegerSchema = z.union([
	z.number().int(),
	z
		.string()
		.regex(/^-?\d+$/u)
		.transform(Number),
]);

const DeveloperNotificationSchema = z.object({
	version: z.string().optional(),
	packageName: z.string().min(1),
	eventTimeMillis: IntegerSchema.optional(),
	subscriptionNotification: z
		.object({
			version: z.string().optional(),
			notificationType: IntegerSchema,
			purchaseToken: z.string().min(1),
			subscriptionId: z.string().optional(),
		})
		.optional(),
	oneTimeProductNotification: z
		.object({
			version: z.string().optional(),
			notificationType: IntegerSchema,
			purchaseToken: z.string().min(1),
			sku: z.string().optional(),
		})
		.optional(),
	voidedPurchaseNotification: z
		.object({
			purchaseToken: z.string().min(1),
			orderId: z.string().optional(),
			productType: IntegerSchema.optional(),
			refundType: IntegerSchema.optional(),
		})
		.optional(),
	pendingRefundReviewNotification: z
		.object({
			orderId: z.string().optional(),
		})
		.optional(),
	testNotification: z.object({version: z.string().optional()}).optional(),
});

export type GooglePlayNotificationEvent =
	| {type: 'subscription'; notificationType: number; purchaseToken: string}
	| {type: 'one_time_product'; notificationType: number; purchaseToken: string; productId: string | null}
	| {
			type: 'voided_purchase';
			purchaseToken: string;
			orderId: string | null;
			productType: 'subscription' | 'one_time' | null;
			refundType: 'full' | 'quantity_based_partial' | null;
	  }
	| {type: 'pending_refund_review'; orderId: string | null}
	| {type: 'test'}
	| {type: 'unknown'};

export interface GooglePlayDeveloperNotification {
	packageName: string;
	eventTime: Date | null;
	event: GooglePlayNotificationEvent;
}

function toEvent(notification: z.infer<typeof DeveloperNotificationSchema>): GooglePlayNotificationEvent {
	if (notification.subscriptionNotification) {
		return {
			type: 'subscription',
			notificationType: notification.subscriptionNotification.notificationType,
			purchaseToken: notification.subscriptionNotification.purchaseToken,
		};
	}
	if (notification.oneTimeProductNotification) {
		return {
			type: 'one_time_product',
			notificationType: notification.oneTimeProductNotification.notificationType,
			purchaseToken: notification.oneTimeProductNotification.purchaseToken,
			productId: notification.oneTimeProductNotification.sku ?? null,
		};
	}
	if (notification.voidedPurchaseNotification) {
		const voided = notification.voidedPurchaseNotification;
		return {
			type: 'voided_purchase',
			purchaseToken: voided.purchaseToken,
			orderId: voided.orderId ?? null,
			productType: voided.productType === 1 ? 'subscription' : voided.productType === 2 ? 'one_time' : null,
			refundType: voided.refundType === 1 ? 'full' : voided.refundType === 2 ? 'quantity_based_partial' : null,
		};
	}
	if (notification.pendingRefundReviewNotification) {
		return {type: 'pending_refund_review', orderId: notification.pendingRefundReviewNotification.orderId ?? null};
	}
	if (notification.testNotification) {
		return {type: 'test'};
	}
	return {type: 'unknown'};
}

export function decodeGooglePlayDeveloperNotification(data: string): GooglePlayDeveloperNotification | null {
	let json: unknown;
	try {
		json = JSON.parse(Buffer.from(data, 'base64').toString('utf8'));
	} catch {
		return null;
	}
	const parsed = DeveloperNotificationSchema.safeParse(json);
	if (!parsed.success) {
		return null;
	}
	const eventTimeMillis = parsed.data.eventTimeMillis;
	return {
		packageName: parsed.data.packageName,
		eventTime: eventTimeMillis === undefined ? null : new Date(eventTimeMillis),
		event: toEvent(parsed.data),
	};
}
