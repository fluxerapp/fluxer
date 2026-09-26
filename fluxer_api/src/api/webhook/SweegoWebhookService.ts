// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';
import type {GatewayDispatchEvent} from '@app/api/constants/Gateway';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {isJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';
import {SuspiciousActivityFlags} from '@fluxer/constants/src/UserConstants';

interface ISweegoUserRepository {
	findByEmail(email: string): Promise<User | null>;
	patchUpsert(userId: UserID, patch: Partial<UserRow>, currentRow: UserRow): Promise<User | null>;
}

interface ISweegoGatewayService {
	dispatchPresence(params: {userId: UserID; event: GatewayDispatchEvent; data: unknown}): Promise<void>;
}

type SweegoEventType =
	| 'email_sent'
	| 'delivered'
	| 'soft-bounce'
	| 'hard_bounce'
	| 'list_unsub'
	| 'complaint'
	| 'email_clicked'
	| 'email_opened'
	| string;

interface SweegoEvent {
	event_type: SweegoEventType;
	timestamp: string;
	swg_uid?: string;
	event_id?: string;
	details?: string;
	channel?: string;
	transaction_id?: string;
	recipient: string;
	domain_from?: string;
	campaign_type?: string;
	campaign_id?: string;
}

function isOptionalString(value: unknown): boolean {
	return value === undefined || typeof value === 'string';
}

function isSweegoEvent(value: unknown): value is SweegoEvent {
	if (!isJsonRecord(value)) return false;
	return (
		typeof value['event_type'] === 'string' &&
		typeof value['timestamp'] === 'string' &&
		typeof value['recipient'] === 'string' &&
		isOptionalString(value['swg_uid']) &&
		isOptionalString(value['event_id']) &&
		isOptionalString(value['details']) &&
		isOptionalString(value['channel']) &&
		isOptionalString(value['transaction_id']) &&
		isOptionalString(value['domain_from']) &&
		isOptionalString(value['campaign_type']) &&
		isOptionalString(value['campaign_id'])
	);
}

export class SweegoWebhookService {
	constructor(
		private readonly userRepository: ISweegoUserRepository,
		private readonly gatewayService: ISweegoGatewayService,
	) {}

	verifySignature(body: string, webhookId: string, timestamp: string, signature: string, secret: string): boolean {
		try {
			const secretBytes = Buffer.from(secret, 'base64');
			const contentToSign = `${webhookId}.${timestamp}.${body}`;
			const digest = crypto.createHmac('sha256', secretBytes).update(contentToSign).digest();
			const computedSignature = digest.toString('base64');
			const computedBuffer = Buffer.from(computedSignature);
			const receivedBuffer = Buffer.from(signature);
			if (computedBuffer.length !== receivedBuffer.length) {
				return false;
			}
			return crypto.timingSafeEqual(computedBuffer, receivedBuffer);
		} catch (error) {
			Logger.error({error}, 'Error verifying Sweego webhook signature');
			return false;
		}
	}

	async handleWebhook(params: {
		body: string;
		webhookId?: string;
		timestamp?: string;
		signature?: string;
		secret?: string | null;
	}): Promise<{
		status: number;
		body: string | null;
	}> {
		const {body, webhookId, timestamp, signature, secret} = params;
		if (!secret) {
			Logger.error('Sweego webhook secret is not configured');
			return {status: 503, body: 'Sweego webhook secret not configured'};
		}
		if (!webhookId || !timestamp || !signature) {
			Logger.warn('Sweego webhook missing signature headers');
			return {status: 401, body: 'Missing signature headers'};
		}
		const isValid = this.verifySignature(body, webhookId, timestamp, signature, secret);
		if (!isValid) {
			Logger.warn('Sweego webhook signature verification failed');
			return {status: 401, body: 'Invalid signature'};
		}
		const event = parseJsonWithGuard(body, isSweegoEvent);
		if (!event) {
			Logger.error({body: body.slice(0, 1000)}, 'Failed to parse Sweego webhook JSON body');
			return {status: 400, body: 'Invalid JSON'};
		}
		await this.processEvent(event);
		return {status: 200, body: null};
	}

	async processEvent(event: SweegoEvent): Promise<void> {
		if (event.event_type !== 'soft-bounce' && event.event_type !== 'hard_bounce' && event.event_type !== 'complaint') {
			Logger.debug({eventType: event.event_type, recipient: event.recipient}, 'Sweego event received (ignored)');
			return;
		}
		if (event.event_type === 'hard_bounce' || event.event_type === 'complaint') {
			await this.handleHardBounceOrComplaint(event);
			return;
		}
		Logger.info(
			{recipient: event.recipient, details: event.details, eventType: event.event_type},
			'Sweego soft bounce received',
		);
	}

	private async handleHardBounceOrComplaint(event: SweegoEvent): Promise<void> {
		Logger.warn(
			{
				recipient: event.recipient,
				eventType: event.event_type,
				details: event.details,
				eventId: event.event_id,
			},
			'Processing hard bounce or complaint - marking email as invalid',
		);
		const user = await this.userRepository.findByEmail(event.recipient);
		if (!user) {
			Logger.warn({recipient: event.recipient}, 'User not found for bounced email');
			return;
		}
		if (user.emailBounced) {
			Logger.debug({userId: user.id, recipient: event.recipient}, 'Email already marked as bounced');
			return;
		}
		const currentFlags = user.suspiciousActivityFlags || 0;
		const newFlags = currentFlags | SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL;
		const updatedUser = await this.userRepository.patchUpsert(
			user.id,
			{
				email_bounced: true,
				email_verified: false,
				suspicious_activity_flags: newFlags,
			},
			user.toRow(),
		);
		Logger.info(
			{userId: user.id, recipient: event.recipient, details: event.details},
			'User email marked as bounced and requires reverification',
		);
		if (updatedUser) {
			await this.gatewayService.dispatchPresence({
				userId: updatedUser.id,
				event: 'USER_UPDATE',
				data: mapUserToPrivateResponse(updatedUser),
			});
		}
	}
}
