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

import crypto from 'node:crypto';
import type {UserID} from '@fluxer/api/src/BrandedTypes';
import type {GatewayDispatchEvent} from '@fluxer/api/src/constants/Gateway';
import type {UserRow} from '@fluxer/api/src/database/types/UserTypes';
import {getMetricsService} from '@fluxer/api/src/infrastructure/MetricsService';
import {Logger} from '@fluxer/api/src/Logger';
import type {User} from '@fluxer/api/src/models/User';
import {mapUserToPrivateResponse} from '@fluxer/api/src/user/UserMappers';
import {SuspiciousActivityFlags} from '@fluxer/constants/src/UserConstants';

export interface ISweegoUserRepository {
	findByEmail(email: string): Promise<User | null>;
	patchUpsert(userId: UserID, patch: Partial<UserRow>, currentRow: UserRow): Promise<User | null>;
}

export interface ISweegoGatewayService {
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

export interface SweegoEvent {
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
	}): Promise<{status: number; body: string | null}> {
		const {body, webhookId, timestamp, signature, secret} = params;

		if (secret) {
			if (!webhookId || !timestamp || !signature) {
				getMetricsService().counter({name: 'fluxer.sweego.webhooks.rejected', value: 1});
				Logger.warn('Sweego webhook missing signature headers');
				return {status: 401, body: 'Missing signature headers'};
			}

			const isValid = this.verifySignature(body, webhookId, timestamp, signature, secret);
			if (!isValid) {
				getMetricsService().counter({name: 'fluxer.sweego.webhooks.rejected', value: 1});
				Logger.warn('Sweego webhook signature verification failed');
				return {status: 401, body: 'Invalid signature'};
			}
		}

		let event: SweegoEvent;
		try {
			event = JSON.parse(body) as SweegoEvent;
		} catch (parseError) {
			getMetricsService().counter({name: 'fluxer.sweego.webhooks.invalid_json', value: 1});
			Logger.error({parseError, body: body.slice(0, 1000)}, 'Failed to parse Sweego webhook JSON body');
			return {status: 400, body: 'Invalid JSON'};
		}

		await this.processEvent(event);
		getMetricsService().counter({name: 'fluxer.sweego.webhooks.processed', value: 1});

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
