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
import {SuspiciousActivityFlags} from '~/Constants';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import {Logger} from '~/Logger';
import type {IUserRepository} from '~/user/IUserRepository';
import {mapUserToPrivateResponse} from '~/user/UserModel';

interface SendGridEvent {
	email: string;
	event:
		| 'bounce'
		| 'dropped'
		| 'delivered'
		| 'processed'
		| 'deferred'
		| 'open'
		| 'click'
		| 'spamreport'
		| 'unsubscribe'
		| 'group_unsubscribe'
		| 'group_resubscribe';
	timestamp: number;
	sg_event_id: string;
	sg_message_id?: string;
	reason?: string;
	status?: string;
	type?: 'bounce' | 'blocked';
	bounce_classification?: string;
}

export class SendGridWebhookService {
	constructor(
		private readonly userRepository: IUserRepository,
		private readonly gatewayService: IGatewayService,
	) {}

	verifySignature(payload: string, signature: string, timestamp: string, publicKey: string): boolean {
		try {
			const signedPayload = timestamp + payload;

			const verifier = crypto.createVerify('sha256');
			verifier.update(signedPayload);
			verifier.end();

			return verifier.verify(publicKey, signature, 'base64');
		} catch (error) {
			Logger.error({error}, 'Error verifying SendGrid webhook signature');
			return false;
		}
	}

	async processEvents(events: Array<SendGridEvent>): Promise<void> {
		for (const event of events) {
			try {
				await this.processEvent(event);
			} catch (error) {
				Logger.error({error, event}, 'Error processing SendGrid webhook event');
			}
		}
	}

	private async processEvent(event: SendGridEvent): Promise<void> {
		if (event.event !== 'bounce' && event.event !== 'dropped') {
			Logger.debug({event: event.event, email: event.email}, 'SendGrid event received (not bounce/dropped)');
			return;
		}

		if (event.event === 'bounce') {
			if (event.type === 'blocked') {
				Logger.info(
					{email: event.email, reason: event.reason, type: event.type},
					'SendGrid soft bounce (blocked) - temporary failure, will retry',
				);
				return;
			}

			if (event.type === 'bounce') {
				await this.handleHardBounce(event);
				return;
			}
		}

		if (event.event === 'dropped') {
			const reason = event.reason?.toLowerCase() || '';
			if (reason.includes('bounced') || reason.includes('invalid')) {
				await this.handleHardBounce(event);
				return;
			}

			Logger.info(
				{email: event.email, reason: event.reason},
				'SendGrid dropped event - not marking as bounced (non-delivery reason)',
			);
		}
	}

	private async handleHardBounce(event: SendGridEvent): Promise<void> {
		Logger.warn(
			{
				email: event.email,
				event: event.event,
				reason: event.reason,
				bounce_classification: event.bounce_classification,
				sg_event_id: event.sg_event_id,
			},
			'Processing hard bounce - marking email as invalid',
		);

		const user = await this.userRepository.findByEmail(event.email);
		if (!user) {
			Logger.warn({email: event.email}, 'User not found for bounced email');
			return;
		}

		if (user.emailBounced) {
			Logger.debug({userId: user.id, email: event.email}, 'Email already marked as bounced');
			return;
		}

		const currentFlags = user.suspiciousActivityFlags || 0;
		const newFlags = currentFlags | SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL;

		await this.userRepository.patchUpsert(user.id, {
			email_bounced: true,
			email_verified: false,
			suspicious_activity_flags: newFlags,
		});

		Logger.info(
			{userId: user.id, email: event.email, reason: event.reason},
			'User email marked as bounced and requires reverification',
		);

		const updatedUser = await this.userRepository.findUnique(user.id);
		if (updatedUser) {
			await this.gatewayService.dispatchPresence({
				userId: updatedUser.id,
				event: 'USER_UPDATE',
				data: mapUserToPrivateResponse(updatedUser),
			});
		}
	}
}
