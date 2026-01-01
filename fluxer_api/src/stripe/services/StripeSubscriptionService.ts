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

import type Stripe from 'stripe';
import type {UserID} from '~/BrandedTypes';
import {NoActiveSubscriptionError, StripeError, UnknownUserError} from '~/Errors';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import {Logger} from '~/Logger';
import type {User} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import {mapUserToPrivateResponse} from '~/user/UserModel';
import {addMonthsClamp} from '../StripeUtils';

export class StripeSubscriptionService {
	constructor(
		private stripe: Stripe | null,
		private userRepository: IUserRepository,
		private cacheService: ICacheService,
		private gatewayService: IGatewayService,
	) {}

	async cancelSubscriptionAtPeriodEnd(userId: UserID): Promise<void> {
		if (!this.stripe) {
			throw new StripeError('Payment processing is not available');
		}

		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		if (!user.stripeSubscriptionId) {
			throw new StripeError('No active subscription found');
		}

		if (user.premiumWillCancel) {
			throw new StripeError('Subscription is already set to cancel at period end');
		}

		try {
			await this.stripe.subscriptions.update(user.stripeSubscriptionId, {
				cancel_at_period_end: true,
			});

			const updatedUser = await this.userRepository.patchUpsert(userId, {
				premium_will_cancel: true,
			});

			if (updatedUser) {
				await this.dispatchUser(updatedUser);
			}

			Logger.debug({userId, subscriptionId: user.stripeSubscriptionId}, 'Subscription set to cancel at period end');
		} catch (error: unknown) {
			Logger.error(
				{error, userId, subscriptionId: user.stripeSubscriptionId},
				'Failed to cancel subscription at period end',
			);
			const message = error instanceof Error ? error.message : 'Failed to cancel subscription';
			throw new StripeError(message);
		}
	}

	async reactivateSubscription(userId: UserID): Promise<void> {
		if (!this.stripe) {
			throw new StripeError('Payment processing is not available');
		}

		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		if (!user.stripeSubscriptionId) {
			throw new StripeError('No subscription found');
		}

		if (!user.premiumWillCancel) {
			throw new StripeError('Subscription is not set to cancel');
		}

		try {
			await this.stripe.subscriptions.update(user.stripeSubscriptionId, {
				cancel_at_period_end: false,
			});

			const updatedUser = await this.userRepository.patchUpsert(userId, {
				premium_will_cancel: false,
			});

			if (updatedUser) {
				await this.dispatchUser(updatedUser);
			}

			Logger.debug({userId, subscriptionId: user.stripeSubscriptionId}, 'Subscription reactivated');
		} catch (error: unknown) {
			Logger.error({error, userId, subscriptionId: user.stripeSubscriptionId}, 'Failed to reactivate subscription');
			const message = error instanceof Error ? error.message : 'Failed to reactivate subscription';
			throw new StripeError(message);
		}
	}

	async extendSubscriptionWithTrialPhase(user: User, durationMonths: number, idempotencyKey: string): Promise<void> {
		if (!this.stripe || !user.stripeSubscriptionId) {
			throw new NoActiveSubscriptionError();
		}

		const appliedKey = `gift_schedule_applied:${user.id}:${idempotencyKey}`;
		const inflightKey = `gift_schedule_inflight:${user.id}:${idempotencyKey}`;

		if (await this.cacheService.get<boolean>(appliedKey)) {
			Logger.debug({userId: user.id, idempotencyKey}, 'Schedule extension already applied (idempotent hit)');
			return;
		}
		if (await this.cacheService.get<boolean>(inflightKey)) {
			Logger.debug({userId: user.id, idempotencyKey}, 'Schedule extension in-flight; skipping duplicate worker');
			return;
		}
		await this.cacheService.set(inflightKey, 60);

		try {
			const subscription = await this.stripe.subscriptions.retrieve(user.stripeSubscriptionId);
			const baseEndUnix = subscription.items.data[0]?.current_period_end;
			if (!baseEndUnix) {
				throw new StripeError('Subscription current_period_end is missing');
			}

			const baseEnd = new Date(baseEndUnix * 1000);
			const extensionEnd = addMonthsClamp(baseEnd, durationMonths);

			let scheduleId = (subscription as Stripe.Subscription & {schedule?: string | null}).schedule ?? null;
			if (!scheduleId) {
				const created = await this.stripe.subscriptionSchedules.create({
					from_subscription: subscription.id,
					end_behavior: 'release',
				});
				scheduleId = created.id;
				Logger.debug({userId: user.id, scheduleId}, 'Created subscription schedule from subscription');
			}

			const schedule = await this.stripe.subscriptionSchedules.retrieve(scheduleId!, {
				expand: ['phases.items'],
			});

			if ((schedule.phases ?? []).some((p) => p.metadata?.gift_idempotency_key === idempotencyKey)) {
				await this.cacheService.set(appliedKey, 365 * 24 * 60 * 60);
				Logger.debug({userId: user.id, scheduleId, idempotencyKey}, 'Gift phase already present on schedule');
				return;
			}

			const existingPhases = schedule.phases ?? [];
			const lastPhase = existingPhases[existingPhases.length - 1];

			const chainStartUnix: number = (() => {
				const lastEnd = lastPhase?.end_date;
				if (typeof lastEnd === 'number' && lastEnd > baseEndUnix) return lastEnd;
				return baseEndUnix;
			})();

			const chainStart = new Date(chainStartUnix * 1000);
			const finalExtensionEnd =
				chainStartUnix === baseEndUnix ? extensionEnd : addMonthsClamp(chainStart, durationMonths);
			const finalExtensionEndUnix = Math.floor(finalExtensionEnd.getTime() / 1000);

			// NOTE: schedule phases require items[].price and quantity.
			const scheduleItems: Array<Stripe.SubscriptionScheduleCreateParams.Phase.Item> = subscription.items.data.map(
				(it) => ({
					price: (it.price as Stripe.Price).id,
					quantity: it.quantity ?? 1,
				}),
			);

			const newPhases: Array<Stripe.SubscriptionScheduleUpdateParams.Phase> = existingPhases.map((p) => ({
				items: (p.items ?? []).map((i) => ({
					price: typeof i.price === 'string' ? i.price : (i.price as Stripe.Price).id,
					quantity: i.quantity ?? 1,
				})),
				start_date: p.start_date!,
				end_date: p.end_date!,
				proration_behavior: (p.proration_behavior ??
					'none') as Stripe.SubscriptionScheduleUpdateParams.Phase.ProrationBehavior,
				metadata: (p.metadata ?? undefined) as Stripe.MetadataParam | undefined,
			}));

			newPhases.push({
				start_date: chainStartUnix,
				end_date: finalExtensionEndUnix,
				items: scheduleItems,
				trial_end: finalExtensionEndUnix,
				proration_behavior: 'none',
				metadata: {
					gift_idempotency_key: idempotencyKey,
					gift_duration_months: String(durationMonths),
				},
			});

			await this.stripe.subscriptionSchedules.update(scheduleId!, {
				end_behavior: schedule.end_behavior ?? 'release',
				proration_behavior: 'none',
				phases: newPhases,
			});

			await this.cacheService.set(appliedKey, 365 * 24 * 60 * 60);

			Logger.debug(
				{
					userId: user.id,
					scheduleId,
					subscriptionId: subscription.id,
					chainStart: new Date(chainStartUnix * 1000),
					extensionEnd: finalExtensionEnd,
					idempotencyKey,
				},
				'Appended full-trial phase to subscription schedule (Stripe is source of truth)',
			);
		} catch (error: unknown) {
			Logger.error(
				{error, userId: user.id, subscriptionId: user.stripeSubscriptionId, idempotencyKey},
				'Failed to append trial phase to subscription schedule',
			);
			const message = error instanceof Error ? error.message : 'Failed to extend subscription with gift';
			throw new StripeError(message);
		} finally {
			await this.cacheService.delete(inflightKey);
		}
	}

	private async dispatchUser(user: User): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId: user.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(user),
		});
	}
}
