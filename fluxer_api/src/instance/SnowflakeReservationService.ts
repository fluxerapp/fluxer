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

import type {Redis} from 'ioredis';
import {SNOWFLAKE_RESERVATION_REFRESH_CHANNEL} from '~/constants/InstanceConfig';
import {Logger} from '~/Logger';
import type {SnowflakeReservationConfig, SnowflakeReservationRepository} from './SnowflakeReservationRepository';

export class SnowflakeReservationService {
	private reservations = new Map<string, bigint>();
	private initialized = false;
	private reloadPromise: Promise<void> | null = null;

	constructor(
		private repository: SnowflakeReservationRepository,
		private redisSubscriber: Redis | null,
	) {}

	async initialize(): Promise<void> {
		if (this.initialized) {
			return;
		}

		await this.reload();
		this.initialized = true;

		if (this.redisSubscriber) {
			try {
				await this.redisSubscriber.subscribe(SNOWFLAKE_RESERVATION_REFRESH_CHANNEL);
				this.redisSubscriber.on('message', (channel) => {
					if (channel === SNOWFLAKE_RESERVATION_REFRESH_CHANNEL) {
						this.reload().catch((error) => {
							Logger.error({error}, 'Failed to reload snowflake reservations');
						});
					}
				});
			} catch (error) {
				Logger.error({error}, 'Failed to subscribe to snowflake reservation refresh channel');
			}
		}
	}

	async reload(): Promise<void> {
		if (this.reloadPromise) {
			return this.reloadPromise;
		}

		this.reloadPromise = (async () => {
			const entries = await this.repository.listReservations();
			this.reservations = this.buildLookup(entries);
		})()
			.catch((error) => {
				Logger.error({error}, 'Failed to reload snowflake reservations from the database');
				throw error;
			})
			.finally(() => {
				this.reloadPromise = null;
			});

		return this.reloadPromise;
	}

	getReservedSnowflake(emailKey: string | null): bigint | null {
		if (!emailKey) {
			return null;
		}
		return this.reservations.get(emailKey) ?? null;
	}

	private buildLookup(entries: Array<SnowflakeReservationConfig>): Map<string, bigint> {
		const lookup = new Map<string, bigint>();
		for (const entry of entries) {
			lookup.set(entry.emailKey, entry.snowflake);
		}
		return lookup;
	}
}
