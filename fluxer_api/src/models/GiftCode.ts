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

import type {GiftCodeRow} from '~/database/CassandraTypes';
import type {UserID} from '../BrandedTypes';

export class GiftCode {
	readonly code: string;
	readonly durationMonths: number;
	readonly createdAt: Date;
	readonly createdByUserId: UserID;
	readonly redeemedAt: Date | null;
	readonly redeemedByUserId: UserID | null;
	readonly stripePaymentIntentId: string | null;
	readonly visionarySequenceNumber: number | null;
	readonly checkoutSessionId: string | null;
	readonly version: number;

	constructor(row: GiftCodeRow) {
		this.code = row.code;
		this.durationMonths = row.duration_months;
		this.createdAt = row.created_at;
		this.createdByUserId = row.created_by_user_id as UserID;
		this.redeemedAt = row.redeemed_at ?? null;
		this.redeemedByUserId = row.redeemed_by_user_id ? (row.redeemed_by_user_id as UserID) : null;
		this.stripePaymentIntentId = row.stripe_payment_intent_id ?? null;
		this.visionarySequenceNumber = row.visionary_sequence_number ?? null;
		this.checkoutSessionId = row.checkout_session_id ?? null;
		this.version = row.version;
	}

	toRow(): GiftCodeRow {
		return {
			code: this.code,
			duration_months: this.durationMonths,
			created_at: this.createdAt,
			created_by_user_id: this.createdByUserId,
			redeemed_at: this.redeemedAt,
			redeemed_by_user_id: this.redeemedByUserId,
			stripe_payment_intent_id: this.stripePaymentIntentId,
			visionary_sequence_number: this.visionarySequenceNumber,
			checkout_session_id: this.checkoutSessionId,
			version: this.version,
		};
	}
}
