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

import type {UserID} from '~/BrandedTypes';
import {fetchMany, fetchOne} from '~/database/Cassandra';
import type {
	UserByEmailRow,
	UserByPhoneRow,
	UserByStripeCustomerIdRow,
	UserByStripeSubscriptionIdRow,
	UserByUsernameRow,
} from '~/database/CassandraTypes';
import type {User} from '~/Models';
import {UserByEmail, UserByPhone, UserByStripeCustomerId, UserByStripeSubscriptionId, UserByUsername} from '~/Tables';

const FETCH_DISCRIMINATORS_BY_USERNAME_QUERY = UserByUsername.select({
	columns: ['discriminator', 'user_id'],
	where: UserByUsername.where.eq('username'),
});

const FETCH_USER_ID_BY_EMAIL_QUERY = UserByEmail.select({
	columns: ['user_id'],
	where: UserByEmail.where.eq('email_lower'),
	limit: 1,
});

const FETCH_USER_ID_BY_PHONE_QUERY = UserByPhone.select({
	columns: ['user_id'],
	where: UserByPhone.where.eq('phone'),
	limit: 1,
});

const FETCH_USER_ID_BY_STRIPE_CUSTOMER_ID_QUERY = UserByStripeCustomerId.select({
	columns: ['user_id'],
	where: UserByStripeCustomerId.where.eq('stripe_customer_id'),
	limit: 1,
});

const FETCH_USER_ID_BY_STRIPE_SUBSCRIPTION_ID_QUERY = UserByStripeSubscriptionId.select({
	columns: ['user_id'],
	where: UserByStripeSubscriptionId.where.eq('stripe_subscription_id'),
	limit: 1,
});

const FETCH_USER_ID_BY_USERNAME_DISCRIMINATOR_QUERY = UserByUsername.select({
	columns: ['user_id'],
	where: [UserByUsername.where.eq('username'), UserByUsername.where.eq('discriminator')],
	limit: 1,
});

export class UserLookupRepository {
	constructor(private findUniqueUser: (userId: UserID) => Promise<User | null>) {}

	async findByEmail(email: string): Promise<User | null> {
		const emailLower = email.toLowerCase();
		const result = await fetchOne<Pick<UserByEmailRow, 'user_id'>>(
			FETCH_USER_ID_BY_EMAIL_QUERY.bind({email_lower: emailLower}),
		);
		if (!result) return null;
		return await this.findUniqueUser(result.user_id);
	}

	async findByPhone(phone: string): Promise<User | null> {
		const result = await fetchOne<Pick<UserByPhoneRow, 'user_id'>>(FETCH_USER_ID_BY_PHONE_QUERY.bind({phone}));
		if (!result) return null;
		return await this.findUniqueUser(result.user_id);
	}

	async findByStripeCustomerId(stripeCustomerId: string): Promise<User | null> {
		const result = await fetchOne<Pick<UserByStripeCustomerIdRow, 'user_id'>>(
			FETCH_USER_ID_BY_STRIPE_CUSTOMER_ID_QUERY.bind({stripe_customer_id: stripeCustomerId}),
		);
		if (!result) return null;
		return await this.findUniqueUser(result.user_id);
	}

	async findByStripeSubscriptionId(stripeSubscriptionId: string): Promise<User | null> {
		const result = await fetchOne<Pick<UserByStripeSubscriptionIdRow, 'user_id'>>(
			FETCH_USER_ID_BY_STRIPE_SUBSCRIPTION_ID_QUERY.bind({stripe_subscription_id: stripeSubscriptionId}),
		);
		if (!result) return null;
		return await this.findUniqueUser(result.user_id);
	}

	async findByUsernameDiscriminator(username: string, discriminator: number): Promise<User | null> {
		const usernameLower = username.toLowerCase();
		const result = await fetchOne<Pick<UserByUsernameRow, 'user_id'>>(
			FETCH_USER_ID_BY_USERNAME_DISCRIMINATOR_QUERY.bind({username: usernameLower, discriminator}),
		);
		if (!result) return null;
		return await this.findUniqueUser(result.user_id);
	}

	async findDiscriminatorsByUsername(username: string): Promise<Set<number>> {
		const usernameLower = username.toLowerCase();
		const result = await fetchMany<Pick<UserByUsernameRow, 'discriminator'>>(
			FETCH_DISCRIMINATORS_BY_USERNAME_QUERY.bind({username: usernameLower}),
		);
		return new Set(result.map((r) => r.discriminator));
	}
}
