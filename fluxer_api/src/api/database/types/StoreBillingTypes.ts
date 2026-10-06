// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {
	StoreEnvironment,
	StoreProvider,
	StorePurchaseKind,
	StorePurchaseState,
	StoreSlot,
} from '@app/api/store_billing/StoreBillingTypes';

type Nullish<T> = T | null;

export interface StorePurchaseRow {
	store_key: string;
	id: bigint;
	provider: StoreProvider;
	kind: StorePurchaseKind;
	slot: StoreSlot;
	environment: StoreEnvironment;
	app_id: string;
	product_id: string;
	base_plan_id: Nullish<string>;
	store_reference: string;
	latest_transaction_id: Nullish<string>;
	app_transaction_id: Nullish<string>;
	user_id: Nullish<UserID>;
	bound_at: Nullish<Date>;
	released_at: Nullish<Date>;
	account_token: Nullish<string>;
	ownership_type: Nullish<string>;
	state: StorePurchaseState;
	store_state: Nullish<string>;
	entitled: boolean;
	expires_at: Nullish<Date>;
	grace_ends_at: Nullish<Date>;
	auto_renew: Nullish<boolean>;
	auto_renew_product_id: Nullish<string>;
	started_at: Nullish<Date>;
	purchased_at: Nullish<Date>;
	revoked_at: Nullish<Date>;
	revocation_reason: Nullish<string>;
	linked_store_key: Nullish<string>;
	superseded_by_store_key: Nullish<string>;
	acknowledged: boolean;
	gift_code: Nullish<string>;
	region: Nullish<string>;
	last_event_at: Nullish<Date>;
	synced_at: Nullish<Date>;
	created_at: Date;
	updated_at: Date;
	version: number;
}

export interface StorePurchaseByUserRow {
	user_id: UserID;
	store_key: string;
	created_at: Date;
}

export interface StoreAccountTokenRow {
	token_: string;
	user_id: UserID;
	created_at: Date;
}

export interface StoreAccountTokenByUserRow {
	user_id: UserID;
	token_: string;
	created_at: Date;
}

export const STORE_PURCHASE_COLUMNS = [
	'store_key',
	'id',
	'provider',
	'kind',
	'slot',
	'environment',
	'app_id',
	'product_id',
	'base_plan_id',
	'store_reference',
	'latest_transaction_id',
	'app_transaction_id',
	'user_id',
	'bound_at',
	'released_at',
	'account_token',
	'ownership_type',
	'state',
	'store_state',
	'entitled',
	'expires_at',
	'grace_ends_at',
	'auto_renew',
	'auto_renew_product_id',
	'started_at',
	'purchased_at',
	'revoked_at',
	'revocation_reason',
	'linked_store_key',
	'superseded_by_store_key',
	'acknowledged',
	'gift_code',
	'region',
	'last_event_at',
	'synced_at',
	'created_at',
	'updated_at',
	'version',
] as const satisfies ReadonlyArray<keyof StorePurchaseRow>;

export const STORE_PURCHASE_BY_USER_COLUMNS = ['user_id', 'store_key', 'created_at'] as const satisfies ReadonlyArray<
	keyof StorePurchaseByUserRow
>;

export const STORE_ACCOUNT_TOKEN_COLUMNS = ['token_', 'user_id', 'created_at'] as const satisfies ReadonlyArray<
	keyof StoreAccountTokenRow
>;

export const STORE_ACCOUNT_TOKEN_BY_USER_COLUMNS = ['user_id', 'token_', 'created_at'] as const satisfies ReadonlyArray<
	keyof StoreAccountTokenByUserRow
>;
