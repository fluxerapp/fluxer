// SPDX-License-Identifier: AGPL-3.0-or-later

export interface FederationOutboxRow {
	target_instance_id: string;
	event_id: string;
	origin: string;
	seq: bigint;
	type: string;
	payload: string;
	created_at: Date;
	next_attempt_at: Date;
	attempts: number;
	last_error: string | null;
	last_attempt_at: Date | null;
}

export const FEDERATION_OUTBOX_COLUMNS = [
	'target_instance_id',
	'event_id',
	'origin',
	'seq',
	'type',
	'payload',
	'created_at',
	'next_attempt_at',
	'attempts',
	'last_error',
	'last_attempt_at',
] as const satisfies ReadonlyArray<keyof FederationOutboxRow>;

export interface FederationInboxRow {
	event_id: string;
	origin: string;
	seq: bigint;
	type: string;
	payload: string;
	payload_hash: string;
	prev_hash: string | null;
	signature: string;
	key_id: string;
	occurred_at: Date;
	received_at: Date;
	status: 'received' | 'applied' | 'rejected';
	error_reason: string | null;
	applied_at: Date | null;
}

export const FEDERATION_INBOX_COLUMNS = [
	'event_id',
	'origin',
	'seq',
	'type',
	'payload',
	'payload_hash',
	'prev_hash',
	'signature',
	'key_id',
	'occurred_at',
	'received_at',
	'status',
	'error_reason',
	'applied_at',
] as const satisfies ReadonlyArray<keyof FederationInboxRow>;
