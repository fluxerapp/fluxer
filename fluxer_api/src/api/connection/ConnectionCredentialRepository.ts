import {randomUUID} from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';
import type {BlueskyOAuthGrantOwner} from '@app/api/bluesky/IBlueskyOAuthService';
import {
	CONNECTION_WRITE_ATTEMPTS,
	ConnectionOwnerClosedError,
	ConnectionWriteConflictError,
	connectionMembershipPatch,
	createPrivateConnectionRow,
	loadConnectionMembership,
} from '@app/api/connection/ConnectionMembership';
import {executeConditional, fetchMany} from '@app/api/database/CassandraQueryExecution';
import {type ConditionalWriteEntry, Db, validateTtlSeconds} from '@app/api/database/CassandraTypes';
import {USER_CONNECTION_CREDENTIAL_TYPE, type UserConnectionStorageRow} from '@app/api/database/types/ConnectionTypes';
import {UserConnections} from '@app/api/Tables';
import {isAtprotoDid} from '@bluesky-social/oauth-client-node';
import {z} from 'zod';

type CredentialWrite = ConditionalWriteEntry<UserConnectionStorageRow, 'user_id' | 'connection_type' | 'connection_id'>;

const CredentialPayload = z.discriminatedUnion('phase', [
	z.strictObject({phase: z.literal('pending'), stateKey: z.string().min(1), payload: z.string().min(1)}),
	z.strictObject({phase: z.literal('consumed'), stateKey: z.string().min(1)}),
	z.strictObject({
		phase: z.literal('active'),
		did: z.string().refine(isAtprotoDid),
		payload: z.string().min(1),
	}),
]);
type CredentialPayload = z.infer<typeof CredentialPayload>;

interface CredentialSnapshot {
	revision: string;
	expiresAt: Date;
	value: CredentialPayload;
}

interface CredentialOperation<T> {
	result: T;
	write?: CredentialWrite;
}

const FETCH_CREDENTIAL_CQL = UserConnections.selectCql({
	where: [
		UserConnections.where.eq('user_id'),
		UserConnections.where.eq('connection_type'),
		UserConnections.where.eq('connection_id'),
	],
	limit: 1,
});

class ConnectionCredentialUnavailableError extends Error {
	constructor() {
		super('Connection credentials are no longer available');
	}
}

export function connectionCredentialKey(owner: BlueskyOAuthGrantOwner): {
	user_id: UserID;
	connection_type: string;
	connection_id: string;
} {
	if (typeof owner.userId !== 'bigint' || !z.uuid().safeParse(owner.grantId).success) {
		throw new Error('Connection credentials have an invalid owner');
	}
	return {
		user_id: owner.userId,
		connection_type: USER_CONNECTION_CREDENTIAL_TYPE,
		connection_id: owner.grantId,
	};
}

export async function readConnectionCredential(
	owner: BlueskyOAuthGrantOwner,
): Promise<UserConnectionStorageRow | null> {
	const key = connectionCredentialKey(owner);
	const [row] = await fetchMany<UserConnectionStorageRow>(FETCH_CREDENTIAL_CQL, key, {consistency: 'serial'});
	return row ?? null;
}

function credentialExpiry(ttlSeconds: number): Date {
	validateTtlSeconds(ttlSeconds);
	if (ttlSeconds === 0) throw new Error('Connection credentials require a positive expiry');
	return new Date(Date.now() + ttlSeconds * 1000);
}

function credentialPatch(
	owner: BlueskyOAuthGrantOwner,
	snapshot: CredentialSnapshot,
	value: CredentialPayload,
	expiresAt: Date,
): CredentialWrite {
	const ttlSeconds = Math.ceil((expiresAt.getTime() - Date.now()) / 1000);
	if (ttlSeconds <= 0) throw new ConnectionCredentialUnavailableError();
	return {
		action: 'patch',
		pk: connectionCredentialKey(owner),
		patch: {
			revision: Db.set(randomUUID()),
			credential_payload: Db.set(JSON.stringify(value)),
			credential_expires_at: Db.set(expiresAt),
		},
		expected: {revision: snapshot.revision},
		ttlSeconds,
	};
}

export class ConnectionCredentialRepository {
	async createState(
		owner: BlueskyOAuthGrantOwner,
		stateKey: string,
		payload: string,
		ttlSeconds: number,
	): Promise<void> {
		const expiresAt = credentialExpiry(ttlSeconds);
		const value = CredentialPayload.parse({phase: 'pending', stateKey, payload});
		await this.withOwner(owner, (snapshot) => {
			if (snapshot) throw new ConnectionWriteConflictError();
			const remainingTtl = Math.ceil((expiresAt.getTime() - Date.now()) / 1000);
			if (remainingTtl <= 0) throw new ConnectionCredentialUnavailableError();
			return {
				result: undefined,
				write: {
					action: 'insert',
					row: {
						...createPrivateConnectionRow(owner.userId, USER_CONNECTION_CREDENTIAL_TYPE, owner.grantId),
						credential_payload: JSON.stringify(value),
						credential_expires_at: expiresAt,
					},
					ttlSeconds: remainingTtl,
				},
			};
		});
	}

	async consumeState(owner: BlueskyOAuthGrantOwner, stateKey: string): Promise<string | undefined> {
		const state = await this.withOwner<{payload: string; expiresAt: Date} | undefined>(owner, (snapshot) => {
			if (snapshot?.value.phase !== 'pending' || snapshot.value.stateKey !== stateKey) {
				return {result: undefined};
			}
			return {
				result: {payload: snapshot.value.payload, expiresAt: snapshot.expiresAt},
				write: credentialPatch(owner, snapshot, {phase: 'consumed', stateKey}, snapshot.expiresAt),
			};
		});
		return state && state.expiresAt.getTime() > Date.now() ? state.payload : undefined;
	}

	async discardPendingState(owner: BlueskyOAuthGrantOwner, stateKey: string): Promise<void> {
		await this.cleanup(owner, (snapshot) => ({
			result: undefined,
			write:
				snapshot?.value.phase === 'pending' && snapshot.value.stateKey === stateKey
					? {action: 'delete', pk: connectionCredentialKey(owner), expected: {revision: snapshot.revision}}
					: undefined,
		}));
	}

	async getSession(owner: BlueskyOAuthGrantOwner, did: string): Promise<string | undefined> {
		const session = await this.withOwner(owner, (snapshot) => ({
			result:
				snapshot?.value.phase === 'active' && snapshot.value.did === did
					? {payload: snapshot.value.payload, expiresAt: snapshot.expiresAt}
					: undefined,
		}));
		return session && session.expiresAt.getTime() > Date.now() ? session.payload : undefined;
	}

	async setSession(owner: BlueskyOAuthGrantOwner, did: string, payload: string, ttlSeconds: number): Promise<void> {
		const expiresAt = credentialExpiry(ttlSeconds);
		const value = CredentialPayload.parse({phase: 'active', did, payload});
		await this.withOwner(owner, (snapshot) => {
			if (
				!snapshot ||
				snapshot.value.phase === 'pending' ||
				(snapshot.value.phase === 'active' && snapshot.value.did !== did)
			) {
				throw new ConnectionCredentialUnavailableError();
			}
			return {result: undefined, write: credentialPatch(owner, snapshot, value, expiresAt)};
		});
	}

	async deleteSession(owner: BlueskyOAuthGrantOwner, did: string): Promise<void> {
		await this.cleanup(owner, (snapshot) => ({
			result: undefined,
			write:
				snapshot?.value.phase === 'active' && snapshot.value.did === did
					? {action: 'delete', pk: connectionCredentialKey(owner), expected: {revision: snapshot.revision}}
					: undefined,
		}));
	}

	private async cleanup(
		owner: BlueskyOAuthGrantOwner,
		operation: (snapshot: CredentialSnapshot | null) => CredentialOperation<void>,
	): Promise<void> {
		try {
			await this.withOwner(owner, operation);
		} catch (error) {
			if (!(error instanceof ConnectionOwnerClosedError)) throw error;
		}
	}

	private async withOwner<T>(
		owner: BlueskyOAuthGrantOwner,
		operation: (snapshot: CredentialSnapshot | null) => CredentialOperation<T>,
	): Promise<T> {
		connectionCredentialKey(owner);
		let initialRevision: string | null | undefined;
		for (let attempt = 0; attempt < CONNECTION_WRITE_ATTEMPTS; attempt++) {
			const membership = await loadConnectionMembership(owner.userId);
			if (!membership) continue;
			if (membership.state === 'closed') throw new ConnectionOwnerClosedError();
			const row = await readConnectionCredential(owner);
			const snapshot = row ? this.parseCredential(row) : null;
			const revision = snapshot?.revision ?? null;
			if (initialRevision !== undefined && initialRevision !== revision) throw new ConnectionWriteConflictError();
			initialRevision = revision;
			const {result, write} = operation(snapshot);
			if (write === undefined) return result;
			const writes: Array<CredentialWrite> = [connectionMembershipPatch(membership, membership.entries), write];
			if (await executeConditional(UserConnections.conditionalBatch(writes))) return result;
		}
		throw new ConnectionWriteConflictError();
	}

	private parseCredential(row: UserConnectionStorageRow): CredentialSnapshot | null {
		if (!(row.credential_expires_at instanceof Date) || !Number.isFinite(row.credential_expires_at.getTime())) {
			throw new Error('Connection credentials have an invalid expiry');
		}
		if (row.credential_expires_at.getTime() <= Date.now()) return null;
		const revision = z.uuid().safeParse(row.revision);
		if (!revision.success || typeof row.credential_payload !== 'string') {
			throw new Error('Connection credentials have an invalid stored revision or payload');
		}
		let payload: unknown;
		try {
			payload = JSON.parse(row.credential_payload);
		} catch {
			throw new Error('Connection credentials contain invalid JSON');
		}
		const value = CredentialPayload.safeParse(payload);
		if (!value.success) throw new Error('Connection credentials have an invalid stored phase');
		return {revision: revision.data, expiresAt: row.credential_expires_at, value: value.data};
	}
}
