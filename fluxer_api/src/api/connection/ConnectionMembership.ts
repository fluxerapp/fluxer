import {randomUUID} from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';
import {executeConditional, fetchMany, fetchOne} from '@app/api/database/CassandraQueryExecution';
import {type ConditionalWriteEntry, Db} from '@app/api/database/CassandraTypes';
import {USER_CONNECTION_CREDENTIAL_TYPE, type UserConnectionStorageRow} from '@app/api/database/types/ConnectionTypes';
import {UserConnections} from '@app/api/Tables';
import {
	type ConnectionType,
	ConnectionTypes,
	MAX_CONNECTIONS_PER_USER,
} from '@fluxer/constants/src/ConnectionConstants';
import {z} from 'zod';

const MEMBERSHIP_TYPE = '_membership';
const MEMBERSHIP_ID = '_membership';
export const CONNECTION_WRITE_ATTEMPTS = 3;

export class ConnectionOwnerClosedError extends Error {
	constructor() {
		super('Connection ownership is closed for this account');
	}
}

export class ConnectionWriteConflictError extends Error {
	constructor() {
		super('Connection ownership changed during the operation');
	}
}

interface ConnectionMembershipEntry {
	connection_type: ConnectionType;
	identifier: string;
	connection_id: string;
}

interface ConnectionMembershipOwner {
	userId: UserID;
	revision: string;
}

interface ActiveConnectionMembership extends ConnectionMembershipOwner {
	state: 'active';
	entries: ReadonlyArray<ConnectionMembershipEntry>;
}

interface ClosedConnectionMembership extends ConnectionMembershipOwner {
	state: 'closed';
}

type ConnectionMembershipSnapshot = ActiveConnectionMembership | ClosedConnectionMembership;

const MembershipRevision = z.uuid();
const MembershipState = z.discriminatedUnion('state', [
	z.strictObject({state: z.literal('closed')}),
	z.strictObject({state: z.literal('active'), entries: z.unknown()}),
]);
const MembershipEntries = z
	.array(
		z.strictObject({
			connection_type: z.enum(ConnectionTypes),
			identifier: z
				.string()
				.min(1)
				.refine((identifier) => identifier === identifier.toLowerCase()),
			connection_id: z.string().min(1),
		}),
	)
	.max(MAX_CONNECTIONS_PER_USER);

const FETCH_MEMBERSHIP_CQL = UserConnections.selectCql({
	where: [
		UserConnections.where.eq('user_id'),
		UserConnections.where.eq('connection_type'),
		UserConnections.where.eq('connection_id'),
	],
	limit: 1,
});
const FETCH_BOOTSTRAP_CONNECTIONS_CQL = UserConnections.selectCql({
	where: UserConnections.where.eq('user_id'),
	limit: MAX_CONNECTIONS_PER_USER + 2,
});

function validateEntries(value: unknown): Array<ConnectionMembershipEntry> {
	const result = MembershipEntries.safeParse(value);
	if (!result.success) throw new Error('Connection membership catalogue contains invalid entries');
	const identities = new Set<string>();
	const connectionIds = new Set<string>();
	for (const entry of result.data) {
		const identity = JSON.stringify([entry.connection_type, entry.identifier]);
		if (identities.has(identity) || connectionIds.has(entry.connection_id)) {
			throw new Error('Connection membership catalogue contains duplicate connections');
		}
		identities.add(identity);
		connectionIds.add(entry.connection_id);
	}
	return result.data;
}

function parseMembership(row: UserConnectionStorageRow, userId: UserID): ConnectionMembershipSnapshot {
	if (row.user_id !== userId || row.connection_type !== MEMBERSHIP_TYPE || row.connection_id !== MEMBERSHIP_ID) {
		throw new Error('Connection membership catalogue has an invalid owner or key');
	}
	const revision = MembershipRevision.safeParse(row.revision);
	if (!revision.success) throw new Error('Connection membership catalogue has an invalid revision');
	if (typeof row.membership !== 'string') throw new Error('Connection membership catalogue has no entries');
	let value: unknown;
	try {
		value = JSON.parse(row.membership);
	} catch {
		throw new Error('Connection membership catalogue contains invalid JSON');
	}
	const state = MembershipState.safeParse(value);
	if (!state.success) throw new Error('Connection membership catalogue has an invalid state');
	if (state.data.state === 'closed') return {userId, revision: revision.data, state: 'closed'};
	return {userId, revision: revision.data, state: 'active', entries: validateEntries(state.data.entries)};
}

export function isConnectionMembershipRow(row: UserConnectionStorageRow): boolean {
	return row.connection_type === MEMBERSHIP_TYPE && row.connection_id === MEMBERSHIP_ID;
}

export function createPrivateConnectionRow(
	userId: UserID,
	connectionType: string,
	connectionId: string,
): UserConnectionStorageRow {
	return {
		user_id: userId,
		connection_type: connectionType,
		connection_id: connectionId,
		identifier: null,
		name: null,
		verified: null,
		visibility_flags: null,
		sort_order: null,
		verification_token: null,
		oauth_grant_id: null,
		verified_at: null,
		last_verified_at: null,
		created_at: null,
		revision: randomUUID(),
		version: null,
		membership: null,
		credential_payload: null,
		credential_expires_at: null,
	};
}

export async function loadConnectionMembership(userId: UserID): Promise<ConnectionMembershipSnapshot | null> {
	const stored = await fetchOne<UserConnectionStorageRow>(FETCH_MEMBERSHIP_CQL, {
		user_id: userId,
		connection_type: MEMBERSHIP_TYPE,
		connection_id: MEMBERSHIP_ID,
	});
	if (stored) return parseMembership(stored, userId);
	await fetchMany(FETCH_BOOTSTRAP_CONNECTIONS_CQL, {user_id: userId}, {consistency: 'serial'});
	const rows = await fetchMany<UserConnectionStorageRow>(
		FETCH_BOOTSTRAP_CONNECTIONS_CQL,
		{user_id: userId},
		{consistency: 'all'},
	);
	const catalogue = rows.find(isConnectionMembershipRow);
	if (catalogue) return parseMembership(catalogue, userId);
	if (rows.some((row) => row.connection_type === USER_CONNECTION_CREDENTIAL_TYPE)) {
		throw new Error('Connection credentials exist without their membership owner');
	}
	const entries = validateEntries(
		rows.map((row) => {
			if (row.user_id !== userId || typeof row.identifier !== 'string') {
				throw new Error('Connection membership bootstrap encountered an invalid connection');
			}
			return {
				connection_type: row.connection_type,
				identifier: row.identifier.toLowerCase(),
				connection_id: row.connection_id,
			};
		}),
	);
	const row: UserConnectionStorageRow = {
		...createPrivateConnectionRow(userId, MEMBERSHIP_TYPE, MEMBERSHIP_ID),
		membership: JSON.stringify({state: 'active', entries}),
	};
	const applied = await executeConditional(UserConnections.insertIfNotExists(row));
	return applied ? parseMembership(row, userId) : null;
}

export async function sealConnectionMembership(userId: UserID): Promise<void> {
	for (let attempt = 0; attempt < CONNECTION_WRITE_ATTEMPTS; attempt++) {
		const stored = await fetchOne<UserConnectionStorageRow>(FETCH_MEMBERSHIP_CQL, {
			user_id: userId,
			connection_type: MEMBERSHIP_TYPE,
			connection_id: MEMBERSHIP_ID,
		});
		if (!stored) {
			const applied = await executeConditional(
				UserConnections.insertIfNotExists({
					...createPrivateConnectionRow(userId, MEMBERSHIP_TYPE, MEMBERSHIP_ID),
					membership: JSON.stringify({state: 'closed'}),
				}),
			);
			if (applied) return;
			continue;
		}
		const membership = parseMembership(stored, userId);
		if (membership.state === 'closed') return;
		const applied = await executeConditional(
			UserConnections.conditionalPatchByPk(
				{user_id: userId, connection_type: MEMBERSHIP_TYPE, connection_id: MEMBERSHIP_ID},
				{revision: Db.set(randomUUID()), membership: Db.set(JSON.stringify({state: 'closed'}))},
				{revision: membership.revision},
			),
		);
		if (applied) return;
	}
	throw new ConnectionWriteConflictError();
}

export function connectionMembershipPatch(
	snapshot: ActiveConnectionMembership,
	entries: ReadonlyArray<ConnectionMembershipEntry>,
): ConditionalWriteEntry<UserConnectionStorageRow, 'user_id' | 'connection_type' | 'connection_id'> {
	if (!MembershipRevision.safeParse(snapshot.revision).success) {
		throw new Error('Connection membership snapshot has an invalid revision');
	}
	return {
		action: 'patch',
		pk: {user_id: snapshot.userId, connection_type: MEMBERSHIP_TYPE, connection_id: MEMBERSHIP_ID},
		patch: {
			revision: Db.set(randomUUID()),
			membership: Db.set(JSON.stringify({state: 'active', entries: validateEntries(entries)})),
		},
		expected: {revision: snapshot.revision},
	};
}
