// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {syncPasskeyTrait} from '@app/api/auth/AuthMfa';
import {visibleWebAuthnCredentials} from '@app/api/auth/services/PasskeyRelyingParty';
import type {UserID} from '@app/api/BrandedTypes';
import {createApiContext} from '@app/api/CreateApiContext';
import {fetchMany} from '@app/api/database/CassandraQueryExecution';
import {Logger} from '@app/api/Logger';
import {WebAuthnCredentialLookup} from '@app/api/Tables';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {WorkerQueueOverflowError} from '@app/api/worker/WorkerQueueOverflowError';
import type {WorkerService} from '@app/api/worker/WorkerService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const PASSKEY_TRAIT_BACKFILL_DONE_KEY = 'backfill:passkey_trait:done';
const CLAIM_KEY = 'backfill:passkey_trait:queued';
const CLAIM_TTL_SECONDS = ms('6 hours') / 1000;
const DEFAULT_PAGE_SIZE = 500;

interface CredentialOwnerRow {
	credential_id: string;
	user_id: UserID;
}

export interface PasskeyTraitBackfillSummary {
	users: number;
	set: number;
	cleared: number;
	failed: number;
}

const firstPageQuery = (limit: number) => WebAuthnCredentialLookup.select({limit});
const nextPageQuery = (limit: number) =>
	WebAuthnCredentialLookup.select({
		where: WebAuthnCredentialLookup.where.tokenGt('credential_id', 'last_credential_id'),
		limit,
	});

async function fetchCredentialOwners(limit: number, after: string | undefined): Promise<Array<CredentialOwnerRow>> {
	if (after === undefined) return fetchMany<CredentialOwnerRow>(firstPageQuery(limit).bind({}));
	return fetchMany<CredentialOwnerRow>(nextPageQuery(limit).bind({last_credential_id: after}));
}

export async function backfillPasskeyTraits(
	ctx: ApiContext,
	pageSize = DEFAULT_PAGE_SIZE,
): Promise<PasskeyTraitBackfillSummary> {
	const summary: PasskeyTraitBackfillSummary = {users: 0, set: 0, cleared: 0, failed: 0};
	const seen = new Set<UserID>();
	let cursor: string | undefined;
	while (true) {
		const page = await fetchCredentialOwners(pageSize, cursor);
		for (const row of page) {
			if (seen.has(row.user_id)) continue;
			seen.add(row.user_id);
			summary.users++;
			try {
				const credentials = await ctx.services.users.listWebAuthnCredentials(row.user_id);
				const present = visibleWebAuthnCredentials(credentials).length > 0;
				if (await syncPasskeyTrait(ctx, row.user_id, present)) {
					if (present) summary.set++;
					else summary.cleared++;
				}
			} catch (error) {
				summary.failed++;
				Logger.error({error, userId: row.user_id.toString()}, 'Failed to backfill the passkey trait');
			}
		}
		if (page.length < pageSize) break;
		cursor = page[page.length - 1]!.credential_id;
	}
	Logger.info(summary, 'Backfilled the passkey trait');
	return summary;
}

export async function queuePasskeyTraitBackfill(
	kvClient: Pick<IKVProvider, 'get' | 'setnx' | 'del'>,
	workerService: Pick<WorkerService, 'addJob'>,
): Promise<void> {
	if ((await kvClient.get(PASSKEY_TRAIT_BACKFILL_DONE_KEY)) !== null) return;
	if (!(await kvClient.setnx(CLAIM_KEY, '1', CLAIM_TTL_SECONDS))) return;
	try {
		await workerService.addJob('backfillPasskeyTrait', {});
	} catch (error) {
		await kvClient.del(CLAIM_KEY);
		if (!(error instanceof WorkerQueueOverflowError)) throw error;
		Logger.warn('Dropped the passkey trait backfill job, jobs stream is at its limit');
	}
}

const backfillPasskeyTraitTask: WorkerTaskHandler = async () => {
	const summary = await backfillPasskeyTraits(createApiContext());
	if (summary.failed > 0) throw new Error(`Passkey trait backfill failed for ${summary.failed} users`);
	await getWorkerDependencies().kvClient.set(PASSKEY_TRAIT_BACKFILL_DONE_KEY, new Date().toISOString());
};

export default backfillPasskeyTraitTask;
