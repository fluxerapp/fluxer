// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {MemeID, UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {createApiContext} from '@app/api/CreateApiContext';
import {fetchMany} from '@app/api/database/CassandraQueryExecution';
import {mapFavoriteMemeToResponse} from '@app/api/favorite_meme/FavoriteMemeModel';
import {FavoriteMemeRepository} from '@app/api/favorite_meme/FavoriteMemeRepository';
import {Logger} from '@app/api/Logger';
import {FavoriteMemesByMemeId} from '@app/api/Tables';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {WorkerQueueOverflowError} from '@app/api/worker/WorkerQueueOverflowError';
import type {WorkerService} from '@app/api/worker/WorkerService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const FAVORITE_MEME_NSFW_BACKFILL_DONE_KEY = 'backfill:favorite_meme_nsfw:done';
const CLAIM_KEY = 'backfill:favorite_meme_nsfw:queued';
const CLAIM_TTL_SECONDS = ms('6 hours') / 1000;
const DEFAULT_PAGE_SIZE = 500;

interface FavoriteMemeOwnerRow {
	meme_id: MemeID;
	user_id: UserID;
}

interface FavoriteMemeNsfwBackfillSummary {
	checked: number;
	flagged: number;
	failed: number;
}

const firstPageQuery = (limit: number) => FavoriteMemesByMemeId.select({limit});
const nextPageQuery = (limit: number) =>
	FavoriteMemesByMemeId.select({
		where: FavoriteMemesByMemeId.where.tokenGt('meme_id', 'last_meme_id'),
		limit,
	});

async function fetchMemeOwners(limit: number, after: MemeID | undefined): Promise<Array<FavoriteMemeOwnerRow>> {
	if (after === undefined) return fetchMany<FavoriteMemeOwnerRow>(firstPageQuery(limit).bind({}));
	return fetchMany<FavoriteMemeOwnerRow>(nextPageQuery(limit).bind({last_meme_id: after}));
}

function isProbeableMedia(contentType: string): boolean {
	return contentType.startsWith('image/') || contentType.startsWith('video/');
}

async function backfillFavoriteMemeNsfw(
	ctx: ApiContext,
	repository: FavoriteMemeRepository,
	pageSize = DEFAULT_PAGE_SIZE,
): Promise<FavoriteMemeNsfwBackfillSummary> {
	const summary: FavoriteMemeNsfwBackfillSummary = {checked: 0, flagged: 0, failed: 0};
	let cursor: MemeID | undefined;
	while (true) {
		const page = await fetchMemeOwners(pageSize, cursor);
		for (const row of page) {
			const meme = await repository.findById(row.user_id, row.meme_id);
			if (!meme || meme.nsfw !== null) continue;
			summary.checked++;
			try {
				const metadata = isProbeableMedia(meme.contentType)
					? await ctx.services.media.getMetadata({
							type: 's3',
							bucket: Config.s3.buckets.cdn,
							key: meme.storageKey,
							nsfw: 'allow',
						})
					: null;
				const nsfw = metadata?.nsfw === true;
				await repository.updateNsfw(meme.userId, meme.id, nsfw);
				if (nsfw) {
					summary.flagged++;
					const updated = await repository.findById(meme.userId, meme.id);
					if (updated) {
						await ctx.services.gateway.dispatchPresence({
							userId: meme.userId,
							event: 'FAVORITE_MEME_UPDATE',
							data: mapFavoriteMemeToResponse(updated),
						});
					}
				}
			} catch (error) {
				summary.failed++;
				Logger.warn({error, memeId: meme.id.toString()}, 'Failed to check a favorite meme for explicit media');
			}
		}
		if (page.length < pageSize) break;
		cursor = page[page.length - 1]!.meme_id;
	}
	Logger.info(summary, 'Backfilled the explicit media flag of favorite memes');
	return summary;
}

export async function queueFavoriteMemeNsfwBackfill(
	kvClient: Pick<IKVProvider, 'get' | 'setnx' | 'del'>,
	workerService: Pick<WorkerService, 'addJob'>,
): Promise<void> {
	if ((await kvClient.get(FAVORITE_MEME_NSFW_BACKFILL_DONE_KEY)) !== null) return;
	if (!(await kvClient.setnx(CLAIM_KEY, '1', CLAIM_TTL_SECONDS))) return;
	try {
		await workerService.addJob('backfillFavoriteMemeNsfw', {});
	} catch (error) {
		await kvClient.del(CLAIM_KEY);
		if (!(error instanceof WorkerQueueOverflowError)) throw error;
		Logger.warn('Dropped the favorite meme explicit flag backfill job, jobs stream is at its limit');
	}
}

const backfillFavoriteMemeNsfwTask: WorkerTaskHandler = async () => {
	await backfillFavoriteMemeNsfw(createApiContext(), new FavoriteMemeRepository());
	await getWorkerDependencies().kvClient.set(FAVORITE_MEME_NSFW_BACKFILL_DONE_KEY, new Date().toISOString());
};

export default backfillFavoriteMemeNsfwTask;
