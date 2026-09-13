// SPDX-License-Identifier: AGPL-3.0-or-later

import {BANNED_FILE_SHAS_REFRESH_CHANNEL} from '@app/api/constants/ContentModeration';
import {EXTERNAL_RESPONSE_LIMITS} from '@app/api/utils/ExternalResponseLimits';
import * as FetchUtils from '@app/api/utils/FetchUtils';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const MALWARE_BAZAAR_SHA256_URL = 'https://bazaar.abuse.ch/export/txt/sha256/recent/';
const SHA256_RE = /^[0-9a-fA-F]{64}$/;
const syncFileShaBlocklists: WorkerTaskHandler = async (_payload, helpers) => {
	helpers.logger.info('Starting file-SHA blocklist sync');
	await helpers.setContextLink('/file-sha-bans');
	const {adminRepository, kvClient} = getWorkerDependencies();
	let added = 0;
	try {
		const res = await fetch(MALWARE_BAZAAR_SHA256_URL, {signal: AbortSignal.timeout(120000)});
		if (!res.ok) {
			FetchUtils.discardResponseBody(res.body, res.status);
			throw new Error(`HTTP ${res.status}`);
		}
		const text = await FetchUtils.streamToStringWithLimit(res.body, {
			maxBytes: EXTERNAL_RESPONSE_LIMITS.fileBlocklistBytes,
			headers: res.headers,
			url: res.url,
			description: 'MalwareBazaar SHA-256 feed',
		});
		const freshHashes = new Set<string>();
		for (const line of text.split('\n')) {
			const l = line.trim().toLowerCase();
			if (!l || l.startsWith('#') || l.startsWith('"')) continue;
			if (SHA256_RE.test(l)) freshHashes.add(l);
		}
		helpers.logger.info({count: freshHashes.size}, 'Fetched MalwareBazaar SHA-256 hashes');
		const existing = await adminRepository.loadAllBannedFileShas();
		const existingSet = new Set(existing.map((r) => r.sha256_hex));
		for (const sha of freshHashes) {
			if (existingSet.has(sha)) continue;
			await adminRepository.banFileSha({
				sha256_hex: sha,
				category: 'malware_bazaar',
				severity: 2,
				content_type: null,
				source_url: MALWARE_BAZAAR_SHA256_URL,
				added_at: new Date(),
				added_by: null,
				notes: null,
			});
			added++;
		}
	} catch (err) {
		helpers.logger.warn({error: err}, 'Failed to fetch MalwareBazaar SHA-256 feed');
	}
	if (added > 0) {
		await kvClient.publish(BANNED_FILE_SHAS_REFRESH_CHANNEL, 'refresh');
	}
	helpers.logger.info({added}, 'File-SHA blocklist sync complete');
};

export default syncFileShaBlocklists;
