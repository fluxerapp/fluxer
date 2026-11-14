// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {randomBase64Url, secretMatchesHash, sha256Hex} from '@app/api/utils/SecretHash';
import {InvalidOriginHandoffNonceError} from '@fluxer/errors/src/domains/auth/InvalidOriginHandoffNonceError';
import {UnknownOriginHandoffError} from '@fluxer/errors/src/domains/auth/UnknownOriginHandoffError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {seconds} from 'itty-time';

const ORIGIN_HANDOFF_KEY_PREFIX = 'origin_handoff:';
const ORIGIN_HANDOFF_ID_BYTES = 32;

interface OriginHandoffRecord {
	nonce_hash: string;
	payload: string;
	user_id: string;
	created_at: number;
}

function originHandoffKey(handoffId: string): string {
	return `${ORIGIN_HANDOFF_KEY_PREFIX}${sha256Hex(handoffId)}`;
}

export async function createOriginHandoff(
	cache: ICacheService,
	args: {userId: UserID; nonceHash: string; payload: string},
): Promise<string> {
	const handoffId = randomBase64Url(ORIGIN_HANDOFF_ID_BYTES);
	const record: OriginHandoffRecord = {
		nonce_hash: args.nonceHash,
		payload: args.payload,
		user_id: args.userId.toString(),
		created_at: Date.now(),
	};
	await cache.set(originHandoffKey(handoffId), record, seconds('2 minutes'));
	return handoffId;
}

export async function redeemOriginHandoff(
	cache: ICacheService,
	args: {handoffId: string; nonce: string},
): Promise<string> {
	const record = await cache.getAndDelete<OriginHandoffRecord>(originHandoffKey(args.handoffId));
	if (!record) {
		throw new UnknownOriginHandoffError();
	}
	if (!secretMatchesHash(args.nonce, record.nonce_hash)) {
		throw new InvalidOriginHandoffNonceError();
	}
	return record.payload;
}
