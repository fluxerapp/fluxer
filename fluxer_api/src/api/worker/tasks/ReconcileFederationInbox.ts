// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SignedFederationEvent} from '@app/api/federation/FederationEvent';
import {getFederationConfig} from '@app/api/federation/FederationConfig';
import {
	getFederationInboxApplicationService,
	getFederationInboxRepository,
} from '@app/api/middleware/ServiceSingletons';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const RECONCILE_BATCH_SIZE = 100;

const reconcileFederationInbox: WorkerTaskHandler = async () => {
	const config = getFederationConfig();
	if (!config.enabled) return;
	const repository = getFederationInboxRepository();
	const applicationService = getFederationInboxApplicationService();
	for (const peer of config.peers) {
		const rows = await repository.listPendingForOrigin(peer.instanceId, RECONCILE_BATCH_SIZE);
		for (const row of rows) {
			const event: SignedFederationEvent = {
				v: 1,
				event_id: row.event_id,
				origin: row.origin,
				seq: Number(row.seq),
				type: row.type as SignedFederationEvent['type'],
				occurred_at: row.occurred_at.toISOString(),
				payload: JSON.parse(row.payload) as SignedFederationEvent['payload'],
				payload_hash: row.payload_hash,
				prev_hash: row.prev_hash,
				signature: row.signature,
			};
			await applicationService.apply(event);
		}
	}
};

export default reconcileFederationInbox;
