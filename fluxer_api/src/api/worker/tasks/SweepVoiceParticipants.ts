// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {Config} from '@app/api/Config';
import {sweepVoiceParticipants, type VoiceParticipantSightings} from '@app/api/voice/VoiceParticipantSweep';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const LOCK_KEY = 'voice:participant_sweep:lock';
const SIGHTINGS_KEY = 'voice:participant_sweep:sightings';
const LOCK_TTL_SECONDS = 300;
const SIGHTINGS_TTL_SECONDS = 900;

function parseSightings(raw: string | null): VoiceParticipantSightings {
	if (raw === null) {
		return {};
	}
	try {
		const parsed: unknown = JSON.parse(raw);
		return typeof parsed === 'object' && parsed !== null ? (parsed as VoiceParticipantSightings) : {};
	} catch {
		return {};
	}
}

const sweepVoiceParticipantsTask: WorkerTaskHandler = async (_payload, helpers) => {
	const {kvClient, liveKitService, gatewayService} = getWorkerDependencies();
	const lockToken = randomUUID();
	if (!(await kvClient.acquireLock(LOCK_KEY, lockToken, LOCK_TTL_SECONDS))) {
		helpers.logger.debug('Voice participant sweep already running elsewhere');
		return;
	}
	try {
		const previous = parseSightings(await kvClient.get(SIGHTINGS_KEY));
		const result = await sweepVoiceParticipants(
			{
				liveKitService,
				gatewayService,
				logger: helpers.logger,
				now: () => Date.now(),
				dryRun: Config.voice.participantSweepDryRun,
			},
			previous,
		);
		await kvClient.setex(SIGHTINGS_KEY, SIGHTINGS_TTL_SECONDS, JSON.stringify(result.sightings));
		const {sightings: _sightings, ...summary} = result;
		if (summary.candidates > 0 || summary.skippedRooms > 0) {
			helpers.logger.info(summary, 'Voice participant sweep finished');
		}
	} finally {
		await kvClient.releaseLock(LOCK_KEY, lockToken);
	}
};

export default sweepVoiceParticipantsTask;
