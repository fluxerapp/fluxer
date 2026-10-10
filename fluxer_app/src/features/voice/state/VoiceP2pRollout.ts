// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	INERT_VOICE_P2P_ASSIGNMENT,
	type VoiceP2pAssignmentResponse,
} from '@fluxer/schema/src/domains/admin/VoiceP2pSchemas';
import {
	INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE,
	readVoiceP2pAssignment,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('VoiceP2pRollout');

class VoiceP2pRolloutSelector {
	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get assignmentReady(): boolean {
		return (
			ExperimentAssignments.response !== INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE &&
			ExperimentAssignments.ownerId === SessionManager.userId
		);
	}

	get assignment(): VoiceP2pAssignmentResponse {
		if (!this.assignmentReady) {
			return INERT_VOICE_P2P_ASSIGNMENT;
		}
		try {
			return readVoiceP2pAssignment(ExperimentAssignments.response);
		} catch (err) {
			logger.warn('Failed to resolve voice P2P assignment:', err);
			return INERT_VOICE_P2P_ASSIGNMENT;
		}
	}

	get enabled(): boolean {
		return this.assignment.enabled;
	}

	get maxParticipants(): number {
		return this.assignment.max_participants;
	}
}

const VoiceP2pRollout = new VoiceP2pRolloutSelector();

export default VoiceP2pRollout;
