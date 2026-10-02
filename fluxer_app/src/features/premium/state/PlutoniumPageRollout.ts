// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE,
	readPlutoniumPageAssignment,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('PlutoniumPageRollout');

class PlutoniumPageRolloutSelector {
	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get assignmentReady(): boolean {
		return (
			ExperimentAssignments.response !== INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE &&
			ExperimentAssignments.ownerId === SessionManager.userId
		);
	}

	get enabled(): boolean {
		if (!this.assignmentReady) {
			return false;
		}
		try {
			return readPlutoniumPageAssignment(ExperimentAssignments.response).enabled;
		} catch (err) {
			logger.warn('Failed to resolve plutonium page assignment:', err);
			return false;
		}
	}
}

export const PlutoniumPageRollout = new PlutoniumPageRolloutSelector();

export default PlutoniumPageRollout;
