// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import SessionManager from '@app/features/platform/state/AuthSession';
import {INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {makeAutoObservable} from 'mobx';

class GuildVisitsRolloutSelector {
	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get enabled(): boolean {
		if (
			ExperimentAssignments.response === INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE ||
			ExperimentAssignments.ownerId !== SessionManager.userId
		) {
			return false;
		}
		return ExperimentAssignments.response.assignments.guild_visits?.enabled === true;
	}
}

const GuildVisitsRollout = new GuildVisitsRolloutSelector();

export default GuildVisitsRollout;
