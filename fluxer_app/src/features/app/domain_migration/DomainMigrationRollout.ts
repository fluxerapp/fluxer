// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	markDomainMigrationEnrolled,
	readDomainMigrationEnrollment,
} from '@app/features/app/domain_migration/DomainMigrationCore';
import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import SessionManager from '@app/features/platform/state/AuthSession';
import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE,
	readDomainMigrationAssignment,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('DomainMigrationRollout');

class DomainMigrationRolloutSelector {
	deviceEnrolled = readDomainMigrationEnrollment(getProtectedLocalStorage());

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get assignmentReady(): boolean {
		return (
			ExperimentAssignments.response !== INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE &&
			ExperimentAssignments.ownerId === SessionManager.userId
		);
	}

	get assignmentEnabled(): boolean {
		if (!this.assignmentReady) {
			return false;
		}
		try {
			return readDomainMigrationAssignment(ExperimentAssignments.response).enabled;
		} catch (err) {
			logger.warn('Failed to resolve domain migration assignment:', err);
			return false;
		}
	}

	get enabled(): boolean {
		return this.assignmentEnabled || this.deviceEnrolled;
	}

	markDeviceEnrolled(): void {
		markDomainMigrationEnrolled(getProtectedLocalStorage(), Date.now());
		this.deviceEnrolled = true;
	}
}

const DomainMigrationRollout = new DomainMigrationRolloutSelector();

export default DomainMigrationRollout;
