// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {type InstanceHTTPTarget, instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';

export interface AuthRequestTarget {
	readonly http: InstanceHTTPTarget;
}

export function authRequestTargetFromSnapshot(runtimeSnapshot: RuntimeConfigSnapshot): AuthRequestTarget {
	return {
		http: instanceTargetFromSnapshot(runtimeSnapshot),
	};
}
