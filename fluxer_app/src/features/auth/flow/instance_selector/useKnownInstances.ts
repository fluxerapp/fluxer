// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type InstanceInfo,
	loadKnownInstances,
} from '@app/features/auth/flow/instance_selector/InstanceDirectoryStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {useEffect, useState} from 'react';

const logger = new Logger('KnownInstances');

const EMPTY_KNOWN_INSTANCES: ReadonlyArray<InstanceInfo> = Object.freeze([]);

export function useKnownInstances(enabled = true): ReadonlyArray<InstanceInfo> {
	const [knownInstances, setKnownInstances] = useState<ReadonlyArray<InstanceInfo>>(EMPTY_KNOWN_INSTANCES);
	useEffect(() => {
		if (!enabled) {
			return undefined;
		}
		let cancelled = false;
		loadKnownInstances()
			.then((instances) => {
				if (!cancelled) {
					setKnownInstances(instances);
				}
			})
			.catch((error: unknown) => {
				logger.warn('Failed to load the known instance directory', error);
			});
		return () => {
			cancelled = true;
		};
	}, [enabled]);
	return enabled ? knownInstances : EMPTY_KNOWN_INSTANCES;
}
