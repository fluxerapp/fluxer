// SPDX-License-Identifier: AGPL-3.0-or-later

import InstanceSnapshotStore from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import type {LimitContextInput} from '@app/features/app/utils/LimitContext';
import {LimitContext} from '@app/features/app/utils/LimitContext';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {resolveLimit} from '@fluxer/limits/src/LimitResolver';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';

export interface LimitResolveOptions {
	key: LimitKey;
	fallback: number;
	context?: LimitContextInput;
	instanceDomain?: string;
}

class LimitResolverClass {
	resolve(options: LimitResolveOptions): number {
		const {key, fallback, context, instanceDomain} = options;
		const snapshot = this.getSnapshotForInstance(instanceDomain);
		if (snapshot === null) {
			return fallback;
		}
		const ctx = context ? LimitContext.build(context) : LimitContext.current();
		const resolved = resolveLimit(snapshot, ctx, key);
		if (!Number.isFinite(resolved) || resolved < 0) {
			return fallback;
		}
		return Math.floor(resolved);
	}

	private getSnapshotForInstance(instanceDomain?: string): LimitConfigSnapshot | null {
		if (instanceDomain !== undefined) {
			return InstanceSnapshotStore.getLimitsForInstance(instanceDomain);
		}
		return RuntimeConfig.getSnapshotOrNull()?.limits ?? null;
	}

	resolvePremium(key: LimitKey, fallback: number): number {
		return this.resolveStock(key, fallback);
	}

	resolveFree(key: LimitKey, fallback: number): number {
		return this.resolveRestricted(key, fallback);
	}

	resolveStock(key: LimitKey, fallback: number): number {
		return this.resolve({
			key,
			fallback,
			context: LimitContext.stock(),
		});
	}

	resolveRestricted(key: LimitKey, fallback: number): number {
		return this.resolve({
			key,
			fallback,
			context: LimitContext.restricted(),
		});
	}
}

export const LimitResolver = new LimitResolverClass();
