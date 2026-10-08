// SPDX-License-Identifier: AGPL-3.0-or-later

import InstanceSnapshotStore from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import type {LimitContextInput} from '@app/features/app/utils/LimitContext';
import {LimitContext} from '@app/features/app/utils/LimitContext';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {resolveLimit, resolveLimits} from '@fluxer/limits/src/LimitResolver';
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

	resolveMultiple(
		keys: Array<LimitKey>,
		fallback: number,
		context?: LimitContextInput,
		instanceDomain?: string,
	): Record<string, number> {
		const snapshot = this.getSnapshotForInstance(instanceDomain);
		if (snapshot === null) {
			return Object.fromEntries(keys.map((key) => [key, fallback]));
		}
		const ctx = context ? LimitContext.build(context) : LimitContext.current();
		const {limits} = resolveLimits(snapshot, ctx);
		const result: Record<string, number> = {};
		for (const key of keys) {
			const resolved = limits[key];
			result[key] = Number.isFinite(resolved) && resolved >= 0 ? Math.floor(resolved) : fallback;
		}
		return result;
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
