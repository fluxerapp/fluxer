// SPDX-License-Identifier: AGPL-3.0-or-later

const RELEASE_DELAY_MS = 2000;

export interface KeyedActionGuard {
	begin: (key: string) => boolean;
	release: (key: string) => void;
	scheduleRelease: (key: string) => void;
}

export function createKeyedActionGuard(): KeyedActionGuard {
	const inFlight = new Set<string>();
	const releaseTimers = new Map<string, NodeJS.Timeout>();
	const clearScheduledRelease = (key: string): void => {
		const releaseTimer = releaseTimers.get(key);
		if (!releaseTimer) return;
		clearTimeout(releaseTimer);
		releaseTimers.delete(key);
	};
	const release = (key: string): void => {
		clearScheduledRelease(key);
		inFlight.delete(key);
	};
	return {
		begin: (key: string): boolean => {
			if (inFlight.has(key)) return false;
			clearScheduledRelease(key);
			inFlight.add(key);
			return true;
		},
		release,
		scheduleRelease: (key: string): void => {
			clearScheduledRelease(key);
			const releaseTimer = setTimeout(() => {
				inFlight.delete(key);
				releaseTimers.delete(key);
			}, RELEASE_DELAY_MS);
			releaseTimers.set(key, releaseTimer);
		},
	};
}
