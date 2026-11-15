// SPDX-License-Identifier: AGPL-3.0-or-later

export type EngineTimer = unknown;

export interface EngineClock {
	now(): number;
	setTimeout(callback: () => void, delayMs: number): EngineTimer;
	clearTimeout(timer: EngineTimer): void;
	random(): number;
}

export const systemClock: EngineClock = Object.freeze({
	now: () => Date.now(),
	setTimeout: (callback: () => void, delayMs: number) => {
		const timer = setTimeout(callback, Math.max(0, delayMs));
		timer.unref?.();
		return timer;
	},
	clearTimeout: (timer: EngineTimer) => clearTimeout(timer as ReturnType<typeof setTimeout>),
	random: () => Math.random(),
});
