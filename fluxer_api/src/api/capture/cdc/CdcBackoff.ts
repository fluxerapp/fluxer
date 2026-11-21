// SPDX-License-Identifier: AGPL-3.0-or-later

const FIRST_DELAY_MS = 1000;
const MAX_DELAY_MS = 60_000;

export class Stopped extends Error {
	override readonly name = 'Stopped';
}

export function delay(ms: number): Promise<void> {
	return new Promise((resolve) => {
		setTimeout(resolve, ms).unref();
	});
}

export async function pause(ms: number, stopping: () => boolean): Promise<void> {
	const until = Date.now() + ms;
	while (!stopping() && Date.now() < until) await delay(Math.min(1000, until - Date.now()));
}

export async function withBackoff<T>(
	run: () => Promise<T>,
	options: {
		stopping: () => boolean;
		onError: (error: unknown, delayMs: number) => void;
		fatal?: (error: unknown) => boolean;
	},
): Promise<T> {
	for (let delayMs = FIRST_DELAY_MS; ; delayMs = Math.min(delayMs * 2, MAX_DELAY_MS)) {
		if (options.stopping()) throw new Stopped('stopping');
		try {
			return await run();
		} catch (error) {
			if (options.fatal?.(error) === true) throw error;
			options.onError(error, delayMs);
			await pause(delayMs, options.stopping);
		}
	}
}
