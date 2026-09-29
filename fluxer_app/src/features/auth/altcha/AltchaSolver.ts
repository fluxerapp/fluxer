// SPDX-License-Identifier: AGPL-3.0-or-later

import {solveChallengeWorkers} from 'altcha-lib';
import type {Challenge} from 'altcha-lib/types';

export type AltchaChallenge = Challenge;

const MAX_SOLVER_WORKERS = 8;
const SOLVE_TIMEOUT_MS = 60_000;

function createSolverWorker(): Worker {
	return new Worker(
		new URL(/* webpackChunkName: "altcha-solver.worker" */ './AltchaSolverWorker.ts', import.meta.url),
		{
			type: 'module',
		},
	);
}

export function readAltchaChallenge(body: unknown): AltchaChallenge | null {
	if (typeof body !== 'object' || body === null) return null;
	const {captcha_provider: provider, altcha_challenge: challenge} = body as Record<string, unknown>;
	if (provider !== 'altcha' || typeof challenge !== 'object' || challenge === null) return null;
	const {parameters, signature} = challenge as Record<string, unknown>;
	if (typeof parameters !== 'object' || parameters === null || typeof signature !== 'string') return null;
	return challenge as AltchaChallenge;
}

export async function solveAltchaChallenge(challenge: AltchaChallenge): Promise<string | null> {
	const solution = await solveChallengeWorkers({
		challenge,
		concurrency: Math.min(MAX_SOLVER_WORKERS, navigator.hardwareConcurrency || 2),
		createWorker: createSolverWorker,
		timeout: SOLVE_TIMEOUT_MS,
	});
	if (!solution) return null;
	return btoa(
		JSON.stringify({challenge: {parameters: challenge.parameters, signature: challenge.signature}, solution}),
	);
}
