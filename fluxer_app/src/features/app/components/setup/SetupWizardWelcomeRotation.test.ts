// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createRandomWelcomeRotationState,
	createWelcomeRotationState,
	WELCOME_ROTATION,
	type WelcomeRotationState,
} from '@app/features/app/components/setup/SetupWizardWelcomeRotation';
import {describe, expect, it} from 'vitest';

function constantRandom(value: number): () => number {
	return () => value;
}

function welcomeTexts(state: WelcomeRotationState): ReadonlyArray<string> {
	return state.order.map((index) => WELCOME_ROTATION[index]?.text ?? '');
}

function firstWelcomeIndexForCode(code: string): number {
	return WELCOME_ROTATION.findIndex((entry) => entry.code === code);
}

describe('setup wizard welcome rotation', () => {
	it('starts with the selected locale welcome text', () => {
		const state = createWelcomeRotationState('sv-SE', constantRandom(0));
		const firstText = WELCOME_ROTATION[state.order[0] ?? 0]?.text;

		expect(firstText).toBe('Välkommen');
	});

	it('shows each rendered welcome text once per locale-seeded pass', () => {
		for (const code of ['en-US', 'en-GB', 'es-419', 'da', 'no']) {
			const texts = welcomeTexts(createWelcomeRotationState(code, constantRandom(0)));

			expect(new Set(texts).size).toBe(texts.length);
		}
	});

	it('shows each rendered welcome text once per random pass', () => {
		const previousIndex = firstWelcomeIndexForCode('en-US');
		const texts = welcomeTexts(createRandomWelcomeRotationState(previousIndex, constantRandom(0)));

		expect(new Set(texts).size).toBe(texts.length);
	});

	it('does not restart a random pass with the previous rendered welcome text', () => {
		const previousIndex = firstWelcomeIndexForCode('en-US');
		const state = createRandomWelcomeRotationState(previousIndex, constantRandom(0));
		const previousText = WELCOME_ROTATION[previousIndex]?.text;
		const nextText = WELCOME_ROTATION[state.order[0] ?? 0]?.text;

		expect(nextText).not.toBe(previousText);
	});
});
