// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import '../main/LocalAppTestSupport.test.mjs';

const {DESKTOP_FIRST_CONTENT_PAINTED_CHANNEL} = await import('../common/Constants.ts');
const {createFirstContentPaintSignal} = await import('./FirstContentPaint.ts');

function harness() {
	const sent = [];
	const frames = [];
	const signal = createFirstContentPaintSignal({send: (channel) => sent.push(channel)}, (callback) => {
		frames.push(callback);
	});
	const flushFrame = () => {
		const due = frames.splice(0);
		for (const callback of due) callback();
	};
	return {sent, frames, signal, flushFrame};
}

describe('first content paint signal', () => {
	test('reports only after two frames, so the content it announces has reached the screen', () => {
		const {sent, signal, flushFrame} = harness();

		signal();
		assert.deepEqual(sent, []);
		flushFrame();
		assert.deepEqual(sent, []);
		flushFrame();

		assert.deepEqual(sent, [DESKTOP_FIRST_CONTENT_PAINTED_CHANNEL]);
	});

	test('the inline shell and the React mount both calling it reports once per document', () => {
		const {sent, signal, frames, flushFrame} = harness();

		signal();
		signal();
		assert.equal(frames.length, 1);
		flushFrame();
		flushFrame();
		signal();
		flushFrame();
		flushFrame();

		assert.deepEqual(sent, [DESKTOP_FIRST_CONTENT_PAINTED_CHANNEL]);
	});
});
