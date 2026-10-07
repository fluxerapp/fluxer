// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {beforeEach, describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {
	applyPendingModuleUpdate,
	discardPendingModuleUpdate,
	getPendingModuleUpdate,
	observePendingModuleUpdate,
	offerPendingModuleUpdate,
} = await import('@electron/main/ModuleUpdateGate');

function entry(modules, calls) {
	return {
		update: {modules},
		apply: () => calls.push(`apply:${modules.join(',')}`),
		discard: () => calls.push(`discard:${modules.join(',')}`),
	};
}

describe('ModuleUpdateGate', () => {
	beforeEach(() => {
		discardPendingModuleUpdate();
	});

	test('there is no pending update until one is offered', () => {
		assert.equal(getPendingModuleUpdate(), null);
		assert.equal(applyPendingModuleUpdate(), false);
	});

	test('an offered update is readable and applies exactly once', () => {
		const calls = [];
		offerPendingModuleUpdate(entry(['fluxer_renderer'], calls));
		assert.deepEqual(getPendingModuleUpdate(), {modules: ['fluxer_renderer']});
		assert.equal(applyPendingModuleUpdate(), true);
		assert.equal(applyPendingModuleUpdate(), false);
		assert.deepEqual(calls, ['apply:fluxer_renderer']);
		assert.equal(getPendingModuleUpdate(), null);
	});

	test('applying never discards the update it applied', () => {
		const calls = [];
		offerPendingModuleUpdate(entry(['fluxer_twemoji'], calls));
		applyPendingModuleUpdate();
		assert.deepEqual(calls, ['apply:fluxer_twemoji']);
	});

	test('discarding releases the pending update without applying it', () => {
		const calls = [];
		offerPendingModuleUpdate(entry(['fluxer_grammars'], calls));
		discardPendingModuleUpdate();
		assert.equal(getPendingModuleUpdate(), null);
		assert.deepEqual(calls, ['discard:fluxer_grammars']);
	});

	test('a superseding offer discards the update it replaced', () => {
		const calls = [];
		offerPendingModuleUpdate(entry(['first'], calls));
		offerPendingModuleUpdate(entry(['second'], calls));
		assert.deepEqual(calls, ['discard:first']);
		assert.deepEqual(getPendingModuleUpdate(), {modules: ['second']});
		applyPendingModuleUpdate();
		assert.deepEqual(calls, ['discard:first', 'apply:second']);
	});

	test('observers receive the current state on subscribe and on every change', () => {
		const seen = [];
		const stop = observePendingModuleUpdate((pending) => {
			seen.push(pending == null ? null : pending.modules.join(','));
		});
		offerPendingModuleUpdate(entry(['fluxer_renderer'], []));
		applyPendingModuleUpdate();
		stop();
		offerPendingModuleUpdate(entry(['ignored'], []));
		assert.deepEqual(seen, [null, 'fluxer_renderer', null]);
	});

	test('a throwing observer does not stop the others', () => {
		const seen = [];
		observePendingModuleUpdate(() => {
			throw new Error('this observer throws');
		});
		observePendingModuleUpdate((pending) => {
			seen.push(pending == null ? null : pending.modules.join(','));
		});
		offerPendingModuleUpdate(entry(['fluxer_renderer'], []));
		assert.deepEqual(seen, [null, 'fluxer_renderer']);
	});
});
