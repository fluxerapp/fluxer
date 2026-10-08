// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {EventEmitter} from 'node:events';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {addLocalAppUploadProgressSubscriber, clearLocalAppUploadProgressSubscribers, emitLocalAppUploadFailure} =
	await import('./LocalAppProxyClient.ts');

function createSubscriber() {
	const contents = new EventEmitter();
	contents.destroyed = false;
	contents.sent = [];
	contents.isDestroyed = () => contents.destroyed;
	contents.send = (channel, payload) => contents.sent.push({channel, payload});
	return contents;
}

describe('the local app upload progress subscribers', () => {
	test('attaches one destroyed listener however often a document resubscribes', () => {
		clearLocalAppUploadProgressSubscribers();
		const sender = createSubscriber();

		for (let cycle = 0; cycle < 12; cycle += 1) {
			addLocalAppUploadProgressSubscriber(sender);
		}

		assert.equal(sender.listenerCount('destroyed'), 1);
	});

	test('a duplicate subscription delivers once and stops once the contents are destroyed', () => {
		clearLocalAppUploadProgressSubscribers();
		const sender = createSubscriber();

		addLocalAppUploadProgressSubscriber(sender);
		addLocalAppUploadProgressSubscriber(sender);
		emitLocalAppUploadFailure('delivered-after-resubscribe');

		assert.deepEqual(
			sender.sent.map((message) => message.payload.uploadId),
			['delivered-after-resubscribe'],
		);

		sender.destroyed = true;
		sender.emit('destroyed');
		sender.destroyed = false;
		emitLocalAppUploadFailure('after-destroy');

		assert.equal(sender.sent.length, 1);
	});
});
