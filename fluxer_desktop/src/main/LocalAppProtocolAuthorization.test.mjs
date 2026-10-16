// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {beforeEach, describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

const registry = installElectronStub();
const {DESKTOP_PROTOCOL_AUTHORIZATION_HEADER, DesktopLocalAppAuthorization, getDesktopLocalAppAuthorization} =
	await import('@electron/main/LocalAppProtocolAuthorization');

const LOCAL_APP_ORIGIN = 'fluxer-app://app';

let nextWebContentsId = 1;
let nextRoutingId = 1;

function createWebContents() {
	const id = nextWebContentsId;
	nextWebContentsId += 1;
	const contents = {
		id,
		destroyed: false,
		mainFrame: null,
		destroyedListeners: [],
		isDestroyed() {
			return this.destroyed;
		},
		once(event, listener) {
			if (event === 'destroyed') {
				this.destroyedListeners.push(listener);
			}
			return this;
		},
		destroy() {
			this.destroyed = true;
			for (const listener of this.destroyedListeners.splice(0)) {
				listener();
			}
		},
	};
	registry.contents.set(id, contents);
	return contents;
}

function createFrame(owner, {url, origin, parent = null}) {
	const routingId = nextRoutingId;
	nextRoutingId += 1;
	const frame = {
		processId: 1,
		routingId,
		url,
		origin,
		parent,
		detached: false,
		owner,
		isDestroyed: () => false,
	};
	frame.top = parent == null ? frame : parent.top;
	registry.frames.set(`1:${routingId}`, frame);
	return frame;
}

function createMainFrame(owner, options) {
	const frame = createFrame(owner, options);
	owner.mainFrame = frame;
	return frame;
}

function stampedValue(headers) {
	return headers[DESKTOP_PROTOCOL_AUTHORIZATION_HEADER] ?? null;
}

let authorization;
let owner;
let mainFrame;

beforeEach(() => {
	authorization = new DesktopLocalAppAuthorization();
	owner = createWebContents();
	mainFrame = createMainFrame(owner, {url: '', origin: 'null'});
});

describe('the boot navigation must be authorized, or the app 404s and never starts', () => {
	test('the very first main-frame navigation is stamped even though the frame URL is still empty', () => {
		authorization.authorize(owner);
		const headers = authorization.applyRequestHeaders(
			{url: 'fluxer-app://app/', resourceType: 'mainFrame', frame: mainFrame, webContentsId: owner.id},
			{Accept: '*/*'},
		);
		assert.notEqual(stampedValue(headers), null, 'the bootstrap navigation was not stamped');
		assert.equal(headers.Accept, '*/*', 'other request headers must survive the stamp');
	});

	test('an about:blank main frame is also a valid bootstrap frame', () => {
		authorization.authorize(owner);
		mainFrame.url = 'about:blank';
		const headers = authorization.applyRequestHeaders(
			{url: 'fluxer-app://app/channels/@me', resourceType: 'mainFrame', frame: mainFrame, webContentsId: owner.id},
			{},
		);
		assert.notEqual(stampedValue(headers), null);
	});

	test('the stamped value is exactly what the request handler accepts', async () => {
		authorization.authorize(owner);
		const headers = authorization.applyRequestHeaders(
			{url: 'fluxer-app://app/', resourceType: 'mainFrame', frame: mainFrame, webContentsId: owner.id},
			{},
		);
		const request = new Request('fluxer-app://app/', {headers: {...headers}});
		assert.equal(authorization.hasValidRequestAuthorization(request), true);
	});

	test('a main-frame navigation to a reserved proxy path is never stamped', () => {
		authorization.authorize(owner);
		for (const url of ['fluxer-app://app/api/k/v1/users/@me', 'fluxer-app://app/proxy/k?url=https://evil.example']) {
			const headers = authorization.applyRequestHeaders(
				{url, resourceType: 'mainFrame', frame: mainFrame, webContentsId: owner.id},
				{},
			);
			assert.equal(stampedValue(headers), null, url);
		}
	});

	test('a main-frame navigation from an unregistered WebContents is never stamped', () => {
		const headers = authorization.applyRequestHeaders(
			{url: 'fluxer-app://app/', resourceType: 'mainFrame', frame: mainFrame, webContentsId: owner.id},
			{},
		);
		assert.equal(stampedValue(headers), null);
	});

	test('a sub-frame cannot claim the bootstrap branch', () => {
		authorization.authorize(owner);
		const child = createFrame(owner, {url: '', origin: 'null', parent: mainFrame});
		const headers = authorization.applyRequestHeaders(
			{url: 'fluxer-app://app/', resourceType: 'mainFrame', frame: child, webContentsId: owner.id},
			{},
		);
		assert.equal(stampedValue(headers), null);
	});
});

describe('subresource requests from a live app document', () => {
	beforeEach(() => {
		authorization.authorize(owner);
		mainFrame.url = 'fluxer-app://app/channels/@me';
		mainFrame.origin = LOCAL_APP_ORIGIN;
	});

	test('XHR at the API route and asset loads are both stamped', () => {
		for (const [url, resourceType] of [
			['fluxer-app://app/api/k/v1/users/@me', 'xhr'],
			['fluxer-app://app/proxy/k?url=https%3A%2F%2Fmedia.fluxer.app%2Fx.png', 'image'],
			['fluxer-app://app/assets/main.js', 'script'],
		]) {
			const headers = authorization.applyRequestHeaders(
				{url, resourceType, frame: mainFrame, webContentsId: owner.id},
				{},
			);
			assert.notEqual(stampedValue(headers), null, url);
		}
	});

	test('an about:blank popout frame that inherited the app origin is stamped', () => {
		const popout = createFrame(owner, {url: 'about:blank', origin: LOCAL_APP_ORIGIN});
		const headers = authorization.applyRequestHeaders(
			{url: 'fluxer-app://app/api/k/v1/x', resourceType: 'xhr', frame: popout, webContentsId: owner.id},
			{},
		);
		assert.notEqual(stampedValue(headers), null);
	});

	test('an about:blank frame with a foreign origin is not stamped', () => {
		const popout = createFrame(owner, {url: 'about:blank', origin: 'https://web.fluxer.app'});
		const headers = authorization.applyRequestHeaders(
			{url: 'fluxer-app://app/api/k/v1/x', resourceType: 'xhr', frame: popout, webContentsId: owner.id},
			{},
		);
		assert.equal(stampedValue(headers), null);
	});
});

describe('the data: URL voice-debug window is a confused deputy and must be refused', () => {
	test('a data: document carrying the privileged preload is never stamped', () => {
		const sink = createWebContents();
		const sinkFrame = createMainFrame(sink, {url: 'data:text/html,x', origin: 'null'});
		authorization.authorize(owner);
		authorization.authorize(sink);
		const headers = authorization.applyRequestHeaders(
			{url: 'fluxer-app://app/api/k/v1/users/@me', resourceType: 'xhr', frame: sinkFrame, webContentsId: sink.id},
			{},
		);
		assert.equal(stampedValue(headers), null);
	});
});

describe('a renderer can never forge or exfiltrate the secret', () => {
	test('a caller-supplied header is stripped before the decision, in any casing', () => {
		authorization.authorize(owner);
		mainFrame.url = 'data:text/html,x';
		for (const name of [
			DESKTOP_PROTOCOL_AUTHORIZATION_HEADER,
			DESKTOP_PROTOCOL_AUTHORIZATION_HEADER.toLowerCase(),
			DESKTOP_PROTOCOL_AUTHORIZATION_HEADER.toUpperCase(),
		]) {
			const headers = authorization.applyRequestHeaders(
				{url: 'fluxer-app://app/api/k/v1/x', resourceType: 'xhr', frame: mainFrame, webContentsId: owner.id},
				{[name]: 'forged'},
			);
			assert.deepEqual(Object.values(headers), [], `${name} survived`);
		}
	});

	test('a forged header on an outbound https request is stripped, so the secret cannot be smuggled', () => {
		authorization.authorize(owner);
		mainFrame.url = 'fluxer-app://app/channels/@me';
		mainFrame.origin = LOCAL_APP_ORIGIN;
		const headers = authorization.applyRequestHeaders(
			{
				url: 'https://evil.example/collect',
				resourceType: 'xhr',
				frame: mainFrame,
				webContentsId: owner.id,
			},
			{'x-fluxer-desktop-protocol-authorization': 'forged', Accept: '*/*'},
		);
		assert.equal(stampedValue(headers), null);
		assert.equal(headers['x-fluxer-desktop-protocol-authorization'], undefined);
		assert.equal(headers.Accept, '*/*');
	});

	test('a wrong or missing header value fails the handler check', () => {
		assert.equal(
			authorization.hasValidRequestAuthorization(
				new Request('fluxer-app://app/', {headers: {[DESKTOP_PROTOCOL_AUTHORIZATION_HEADER]: 'wrong'}}),
			),
			false,
		);
		assert.equal(authorization.hasValidRequestAuthorization(new Request('fluxer-app://app/')), false);
	});

	test('two authorization instances never share a secret', () => {
		const other = new DesktopLocalAppAuthorization();
		other.authorize(owner);
		authorization.authorize(owner);
		const details = {url: 'fluxer-app://app/', resourceType: 'mainFrame', frame: mainFrame, webContentsId: owner.id};
		const first = stampedValue(authorization.applyRequestHeaders(details, {}));
		const second = stampedValue(other.applyRequestHeaders(details, {}));
		assert.notEqual(first, null);
		assert.notEqual(second, null);
		assert.notEqual(first, second);
	});
});

describe('registration hygiene', () => {
	test('authorizing something that is not a live WebContents throws', () => {
		const isInvalid = (error) => error.name === 'InvalidLocalAppWebContentsError';
		assert.throws(() => authorization.authorize(null), isInvalid);
		assert.throws(() => authorization.authorize({id: 999}), isInvalid);
		const destroyed = createWebContents();
		destroyed.destroyed = true;
		assert.throws(() => authorization.authorize(destroyed), isInvalid);
	});

	test('authorizing the same WebContents twice is idempotent', () => {
		authorization.authorize(owner);
		authorization.authorize(owner);
		assert.equal(owner.destroyedListeners.length, 1, 'the second authorize registered a second destroyed listener');
		const headers = authorization.applyRequestHeaders(
			{url: 'fluxer-app://app/', resourceType: 'mainFrame', frame: mainFrame, webContentsId: owner.id},
			{},
		);
		assert.notEqual(stampedValue(headers), null);
	});

	test('a recycled WebContents id never takes over the registration it collides with', () => {
		authorization.authorize(owner);
		const recycled = createWebContents();
		registry.contents.delete(recycled.id);
		recycled.id = owner.id;
		registry.contents.set(owner.id, recycled);
		assert.throws(
			() => authorization.authorize(recycled),
			(error) => error.name === 'InvalidLocalAppWebContentsError',
		);
	});

	test('a destroyed WebContents stops being authorized', () => {
		authorization.authorize(owner);
		owner.destroy();
		const headers = authorization.applyRequestHeaders(
			{url: 'fluxer-app://app/', resourceType: 'mainFrame', frame: mainFrame, webContentsId: owner.id},
			{},
		);
		assert.equal(stampedValue(headers), null);
	});

	test('malformed details never throw and never stamp', () => {
		authorization.authorize(owner);
		for (const details of [null, undefined, 'fluxer-app://app/', {}, {url: 42}, {url: 'fluxer-app://app/'}]) {
			assert.deepEqual(Object.values(authorization.applyRequestHeaders(details, {})), []);
		}
	});

	test('the module singleton is stable', () => {
		assert.equal(getDesktopLocalAppAuthorization(), getDesktopLocalAppAuthorization());
	});
});
